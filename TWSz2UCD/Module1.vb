Imports System
' console i/o
Imports System.IO
' web requests
Imports System.Net
' Encoding
Imports System.Text
' certificate stuff
Imports System.Security.Cryptography
Imports System.Security.Cryptography.X509Certificates
' RemoteCertificateValidation
Imports System.Net.Security
' Registry stuff
Imports Microsoft.Win32
' Newtonsoft JSON add-in. Added using Tools->NuGet Package Manager->NuGet Console, then @ PM> prompt: Install-Package Newtonsoft.Json
Imports Newtonsoft.Json
' FileIO TextFieldParser:
Imports Microsoft.VisualBasic.FileIO

Module Module1
    Const __ucdCsrfToken_Name As String = "UCD_CSRF_TOKEN"
    Const __ucdSessionId_Name As String = "UCD_SESSION_ID"
    Const __partJSessionId_Name As String = "JSESSIONID_"
    Private __baseurl As String = ""
    Private __ucdSessionId As String = ""
    Private __ucdCsrfToken As String = ""
    Private __jsessionId As String = ""
    Private __jsessionId_Name As String = ""
    Private __debug As Boolean = True
    Private zosfiles As String = ".\downloads"

    ' The following are built from the TWSz export file
    ' the name of the AD covered by the export file - retrieved from the ADSTART ADID field
    Private adname As String = ""
    ' list of job names retrieved from ADOP JOBN field
    Private joblist As List(Of String) = New List(Of String)
    ' list of operation number to job name mappings retrieved from ADOP OPNO and JOBN fields
    Private opslist As Dictionary(Of String, String) = New Dictionary(Of String, String)
    ' maximum "okay" job condition code
    Private maxcc As Dictionary(Of String, Integer) = New Dictionary(Of String, Integer)
    ' job name to job name dependency list
    Private deplist As List(Of String) = New List(Of String)
    ' flag whether or not a job has a successor. If not it needs to have "Finish" as a follow on
    Private hasdep As Dictionary(Of String, Boolean) = New Dictionary(Of String, Boolean)
    ' job name to JCL text mapping - one entry for each job containing full JCL text
    Private jcllist As Dictionary(Of String, String) = New Dictionary(Of String, String)

    ' return a count of the number of occurences of a particular character (needle) in a string (haystack)
    Function GetCharCount(haystack As String, needle As Char)
        Dim count As Integer = 0
        For Each ch In haystack
            If ch = needle Then
                count += 1
            End If
        Next
        GetCharCount = count
    End Function

    ' return an array of strings having split a stanza into its components:
    Function SplitStanza(stanza As String) As String()
        Dim rv As List(Of String) = New List(Of String)
        Dim ch As Char() = stanza.ToCharArray()
        Dim field As String = ""
        Dim state As String = "START"
        Dim inquotes As Boolean = False
        For Each c In ch
            If state = "START" Then
                If c = " " Or c = vbTab Then
                Else
                    field &= c
                    state = "STNAME"
                End If
            ElseIf state = "STNAME" Then
                If c = " " Or c = vbTab Then
                    rv.Add(field)
                    field = ""
                    state = "STFIELD"
                Else
                    field &= c
                End If
            ElseIf state = "STFIELD" Then
                If c = " " Or c = vbTab Then
                Else
                    field &= c
                    state = "FIELD"
                End If
            ElseIf state = "FIELD" Then
                If c = " " Or c = vbTab Then
                    If inquotes Then
                        field &= c
                    Else ' don't do anything - lose the space
                    End If
                ElseIf c = "'" Then
                    If inquotes Then
                        field &= c
                        inquotes = False
                    Else
                        field &= c
                        inquotes = True
                    End If
                ElseIf c = ")" Then
                    If inquotes Then
                        field &= c
                    Else
                        field &= c
                        rv.Add(field)
                        field = ""
                        state = "STFIELD"
                    End If
                Else
                    field &= c
                End If
            End If
        Next
        SplitStanza = rv.ToArray()
    End Function

    ' keep a track of the current operation / job name
    Private currentOperation As String = ""

    ' process a complete TWSz Export stanza
    Function ProcessStanza(stanza As String, text As String) As Boolean
        Dim someError As Boolean = False
        Dim inquotes As Boolean = False
        Dim fieldset As String = ""
        Dim fieldvalue As String = ""
        Dim fieldname As String = ""
        Dim elements As Dictionary(Of String, String) = New Dictionary(Of String, String)
        ' split the stanza into words - this is made slightly tricky by space padding of fields
        Dim linewords As String() = SplitStanza(text)
        'Dim linewords As String() = text.Split(" ")
        For Each field In linewords
            ' does the word contain a quote? We might need to reassemble the value field
            If field.Contains("'") Then
                Dim nquotes As Integer = GetCharCount(field, "'"c)
                ' an odd number of quotes? Incomplete quoted value
                If nquotes Mod 2 = 1 Then
                    If inquotes Then
                        fieldset += " " + field
                        inquotes = False
                    Else
                        fieldset = field
                        inquotes = True
                    End If
                Else ' i.e. field contains an even number of quotes
                    If inquotes Then
                        fieldset += " " + field
                    Else
                        fieldset = field
                    End If
                End If
            Else ' i.e. field doesn't contain a quote
                If inquotes Then
                    fieldset += " " + field
                Else
                    fieldset = field
                End If
            End If
            If Not inquotes Then
                If fieldset.Contains("(") Then
                    Dim fieldparts As String() = fieldset.Split(New Char() {"("c, ")"c})
                    elements.Add(fieldparts(0), fieldparts(1))
                End If
            End If
        Next
        elements.Add("##TYPE##", stanza)
        If stanza = "ADSTART" Then
            adname = elements("ADID")
            Console.WriteLine(String.Format("Working with AD name {0}", adname))
        ElseIf stanza = "ADOP" Then
            If elements("WSID") = "DMMY" Then
                Console.WriteLine(String.Format("Skipping operation {0} ({1}) with workstation = DMMY", elements("OPNO"), elements("JOBN")))
                currentOperation = ""
            Else
                currentOperation = elements("JOBN")
                If joblist.Count = 0 Then
                    deplist.Add(currentOperation)
                End If
                joblist.Add(currentOperation)
                opslist.Add(elements("OPNO"), currentOperation)
                hasdep.Add(currentOperation, False)
                ' set maxcc = 0, or the integer value parsed in HIGHRC
                Dim hcc As Integer = 0
                Try
                    Integer.TryParse(elements("HIGHRC"), hcc)
                Catch ex As Exception
                    hcc = 0
                End Try
                maxcc.Add(currentOperation, hcc)
                ' read supporting JCL from downloads:
                Dim sr As StreamReader = Nothing
                Dim fname As String = String.Format("{0}\{1}", zosfiles, currentOperation)
                Try
                    sr = New StreamReader(fname)
                Catch ex As System.IO.FileNotFoundException
                    Console.WriteLine("ERROR - unable to locate JCL file for operation {0}. This should be in {1}", currentOperation, fname)
                    Console.WriteLine("Please check that all of the contents of the STAGING PDS were downloaded to {0}", zosfiles)
                    Console.WriteLine("Exit RC=8")
                    Environment.ExitCode = 8
                    someError = True
                End Try
                Dim file As String = sr.ReadToEnd()
                ' change all cr/lf pairs to "\r\n"
                file = file.Replace(vbCrLf, "\r\n")
                ' change all "/" to "\/"
                file = file.Replace("/", "\/")
                ' change all '"' to '\"'
                file = file.Replace("""", "\""")
                jcllist.Add(currentOperation, file)
            End If
        ElseIf stanza = "ADDEP" Then
            Dim depad As String = ""
            Try
                depad = elements("PREADID")
            Catch ex As Exception
                depad = adname
            End Try
            Dim depjobno As String = elements("PREOPNO")
            Dim depjob As String = ""
            Try
                depjob = opslist(depjobno)
            Catch ex As Exception
                depjob = ""
            End Try
            Dim jobnotskipped As Boolean = True
            Dim testjobexists As Boolean = False
            Try
                testjobexists = hasdep.Item(depjob)
            Catch ex As Exception
                jobnotskipped = False
            End Try
            If depad = adname And jobnotskipped And currentOperation <> "" Then
                deplist.Add(String.Format("{0}->{1}", depjob, currentOperation))
                hasdep.Item(depjob) = True
            Else
                Console.WriteLine(String.Format("Discarding dependency on AD {0} job {1}", depad, depjobno))
            End If
        End If
        ProcessStanza = someError
    End Function

    Sub Main()
        Dim args As String() = Environment.GetCommandLineArgs()
        Dim ia As Integer = 0
        Dim configFile = ""
        Dim parmid = ""
        If args.Count = 1 Then
            Console.WriteLine("Missing the configuration file path and name. Please use:")
            Console.WriteLine()
            Console.WriteLine("   {0} configFile | --help | -h | /?", args(0))
            Console.WriteLine()
            Console.WriteLine("Exit RC=8")
            Environment.ExitCode = 8
            Exit Sub
        End If
        For Each arg In args
            If ia > 0 Then
                parmid = ""
                If ia = 1 Then
                    configFile = arg
                    parmid = "  <== configuration file"
                Else
                    parmid = "  ??? unknown parameter ???"
                End If
                'Console.WriteLine("   {0}: {1}{2}", ia, arg, parmid)
            End If
            ia += 1
        Next
        If parmid.Substring(0, 5) = "  ???" Then
            Console.WriteLine("Please remove extraneous parameters and rerun")
            Console.WriteLine("Exit RC=8")
            Environment.ExitCode = 8
            Exit Sub
        End If
        ' handle help request
        If configFile = "--help" Or configFile = "/?" Or configFile = "-h" Then
            Console.WriteLine("TWSz2UCD - Help")
            Console.Write("This program has been provided to convert an OPC AD into an UCD component process. " +
                          "It does this by reading an OPC AD extract file generated on z/OS, and then connects " +
                          "to UCD to generate the required component elements. As well as the AD extract file " +
                          "we import the JCL for each of the operations in the AD. This is all provided and " +
                          "staged by the Rexx utility OPCJOBLB.")
            Console.WriteLine()
            Console.WriteLine()
            Console.Write("The program takes the filename of a configuration file as its only parameter. The " +
                          "config file is treats blank lines, and lines with a '#' in column 1 as comments. " +
                          "The rest of the lines should conform to:")
            Console.WriteLine()
            Console.WriteLine()
            Console.WriteLine("   key = value")
            Console.WriteLine()
            Console.WriteLine("Where:")
            Console.WriteLine("  key = config.Downloads      is the path to the AD export and JCL downloads")
            Console.WriteLine("      = config.UCD            is the URL of the UCD server - e.g. https://ucd.intranet.group")
            Console.WriteLine("      = config.UCDuser        is the UCD userid to connect and perform actions with")
            Console.WriteLine("      = config.UCDpassword    is the password for config.UCFuser")
            Console.WriteLine("      = config.UCDapplication is the UCD application to connect the new component to")
            Console.WriteLine("      = config.UCDenvironment is the UCD application environment to associate the component with")
            Console.WriteLine()
            Console.WriteLine("James Gill - June 2018")
            Environment.ExitCode = 4
            Exit Sub
        End If
        ' Set default values for supplied parameters
        ' nb - zosfile is global as it's used to process AD export file stanzas
        Dim baseurl As String = "https://ucd.intranet.group"
        Dim userid As String = "twszadmin"
        Dim password As String = ""
        Dim applicationName As String = "TWSz"
        Dim applicationEnvironment As String = "Development"
        ' read the config file, if a name was provided:
        Dim line As String = ""
        If configFile <> "" Then
            Dim crs As StreamReader
            Try
                crs = New StreamReader(configFile)
            Catch ex As System.IO.FileNotFoundException
                Console.WriteLine("ERROR - configuration file ""{0}"" not found", configFile)
                Console.WriteLine("Ending RC=8")
                Environment.ExitCode = 8
                Exit Sub
            End Try
            Dim lineno As Integer = 0
            Do Until crs.EndOfStream()
                line = crs.ReadLine().TrimEnd()
                lineno += 1
                If line.Length = 0 Then
                    ' it's a blank line
                ElseIf line.Substring(0, 1) = "#" Then
                    ' it's a comment
                Else
                    Dim lineparts As String() = line.Split("=")
                    If lineparts.Count <> 2 Then
                        Console.WriteLine("ERROR - line {0} is invalid. Each line should be of the form:", lineno)
                        Console.WriteLine("        key = value")
                        Console.WriteLine("Exit RC=8")
                        Environment.ExitCode = 8
                        Exit Sub
                    End If
                    Dim parmkey As String = lineparts(0).Trim()
                    Dim parmval As String = lineparts(1).Trim()
                    Select Case parmkey
                        Case "config.downloads"
                            zosfiles = parmval
                        Case "config.UCD"
                            baseurl = parmval
                        Case "config.UCDuser"
                            userid = parmval
                        Case "config.UCDpassword"
                            password = parmval
                        Case "config.UCDapplication"
                            applicationName = parmval
                        Case "config.UCDenvironment"
                            applicationEnvironment = parmval
                        Case Else
                            Console.WriteLine("Error - unknown key value ({0}) in config file", parmkey)
                            Console.WriteLine("Exit RC=8")
                            Environment.ExitCode = 8
                            Exit Sub
                    End Select
                End If
            Loop
        End If
        Dim inputFile As String = String.Format("{0}\##01", zosfiles)
        If password = "" Then
            Console.WriteLine("ERROR - no password was supplied for the UCD userid {0}", userid)
            Console.WriteLine("Exit RC=8")
            Environment.ExitCode = 8
            Exit Sub
        End If
        ' remove trailing "/" if supplied with config.UCD
        If baseurl.Length > 0 Then
            baseurl = baseurl.TrimEnd(New Char() {"/"c})
        End If
        If baseurl = "" Then
            Console.WriteLine("ERROR - no UCD server URL was supplied")
            Console.WriteLine("Exit RC=8")
            Environment.ExitCode = 8
            Exit Sub
        End If
        ' Report config
        Console.WriteLine("The following configuration is in use for this run:")
        Console.WriteLine("  + z/OS files path      = {0}", zosfiles)
        Console.WriteLine("  + AD export file       = {0}", inputFile)
        Console.WriteLine("  + UCD network address  = {0}", baseurl)
        Console.WriteLine("  + UCD userid to use    = {0}", userid)
        Console.WriteLine("  + UCD application name = {0}", applicationName)
        Console.WriteLine("  + UCD environment name = {0}", applicationEnvironment)
        Console.WriteLine()
        Console.WriteLine(String.Format("Reading TWSz AD Export file {0}", inputFile))
        Dim rs As StreamReader
        Try
            rs = New StreamReader(inputFile)
        Catch ex As System.IO.FileNotFoundException
            Console.WriteLine("ERROR - AD export file ""{0}"" not found", inputFile)
            Console.WriteLine("Ending RC=8")
            Environment.ExitCode = 8
            Exit Sub
        End Try
        Dim thisStanza As String = ""
        Dim thisStanzaText As String = ""
        Do Until rs.EndOfStream()
            line = rs.ReadLine().Trim()
            Dim lineWords As String() = line.Split(" ")
            If lineWords(0).Contains("(") Then
                ' more of the current stanza
                thisStanzaText += " " + line
            Else
                If thisStanza.Length > 0 Then
                    If ProcessStanza(thisStanza, thisStanzaText) Then
                        Exit Sub
                    End If
                End If
                thisStanza = lineWords(0)
                thisStanzaText = line
            End If
        Loop
        If thisStanza.Length > 0 Then
            If ProcessStanza(thisStanza, thisStanzaText) Then
                Exit Sub
            End If
        End If
        rs.Close()

        ' verify input
        ' got AD name?
        If adname.Length = 0 Then
            Console.WriteLine("ERROR - Unable to locate ADID in ADSTART stanza. RC=8")
            Environment.ExitCode = 8
            Exit Sub
        End If
        ' got at least one job?
        If joblist.Count < 1 Then
            Console.WriteLine("ERROR - Unable to locate jobs in ADOP stanzas. RC=8")
            Environment.ExitCode = 8
            Exit Sub
        End If

        ' report jobs and dependencies
        ' reported by the stanza processor : Console.WriteLine(String.Format("AD name {0}:", adname))
        Console.WriteLine("  Jobs:")
        For Each job In joblist
            If hasdep(job) Then
            Else
                deplist.Add(String.Format("{0}->Finish", job))
                hasdep.Item(job) = True
            End If
            Console.WriteLine(String.Format("    {0}", job))
        Next
        Console.WriteLine("  Dependencies:")
        For Each dep In deplist
            Console.WriteLine(String.Format("    {0}", dep))
        Next

        ' Handle inbound server certificate validation issues. This allows us to accept self-signed and broken chain server certificate
        ServicePointManager.ServerCertificateValidationCallback = New RemoteCertificateValidationCallback(AddressOf ValidateServerCertificate)

        If Not Logon(baseurl, userid, password) Then
            Exit Sub
        End If

        Console.WriteLine("Creating component")
        '
        ' Now we've got the Session Key, we can make the component create request
        Dim apiType As String = "component"
        Dim apiFunc As String = "create"
        Dim mycompname As String = String.Format("{0}-Comp", adname)
        Dim data As String = "{""name"":""" & mycompname & """," &
                             """description"":""" & mycompname & " Component""," &
                             """importAutomatically"":false," &
                             """defaultVersionType"":""INCREMENTAL""," &
                             """sourceConfigPlugin"":""""," &
                             """componentType"":""ZOS""," &
                             """useVfs"":false," &
                             "}"
        Dim crComp As String = HttpPUT("component/create", data)
        If crComp.StartsWith("OK") Then
            crComp = crComp.Substring(3)
        Else
            Console.WriteLine("Error encountered creating component: " + crComp)
            Exit Sub
        End If
        Console.WriteLine("Success")
        Dim component = JsonConvert.DeserializeObject(crComp)
        Console.WriteLine(String.Format("Generated component id = {0}", component("id")))
        Dim mycompid = component("id")
        ' retrieve the created component details so that we can get the roleid
        Dim compinfo As String = HttpGET(String.Format("component/info?component={0}", mycompid))
        If compinfo.StartsWith("OK") Then
            compinfo = compinfo.Substring(3)
        Else
            Console.WriteLine("Error encountered retrieving component information: " + compinfo)
            Exit Sub
        End If
        Dim info = JsonConvert.DeserializeObject(compinfo)
        Dim mycompRoleId = ""
        mycompRoleId = info("resourceRole")("id").ToString
        Console.WriteLine(String.Format("           with roleId = {0}", mycompRoleId))

        ' define the process
        Dim prdata As String = ""
        Dim myjobs As String = ""
        Dim mypres As String = ""
        Dim myosets As String = ""
        Dim cury = 100
        For Each job In joblist
            If myjobs.Length > 0 Then
                myjobs += ","
                myosets += ","
            End If
            myjobs += "{" + String.Format("""name"":""{0}"",", job)
            myjobs += """allowfailure"":false,"
            myjobs += """useImpersonation"":false,"
            myjobs += """showHidden"":true,"
            myjobs += """children"":[],"
            myjobs += """impersonationUseSudo"":false,"
            myjobs += """type"":""plugin"","
            myjobs += """pluginName"":""zOS Utility"","
            myjobs += """pluginVersion"":27,"
            myjobs += """commandName"":""Submit Job"","
            myjobs += """properties"":{"
            myjobs += """mvsFilename"":"""","
            myjobs += """ussFilename"":"""","
            myjobs += String.Format("""jclString"":""{0}"",", jcllist(job))
            myjobs += """jobCard"":""${p?:deploy.env.jobstatement}"","
            myjobs += """explicitTokens"":"""","
            myjobs += """explicitTokensForeachJob"":"""","
            myjobs += """waitForJob"":true,"
            myjobs += """stopOnFail"":true,"
            myjobs += """timeout"":60,"
            myjobs += """showOutput"":""ALL"","
            myjobs += """cutOff"":1000,"
            myjobs += String.Format("""maxRC"":{0},", maxcc(job))
            myjobs += """hostname"":""${p:jes.host}"","
            myjobs += """port"":""${p:jes.monitor.port}"","
            myjobs += """userid"":""${p:jes.user}"","
            myjobs += """password"":""${p?:jes.password}"","
            myjobs += """usePassticket"":false,"
            myjobs += """irrracfJarFile"":""/usr/include/java_classes/IRRRacf.jar"","
            myjobs += """irrracfLibraryPath"":""/usr/lib"""
            myjobs += "}}"
            myosets += "{" + String.Format("""name"":""{0}"",""x"":-70,""y"":{1}", job, cury) + "}"
            cury += 270
        Next
        If myjobs.Length > 0 Then
            myjobs += ","
        End If
        If myosets.Length > 0 Then
            myosets += ","
        End If
        ' add Finish process element - required by UCD
        myjobs += "{""type"":""finish"",""name"":""Finish"",""children"":[]}"
        myosets += "{" + String.Format("""name"":""Finish"",""x"":-70,""y"":{0}", cury) + "}"
        For Each dep In deplist
            If mypres.Length > 0 Then
                mypres += ","
            End If
            Dim fromStr As String = ""
            Dim toStr As String = ""
            Dim typeStr As String = ""
            Dim preparts As String() = dep.Split("-")
            If preparts.Count > 1 Then
                fromStr = String.Format("""from"":""{0}"",", preparts(0))
                toStr = String.Format("""to"":""{0}"",", preparts(1).Trim(">".ToCharArray()))
                typeStr = """type"":""SUCCESS"","
            Else
                toStr = String.Format("""to"":""{0}"",", preparts(0))
                typeStr = """type"":""ALWAYS"","
            End If
            mypres += "{" + String.Format("{0}{1}{2}""value"":""""", fromStr, toStr, typeStr) + "}"
        Next
        prdata = "{" + String.Format("""component"":""{0}"",", mycompid)
        prdata += """configActionType"":""ADD"",""defaultWorkingDir"":""${p:resource/work.dir}/${p:component.name}"","
        prdata += String.Format("""description"":""Process for {0}"",", adname)
        prdata += """inventoryActionType"":""ADD"","
        prdata += String.Format("""name"":""{0}"",", adname)
        prdata += """status"":""Active"",""takesVersion"":false,""rootActivity"":{"
        prdata += String.Format("""children"":[{0}],""edges"":[{1}],""layoutMode"":""auto"",""name"":""{3}-GRAPH"",""offsets"":[{2}],", myjobs, mypres, myosets, adname)
        prdata += """type"":""graph""}}"
        Console.WriteLine("Creating process")
        Dim crCompProc As String = HttpPUT("componentProcess/create", prdata)
        If crCompProc.StartsWith("OK") Then
            crCompProc = crCompProc.Substring(3)
        Else
            Console.WriteLine("Error encountered creating component process: " + crCompProc)
            Exit Sub
        End If
        Console.WriteLine("Success")
        Dim componentProcess = JsonConvert.DeserializeObject(crCompProc)
        Dim mycompProcId = componentProcess("id")
        Console.WriteLine(String.Format("Generated comp/proc id = {0}", mycompProcId))

        Console.WriteLine(String.Format("Verifying application 'Schedules' ({0}) exists", applicationName))
        Dim appinfo As String = HttpGET(String.Format("application/info?application={0}", Uri.EscapeDataString(applicationName)))
        If appinfo.StartsWith("OK") Then
            appinfo = appinfo.Substring(3)
        Else
            Console.WriteLine("Error encountered fetching application information: " + appinfo)
            Exit Sub
        End If
        Dim appdata = JsonConvert.DeserializeObject(appinfo)
        Dim myappid As String = appdata("id")
        Console.WriteLine(String.Format("Retrieved app id       = {0}", myappid))

        Console.WriteLine(String.Format("Retrieving list of application 'Schedules' ({0}) environments", applicationName))
        Dim environmentinfo = HttpGET(String.Format("application/environmentsInApplication?application={0}", myappid))
        If environmentinfo.StartsWith("OK") Then
            environmentinfo = environmentinfo.Substring(3)
        Else
            Console.WriteLine("Error encountered fetching application environments: " + environmentinfo)
        End If
        Dim scheddata = JsonConvert.DeserializeObject(environmentinfo)
        Dim environmentid As String = ""
        Dim environmentList As String = ""
        For Each schedenv In scheddata
            If schedenv("name") = applicationEnvironment Then
                environmentid = schedenv("id")
                Exit For
            End If
            If environmentList.Length > 0 Then
                environmentList += ", "
            End If
            environmentList += schedenv("name")
        Next
        If environmentid.Length > 0 Then
            Console.WriteLine(String.Format("Environment id found   = {0}", environmentid))
        Else
            Console.WriteLine(String.Format("ERROR - Unable to locate environment {0} in application {1} environments list: {3}", applicationEnvironment, applicationName, environmentList))
            Exit Sub
        End If

        Console.WriteLine(String.Format("Retrieving list of base resources for application 'Schedules' ({0}) environment {1}", applicationName, applicationEnvironment))
        Dim resourceinfo As String = HttpGET(String.Format("environment/getBaseResources?environment={0}", environmentid))
        If resourceinfo.StartsWith("OK") Then
            resourceinfo = resourceinfo.Substring(3)
        Else
            Console.WriteLine("Error encountered fetching base resources for environment {0}: {1}", applicationEnvironment, resourceinfo)
            Exit Sub
        End If
        Dim resourcedata = JsonConvert.DeserializeObject(resourceinfo)
        Dim resourceid As String = ""
        Dim resourcename As String = ""
        For Each res In resourcedata
            resourceid = res("id")
            resourcename = res("name")
            Exit For
        Next
        If resourceid.Length > 0 Then
            Console.WriteLine(String.Format("1st resource id (name) = {0} ({1})", resourceid, resourcename))
        Else
            Console.WriteLine("ERROR: No base resources found")
            Exit Sub
        End If

        Console.WriteLine(String.Format("Adding component {0} to application {1}", mycompname, applicationName))
        Dim compapp As String = HttpPUT(String.Format("application/addComponentToApp?component={0}&application={1}", mycompid, myappid), "")
        If compapp.StartsWith("OK") Then
            compapp = compapp.Substring(3)
        Else
            Console.WriteLine("Error encountered adding component to application: " + compapp)
            Exit Sub
        End If
        Console.WriteLine("Success")

        ' get any inheritted team info from the resource detail
        Console.WriteLine("Retrieve team inheritence data for resource")
        Dim teaminherit As String = HttpGET(String.Format("resource/info?resource={0}", resourceid))
        If teaminherit.StartsWith("OK") Then
            teaminherit = teaminherit.Substring(3)
        Else
            Console.WriteLine("Error retrieving resource team inheritence data: " + teaminherit)
            Exit Sub
        End If
        Dim teams As String = ""
        Dim dsresource = JsonConvert.DeserializeObject(teaminherit)
        Dim teamcollection = Nothing
        Try
            teamcollection = dsresource("extendedSecurity")("teams")
            For Each team In teamcollection
                If teams.Length > 0 Then
                    teams += ","
                End If
                teams += "{" + String.Format("""teamId"":""{0}"",""teamLabel"":""{1}""", team("teamId"), team("teamLabel")) + "}"
            Next
        Catch ex As Exception
        End Try
        Console.WriteLine("Got the following back: " + teams)

        Console.WriteLine(String.Format("Adding component to application environment resource ({0})", resourcename))
        ' assemble the JSON data string:
        Dim appresdata As String = "{"
        appresdata += """description"":"""","
        appresdata += String.Format("""roleId"":""{0}"",", mycompRoleId)
        appresdata += String.Format("""name"":""{0}"",", mycompname)
        appresdata += """inheritTeam"":""true"","
        appresdata += """useImpersonation"":""false"","
        appresdata += """roleProperties"":{},"
        appresdata += String.Format("""parentId"":""{0}"",", resourceid)
        appresdata += String.Format("""teamMappings"":[{0}]", teams)
        appresdata += "}"
        Dim arresp As String = HttpPUT("resource/create", appresdata)
        If arresp.StartsWith("OK") Then
            arresp = arresp.Substring(3)
        Else
            Console.WriteLine("Error encountered adding component to base resource: ", arresp)
            Console.WriteLine("Request data: " + appresdata)
            Exit Sub
        End If
        Console.WriteLine("Success")
        ' Define the application process:
        apiType = "applicationProcess"
        apiFunc = "create"
        Dim appprocname As String = String.Format("{0}-APP", adname)
        Dim adat As String = "{""application"":""" & myappid & """," &
                             """name"":""" & appprocname & """," &
                             """description"":""" & adname & " AD process""," &
                             """inventoryManagementType"":""AUTOMATIC""," &
                             """offlineAgentHandling"":""PRE_EXECUTION_CHECK""," &
                             """rootActivity"":{" &
                               """children"":[" &
                                 "{" &
                                   """type"":""finish""," &
                                   """name"":""Finish""," &
                                   """children"":[]" &
                                 "},{" &
                                   """type"":""componentEnvironmentIterator""," &
                                   """name"":""" & mycompProcId & """," &
                                   """componentName"":""" & mycompname & """," &
                                   """properties"":[" &
                                     "{" &
                                     """name"":""maxIterations""," &
                                     """value"":""-1""" &
                                     "},{" &
                                     """name"":""runOnlyOnFirst""," &
                                     """value"":""false""" &
                                     "},{" &
                                     """name"":""failFast""," &
                                     """value"":""false""" &
                                     "}" &
                                   "]," &
                                   """children"":[{" &
                                     """componentProcessName"":""" & adname & """," &
                                     """componentName"":""" & mycompname & """," &
                                     """allowFailure"":false," &
                                     """properties"":{}," &
                                     """type"":""componentProcess""," &
                                     """name"":""" & adname & """," &
                                     """children"":[]" &
                                   "}]" &
                                 "}" &
                               "]," &
                               """edges"":[" &
                                 "{" &
                                   """to"":""Finish""," &
                                   """from"":""" & mycompProcId & """," &
                                   """type"":""SUCCESS""," &
                                   """value"":""""" &
                                 "},{" &
                                   """to"":""" & mycompProcId & """," &
                                   """type"":""ALWAYS""," &
                                   """value"":""""" &
                                 "}" &
                               "]," &
                               """offsets"":[" &
                                 "{" &
                                   """name"":""" & mycompProcId & """,""x"":-35,""y"":180" &
                                 "},{" &
                                   """name"":""Finish"",""x"":-5,""y"":330" &
                                 "}" &
                               "]," &
                               """layoutMode"":""auto""," &
                               """type"":""graph""" &
                             "}}"
        Console.WriteLine(String.Format("Creating application {0} process {1}", applicationName, appprocname))
        Dim crappprocresp As String = HttpPUT(String.Format("{0}/{1}", apiType, apiFunc), adat)
        If crappprocresp.StartsWith("OK") Then
            crappprocresp = crappprocresp.Substring(3)
        Else
            Console.WriteLine(String.Format("Error encountered creating application process {0}: {1}", appprocname, crappprocresp))
            Exit Sub
        End If
        Dim crappdata = JsonConvert.DeserializeObject(crappprocresp)
        Dim mycrappid As String = crappdata("id")
        Console.WriteLine("Success")
        Console.WriteLine(String.Format("Retrieved app proc id  = {0}", mycrappid))
        Console.WriteLine(String.Format("TWSz AD {0} has been deployed to application {1} as component {2}", adname, applicationName, mycompname))
    End Sub

    Function HttpPUT(reqFunc As String, data As String) As String
        Dim strurl = String.Format("{0}/cli/{1}", __baseurl, reqFunc)
        ' define the request
        Dim req1 As HttpWebRequest = DirectCast(WebRequest.Create(strurl), HttpWebRequest)
        ' set the headers
        req1.Accept = "application/json"
        req1.Method = "PUT"
        req1.KeepAlive = True
        req1.UserAgent = "VB.NET TWSz AD Importer"
        ' NOTE: The following is required to stop .Net adding header 
        '       "Expect: 100-continue" which is not supported by DB2
        req1.ServicePoint.Expect100Continue = False
        Dim cookieStr As String = ""
        If __ucdCsrfToken.Length > 0 Then
            req1.Headers.Add(__ucdCsrfToken_Name, __ucdCsrfToken_Name)
            cookieStr = String.Format("{0}={1}", __ucdCsrfToken_Name, __ucdCsrfToken)
        End If
        If __ucdSessionId.Length > 0 Then
            If cookieStr.Length > 0 Then
                cookieStr += "; "
            End If
            cookieStr += String.Format("{0}={1}", __ucdSessionId_Name, __ucdSessionId)
            req1.Headers.Add(__ucdSessionId_Name, __ucdSessionId)
        End If
        If __jsessionId.Length > 0 Then
            If cookieStr.Length > 0 Then
                cookieStr += "; "
            End If
            cookieStr += String.Format("{0}={1}", __jsessionId_Name, __jsessionId)
            req1.Headers.Add(__jsessionId_Name, __jsessionId)
        End If
        req1.Headers.Add("Cookie", cookieStr)
        ' set UTF-8 encoding - used for parm encoding
        Dim enc As New System.Text.UTF8Encoding
        ' apply the JSON parameters to the request - if there are any
        If data.Length > 0 Then
            Dim bdata As Byte() = enc.GetBytes(data)
            Dim sw As Stream = req1.GetRequestStream()
            sw.Write(bdata, 0, bdata.Length)
            sw.Close()
        End If
        ' Make the request
        Dim resp As HttpWebResponse
        Try
            resp = DirectCast(req1.GetResponse(), HttpWebResponse)
        Catch e As System.Net.WebException
            resp = e.Response
        End Try
        If resp Is Nothing Then
            Console.WriteLine("No response from the server")
            HttpPUT = "ERROR - no response from the server"
            Exit Function
        End If
        Dim rv As String = "OK "
        If resp.StatusCode <> HttpStatusCode.OK Then
            rv = String.Format("ERROR RETURNED: {0}{1}", resp.StatusCode, vbNewLine)
        End If
        ' Return response data
        Dim sr As StreamReader = New StreamReader(resp.GetResponseStream())
        Dim srJSON As String = sr.ReadToEnd()
        sr.Close()
        resp.Close()
        HttpPUT = rv + srJSON
    End Function

    Function HttpGET(reqFunc As String) As String
        Dim strurl = String.Format("{0}/cli/{1}", __baseurl, reqFunc)
        ' define the request
        Dim req1 As HttpWebRequest = DirectCast(WebRequest.Create(strurl), HttpWebRequest)
        ' set the headers
        req1.Accept = "application/json"
        req1.Method = "GET"
        req1.KeepAlive = True
        req1.UserAgent = "VB.NET TWSz AD Importer"
        ' NOTE: The following is required to stop .Net adding header 
        '       "Expect: 100-continue" which is not supported by DB2
        req1.ServicePoint.Expect100Continue = False
        Dim cookieStr As String = ""
        If __ucdCsrfToken.Length > 0 Then
            req1.Headers.Add(__ucdCsrfToken_Name, __ucdCsrfToken_Name)
            cookieStr = String.Format("{0}={1}", __ucdCsrfToken_Name, __ucdCsrfToken)
        End If
        If __ucdSessionId.Length > 0 Then
            If cookieStr.Length > 0 Then
                cookieStr += "; "
            End If
            cookieStr += String.Format("{0}={1}", __ucdSessionId_Name, __ucdSessionId)
            req1.Headers.Add(__ucdSessionId_Name, __ucdSessionId)
        End If
        If __jsessionId.Length > 0 Then
            If cookieStr.Length > 0 Then
                cookieStr += "; "
            End If
            cookieStr += String.Format("{0}={1}", __jsessionId_Name, __jsessionId)
            req1.Headers.Add(__jsessionId_Name, __jsessionId)
        End If
        req1.Headers.Add("Cookie", cookieStr)
        ' set UTF-8 encoding - used for parm encoding
        Dim enc As New System.Text.UTF8Encoding
        ' Make the request
        Dim resp As HttpWebResponse
        Try
            resp = DirectCast(req1.GetResponse(), HttpWebResponse)
        Catch e As System.Net.WebException
            resp = e.Response
        End Try
        If resp Is Nothing Then
            Console.WriteLine("No response from the server")
            HttpGET = "ERROR: No response from the server"
            Exit Function
        End If
        Dim rv As String = "OK "
        If resp.StatusCode <> HttpStatusCode.OK Then
            rv = String.Format("ERROR RETURNED: {0}{1}", resp.StatusCode, vbNewLine)
        End If
        ' Return response data
        Dim sr As StreamReader = New StreamReader(resp.GetResponseStream())
        Dim srJSON As String = sr.ReadToEnd()
        sr.Close()
        resp.Close()
        HttpGET = rv + srJSON
    End Function

    ' Separate function as we need to process the response headers
    Function Logon(serverUrl As String, userid As String, password As String) As Boolean
        Console.WriteLine(String.Format("Logging on to {0} as {1}", serverUrl, userid))
        ' drive application list command to get the Set-Cookie response header, and extract the UCD_SESSION_KEY
        Dim reqUrl As String = String.Format("{0}/cli/application", serverUrl)
        ' declare request and response
        Dim req As HttpWebRequest
        Dim resp As HttpWebResponse
        ' set UTF-8 encoding - used for parm encoding
        Dim enc As New System.Text.UTF8Encoding
        ' define the request
        req = DirectCast(WebRequest.Create(reqUrl), HttpWebRequest)
        ' set the headers
        req.ContentType = "application/json"
        req.Method = "GET"
        req.KeepAlive = True
        ' NOTE: The following is required to stop .Net adding header 
        '       "Expect: 100-continue" which is not supported by DB2
        req.ServicePoint.Expect100Continue = False
        ' create the base64 encoded authorization string
        Dim usrpwd As String = String.Format("{0}:{1}", userid, password)
        Dim upbytes As Byte() = Encoding.ASCII.GetBytes(usrpwd)
        Dim authstr As String = Convert.ToBase64String(upbytes)
        req.Headers.Add("Authorization", String.Format("Basic {0}", authstr))
        Try
            resp = DirectCast(req.GetResponse(), HttpWebResponse)
        Catch e As System.Net.WebException
            Console.WriteLine("HTTP GET Request error status: {0}", e.Message)
            resp = e.Response
        End Try
        If resp Is Nothing Then
            Console.WriteLine("No response received from the server")
            Logon = False
            Exit Function
        End If
        ' Scan response headers for Set-Cookie
        Dim rsphdrs As WebHeaderCollection = resp.Headers()
        Dim i As Integer = 0
        Dim csrf_token As String = ""
        Dim jsessionid As String = ""
        Dim sessionkey As String = ""
        While i < rsphdrs.Count()
            If rsphdrs.Keys(i) = "Set-Cookie" Then
                Dim jpos As Integer = rsphdrs.Item(i).IndexOf(__partJSessionId_Name)
                If jpos >= 0 Then
                    Dim jparts As String() = rsphdrs.Item(i).Substring(jpos).Split("=;".ToCharArray())
                    __jsessionId_Name = jparts(0)
                    __jsessionId = jparts(1)
                End If
                Dim cpos As Integer = rsphdrs.Item(i).IndexOf(__ucdCsrfToken_Name)
                If cpos >= 0 Then
                    Dim cparts As String() = rsphdrs.Item(i).Substring(cpos).Split("=;".ToCharArray())
                    __ucdCsrfToken = cparts(1)
                End If
                Dim upos As Integer = rsphdrs.Item(i).IndexOf(__ucdSessionId_Name)
                If upos >= 0 Then
                    Dim uparts As String() = rsphdrs.Item(i).Substring(upos).Split("=;".ToCharArray())
                    __ucdSessionId = uparts(1)
                End If
            End If
            i += 1
        End While
        Dim sri As StreamReader = New StreamReader(resp.GetResponseStream())
        Dim sriJSON As String = sri.ReadToEnd()
        sri.Close()
        resp.Close()
        __baseurl = serverUrl
        Console.WriteLine("Success")
        Logon = True
    End Function

    Dim selfSignedButAllowed As X509Certificate2Collection = New X509Certificate2Collection()
    Function ValidateServerCertificate(sender As Object, certificate As X509Certificate, chain As X509Chain, sslPolicyErrors As SslPolicyErrors) As Boolean
        If sslPolicyErrors = sslPolicyErrors.None Then
            Return True
        Else
            Dim srsn As String = ""
            Dim certFlaggedInError As X509Certificate2Collection = New X509Certificate2Collection()
            If sslPolicyErrors = Net.Security.SslPolicyErrors.RemoteCertificateNameMismatch Then
                srsn = "Certificate name mismatch"
            ElseIf sslPolicyErrors = Net.Security.SslPolicyErrors.RemoteCertificateNotAvailable Then
                srsn = "Remote server certificate not available"
            ElseIf sslPolicyErrors = Net.Security.SslPolicyErrors.RemoteCertificateChainErrors Then
                Dim els As X509ChainElementCollection = chain.ChainElements()
                Dim i As Integer = 0
                For Each el In els
                    Dim thiscert As X509Certificate2 = el.Certificate()
                    If Not selfSignedButAllowed.Contains(thiscert) Then
                        Dim s As String = ""
                        For Each st In el.ChainElementStatus()
                            Dim status As String = st.Status()
                            Dim stinf As String = st.StatusInformation()
                            If status.Length() > 0 Then
                                s = s & String.Format("{0} - {1} ", st.Status(), st.StatusInformation())
                                certFlaggedInError.Add(thiscert)
                            End If
                        Next
                        If s.Length > 0 Then
                            srsn = String.Format("{0} : {1}", thiscert.GetNameInfo(X509NameType.SimpleName, False), s)
                        End If
                    End If
                    i += 1
                Next
            End If
            If srsn.Length() > 0 Then
                If MsgBox("Server certificate is not valid" & vbNewLine & vbNewLine & srsn & vbNewLine & "Accept?", MsgBoxStyle.OkCancel, "DB2REST") = MsgBoxResult.Ok Then
                    For Each errcert In certFlaggedInError
                        selfSignedButAllowed.Add(errcert)
                    Next
                    Return True
                Else
                    Return False
                End If
            Else
                Return True
            End If
        End If
        Return True
    End Function

End Module
