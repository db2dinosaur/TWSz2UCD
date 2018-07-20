/* Rexx -------------------------------------------------------------*/
/*                                                                   */
/* OPCJOBLB                                                          */
/* ========                                                          */
/* Capture the JCL for jobs in an OPC AD, along with the AD EXPORT   */
/* file. To do this, we need to know the concatenation of the OPC    */
/* EQQJBLIB DD, then read the AD EXPORT file to get all of the job   */
/* names, then finally copy everything into a staging library.       */
/*                                                                   */
/* Because the relevent control blocks (ASXB and TIOT) reside in     */
/* private storage, we'd have to schedule an SRB to get them, which  */
/* (authorised code) is a little over the top, so we've used the     */
/* Rexx/SDSF interface instead.                                      */
/*                                                                   */
/* Inputs:                                                           */
/* + Parameters:                                                     */
/*   1. OPC STC name (to search for EQQJBLIB)                        */
/*                                                                   */
/* + ADEXPRT DD for AD EXPORT file. See ADEXPRT job.                 */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*                                                                   */
/* James Gill - June 2018                                            */
/*                                                                   */
/*-------------------------------------------------------------------*/
   parse upper arg iasname
   /* check parms */
   if iasname = "" then do
      say left("",72,"-")
      say " "
      say "ERROR - Missing parameter"
      say " "
      say "Please use the following call format:"
      say " "
      say "  OPCJOBLB opcstc"
      say " "
      say "Where opcstc is the name of the OPC address space to query"
      say " "
      say left("",72,"-")
      exit 8
   end
   /* check DD ADEXPRT is allocated */
   adexprt = CheckAlloc("ADEXPRT")
   if substr(adexprt,1,1) = "#" then do
      say left("",72,"-")
      say " "
      say "ERROR - Problem with ADEXPRT DD:"
      say " "
      say "   "substr(astat,2)
      say " "
      say left("",72,"-")
      exit 8
   end

   /* Startup messages */
   say " "
   say "OPCJOBLB"
   say "========"
   asname = strip(iasname)
   dd     = "EQQJBLIB"
   ddtrip = "XX"dd
   say "Searching for DD "dd" in "asname

   /* set staging dataset name */
   jdate = "D"date('J')
   t = time()
   stime = "T"substr(t,1,2)||substr(t,4,2)||substr(t,7)
   staging = userid()".STAGING."jdate"."stime

   debug = 0
   xrc = isfcalls('ON')
   if xrc /= 0 then do
      say left("",72,"-")
      say " "
      say "ERROR: ISFCALLS('ON') returned RC("xrc")"
      say " "
      say left("",72,"-")
      exit 12
   end
   say "Rexx/SDSF interface started"
/* find asname */
   this_sys = mvsvar('SYSNAME')
   say "Running on system "this_sys
/* report run details (SDSF WHO command) - used to clarify ISFPRM00 */
/* batch mode ILPROC value (REXX) - uncomment to reuse */
/* Address SDSF "ISFEXEC WHO"
   wh_rc = rc
   if wh_rc /= 0 then do
      call SdsfError "ERROR: WHO returned RC("wh_rc")"
   end
   say "SDSF WHO response:"
   do i = 1 to isfresp.0
      say right(i,3)": "isfresp.i
   end
*/
   /* Effective SDSF: PREFIX asname */
   isfprefix = asname
   /* Limit output to the current system only */
   isfsysname = this_sys
   /* Look for asname on this_sys using SDSF DA */
   Address SDSF "ISFEXEC DA"
   st_rc = rc
   if st_rc /= 0 then do
      call SdsfError "ERROR: SDSF 'DA' RC("st_rc")"
   end
   found = 0
   tok = -1
   i = jname.0
   if i = 0 then do
      call SdsfError "ERROR: "asname" not found"
   end
   else if i /= 1 then do
      call SdsfError "More than one "asname" appears to be active"
   end
   astok = token.1
/* get the asname datasets - SDSF "?" line command */
   Address SDSF "ISFACT DA TOKEN('"astok"') PARM(NP ?)"
   da_rc = rc
   if da_rc /= 0 then do
      call SdsfError "ERROR: SDSF DA TOK failed - RC("da_rc")"
   end
   jtok = 0
   do i = 1 to ddname.0
      if ddname.i = "JESJCL" then do
         jtok = token.i
         leave i
      end
   end
   if jtok = 0 then do
      call SdsfError "ERROR: Unable to find JESJCL for "asname
   end
/* select JESJCL for asname */
   Address SDSF "ISFACT DA TOKEN('"jtok"') PARM(NP SA)"
   qs_rc = rc
   if qs_rc /= 0 then do
      call SdsfError "ERROR: ISFACT selecting JESJCL - RC("qs_rc")"
   end
/* process each of the ddnames/datasets returned (should be only one) */
   dsn. = ""
   dsn.0 = 0
   say "Reading "isfddname.0" JESJCL dataset(s):"
   do i = 1 to isfddname.0
      say right(i,3)": Reading DD "isfddname.i", dataset "isfdsname.i
      Address TSO "EXECIO * DISKR "isfddname.i" (STEM in. FINIS"
      say "     Read "in.0" lines:"
      /* read through the JCL looking for our DD name */
      ddstmt = ""
      capture_on = 0
      do j = 1 to in.0
         if word(in.j,2) = ddtrip then do  /* start of our DD */
            /* make next bit process this line as well */
            in.j = substr(in.j,1,12)" "substr(in.j,14)
            capture_on = 1
         end
         if capture_on then do
            if substr(in.j,13,1) /= " " then do
               if substr(in.j,11,2) = "XX" then capture_on = 0
            end
            else do
               /* if this line contains "DSN="... */
               dpos = pos("DSN=",in.j)
               if dpos > 0 then do
                  /* dsname ends in a comma? */
                  cpos = pos(",",in.j,dpos+4)
                  /* dsname ends in a space? */
                  if cpos < dpos then cpos = pos(" ",in.j,dpos+4)
                  /* dsname last text in line/stmt? */
                  if cpos < dpos then cpos = length(in.j) + 1
                  /* capture dataset name */
                  dsname = substr(in.j,dpos+4,cpos - (dpos+4))
                  /* if JCL, add, if substitution message, replace */
                  if substr(in.j,11,2) = "XX" then r = add(dsname)
                                              else r = rep(dsname)
               end
            end
         end
      end
   end
   say "OPC subsystem "asname" EQQJBLIB references:"
   dslist = ""
   do i = 1 to dsn.0
      dslist = dslist" '"dsn.i"'"
      say right(i,3)" - "dsn.i
   end

   say "Now reading AD export file ("adexprt")"
   Address TSO "EXECIO * DISKR ADEXPRT (STEM in. FINIS"
   say "Read "in.0" lines"
   stmt = ""
   jobn. = ""
   jobn.0 = 0
   say "Allocating joblib datasets"
   Address TSO "ALLOC F(CONCAT) DA("dslist") SHR REUSE"
   Address IspExec "LMINIT DATAID(cdid) DDNAME(CONCAT)"
   Address IspExec "LMOPEN DATAID("cdid")"
   say "Job to JCL mapping:"
   do i = 1 to in.0
      instr = in.i
      if substr(instr,1,1) = " " then stmt = stmt" "strip(instr)
      else do
         mem = processStanza(stmt)
         if mem /= "" then do
            Address IspExec "LMMFIND DATAID("cdid")",
                                    "MEMBER("mem") STATS(YES)"
            if rc = 0 then do
               pdsn = dsn.ZLLIB"("mem")"
               say mem "->" pdsn
               idx = jobn.0 + 1
               jobn.idx = mem" "pdsn
               jobn.0 = idx
            end
            else say "LMMFIND for "mem" RC("lm_rc")"
         end
         stmt = instr
      end
   end
   if stmt /= "" then prc = processStanza(stmt)
   Address IspExec "LMCLOSE DATAID("cdid")"
   Address IspExec "LMFREE DATAID("cdid")"
   Address TSO "FREE F(CONCAT)"
   say "Creating / Clearing temp staging DSN ("staging")"
   pv = msg("OFF")
   ot = outtrap("otl.","*","NOCONCAT")
   Address TSO "DELETE '"staging"'"
   ot = outtrap("OFF")
   pv = msg(pv)
   Address TSO "ALLOC F(STAGING) DA('"staging"') NEW CYL",
               "SP(5 2) DIR(20) RECFM(F B) LRECL(80) BLKSIZE(27920)"
   Address TSO "FREE F(STAGING)"
   say "Copying JCL members to staging:"
   do i = 1 to jobn.0
      jn = word(jobn.i,1)
      jd = word(jobn.i,2)
      Address TSO "ALLOC F(INMEM) DA('"jd"') SHR REUSE"
      Address TSO "ALLOC F(OUTMEM) DA('"staging"("jn")') SHR REUSE"
      Address TSO "EXECIO * DISKR INMEM (STEM dat. FINIS"
      Address TSO "EXECIO "dat.0" DISKW OUTMEM (STEM dat. FINIS"
      Address TSO "FREE F(INMEM)"
      Address TSO "FREE F(OUTMEM)"
      say "  "jd" -("dat.0")-> "staging"("jn")"
   end
   say "Copying AD export to staging as ##01"
   Address TSO "ALLOC F(OUTMEM) DA('"staging"(##01)') SHR REUSE"
   Address TSO "EXECIO "in.0" DISKW OUTMEM (STEM in. FINIS"
   Address TSO "FREE F(OUTMEM)"
   say " "
   say left("",79,"-")
   say " "
   say "All actions performed. To continue:"
   say " "
   say "1. ASCII download all members from the staging dataset:"
   say " "
   say "      "staging
   say " "
   say "2. Run the VB.NET tool TWSz2UCD against this download"
   say " "
   say left("",79,"-")
   say "Done"
return

/* Process a stanza from the AD EXPORT file. We're looking for ADOP  */
/* (AD operation) statements as these include the job names (JOBN).  */
/* Report the job name back to the caller */
processStanza:
   stanza = arg(1)
   jn = ""
   mode = word(stanza,1)
   if mode = "ADOP" then do
      st = pos("JOBN(",stanza)
      if st > 0 then do
         en = pos(")",stanza,st)
         jn = substr(stanza,st+5,(en - (st+5)))
      end
   end
return jn

/* Used to add a dataset to the list of OPC job lib datasets */
add:
   idx = 1 + dsn.0
   dsn.idx = arg(1)
   dsn.0 = idx
return idx

/* Used to replace a dataset in the list of OPC job lib datasets */
rep:
   idx = dsn.0
   dsn.idx = arg(1)
return idx

/* Report an error with Rexx/SDSF diags and end with RC=12 */
SdsfError:
   errMsg = arg(1)
   say left("",72,"-")
   say " "
   say errMsg
   say isfmsg
   do i = 1 to isfmsg2.0
      say isfmsg2.i
   end
   say " "
   say left("",72,"-")
exit 12

/* Check to see if DD is allocated. If okay, return dataset name,    */
/* if not, return "#"status - i.e. "#" as char 1, followed by an     */
/* error message.                                                    */
CheckAlloc: Procedure
   parmddn = arg(1)
   numeric digits 20
   foundDd = 0
   camsg = ""
   errmsg = ""
   dddsname = ""
   psa = 0
   tcb = c2d(storage(d2x(psa + 540),4))
   tiot = c2d(storage(d2x(tcb + 12),4))
   ix = 1
   dd = tiot + 24  /* start of DD entry */
   ddname = ""
   more_to_do = 1
   do while more_to_do
      ldd = c2d(storage(d2x(dd),1))
      if ldd = 0 then more_to_do = 0
      else do
         addname = dd + 4
         thisddn = strip(storage(d2x(addname),8))
         if substr(thisddn,1,1) /= " " then ddname = thisddn
         jfcb = dd + 12
         jfcbp = c2d(storage(d2x(jfcb),3))
         jfcbp = swareq(jfcbp)
         if jfcbp = 0 then more_to_do = 0
         else do
            dsname = storage(d2x(jfcbp),44)
            aflgs = dd + 23
            flgs  = c2x(storage(d2x(aflgs),2))
/*          say right(ix,3)" - "left(ddname,8)" : "dsname
*/          if ddname = parmddn then do
               foundDd = 1
               dddsname = dddsname" "strip(dsname)
            end
         end
      end
      dd = dd + ldd
      ix = ix + 1
   end
   if foundDd then camsg = strip(dddsname)
   else do
      camsg = "#DD "parmddn" not allocated"
   end
return camsg

/* Updated version of Gilbert Saint-Flour's Rexx implementation of   */
/* SWAREQ, used to map the three byte SVA in the TIOT to the actual  */
/* dataset name address */
swareq:
   numeric digits 20           /* Support 64-bit addresses           */
   jfcbdec = arg(1)            /* Decimal JFCB / SVA parm            */
   rv = 0                      /* Initialise return value to zero    */
   sva = jfcbdec               /* SVA in decimal for calculations    */
   psa = 0                     /* Address of PSA (by Architecture)   */
   psatold = 540               /* PSA -> current TCB                 */
   tcbjscb = 180               /* TCB -> JSCB                        */
   jscbqmpi = 244              /* JSCB-> QMPA                        */
   qmadd01  = 10               /* QMAT bytes 0-1 if QMQMAT64 is on   */
   qmadd23  = 18               /* QMAT bytes 2-3 if QMQMAT64 is on   */
   qmpaqmst = 16               /* QMPA QMSTA byte                    */
   qmpaqmad = 24               /* 31-bit QM address table pointer    */
   qmatqmat = 12               /* Next entry in QM address table     */
   thistcb = c2d(storage(d2x(psa + psatold),4))
   thisjscb = c2d(storage(d2x(thistcb + tcbjscb),4))
   thisqmpa = c2d(storage(d2x(thisjscb + jscbqmpi),4))
   qmsta = x2b(c2x(storage(d2x(thisqmpa + qmpaqmst),1)))
   if substr(qmsta,6,1) = 1 then do  /* QMQMAT64 i on                */
      if right(x2b(d2x(sva)),1) /= "1" then do  /* SWA=BELOW? Error! */
         rv = sva + 16         /* Only here because QMQMAT64 is on.. */
      end
      else do
         /* assemble QM address table 64-bit address */
         qmad1 = c2d(storage(d2x(thisqmpa + qmadd01),2)) * (2**48)
         qmad2 = c2d(storage(d2x(thisqmpa + qmadd23),2)) * (2**32)
         qmad3 = c2d(storage(d2x(thisqmpa + qmpaqmad),4))
         qmat = qmad1 + qmad2 + qmad3
         rv = c2d(storage(d2x(qmat + (sva * 12) + 64),4)) + 16
      end
   end
   else do  /* 31-bit address */
      jfcbhex = right(d2x(jfcbdec),6,"0")
      if right(jfcbhex,1) <> "F" then do  /* SWA=BELOW ?             */
        rv = sva + 16           /* Yes -> return SVA + 16            */
      end
      else do
         qmat = c2d(storage(d2x(thisqmpa + qmpaqmat),4))
         do while sva > 65536        /* x'010000'                    */
            qmat = c2d(storage(d2x(qmat + qmatqmat),4))
            sva = sva - 65536
         end
         rv = c2d(storage(d2x(qmat + sva + 1),4)) + 16
      end
   end
return rv
