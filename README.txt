This tool is intended to be used to port an OPC AD to UCD. To do this:

1. Use the mainframe elements (in .\mainframe):
   a) Use ADEXPRT.jcl to generate an export file
   b) Run ADJCL against OPC using the file just generated to stage all of the JCL
2. ASCII download all of the members created in the staing PDS
3. Create a config file - see TWSz2UCD/bind/debug/config.txt
   a) Current valid config file stanzas are reported by:
       TWSz2UCD --help
4. Run TWSz2UCD using the config file:
       TWsz2UCD config.file

James Gill - July 2018
