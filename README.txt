This tool is intended to be used to port an OPC AD to UCD. To do this:

1. Use the mainframe elements (in .\mainframe):
   a) Use ADEXPRT.jcl to generate an export file
   b) Run ADJCL against OPC using the file just generated to stage all of the JCL
2. ASCII download all of the members created in the staing PDS
3. Run TWSz2UCD against the directory that you downloaded everything into:
   a) TWsz2UCD config.file download.directory

