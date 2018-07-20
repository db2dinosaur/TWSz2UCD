//*
//*--------------------------------------------------------------------
//*
//* EXPORT AN OPC AD IN BATCH LOADER FORMAT
//*
//*--------------------------------------------------------------------
//*
//* TIDY UP FROM PREVIOUS RUN
//*
//DELETE  EXEC PGM=IEFBR14
//OLDLIST  DD  DSN=GILLJ.OPCA.JES2HK.EXPORT,
//             UNIT=SYSDA,SPACE=(TRK,0),DISP=(MOD,DELETE)
//*
//* DEFINE NEW OUTPUT DATASET
//*
//ALLOC   EXEC PGM=IEFBR14
//NEWLIST  DD  DSN=GILLJ.OPCA.JES2HK.EXPORT,
//             UNIT=SYSDA,DISP=(,CATLG),SPACE=(CYL,(2,5),RLSE),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=6240)
//*
//* RUN THE EXPORT
//*
//EXPORT  EXEC PGM=IKJEFT01,REGION=4M,
//             PARM='EQQYXTOP OPCA'
//SYSPROC  DD  DISP=SHR,DSN=OPCA.SEQQMISC
//SYSTSPRT DD  SYSOUT=*
//SYSTSIN  DD  DUMMY
//EQQMLIB  DD  DISP=SHR,DSN=OPCA.SEQQMSG0
//EQQMLOG  DD  SYSOUT=*
//EQQDUMP  DD  SYSOUT=*
//EQQSMTP  DD  SYSOUT=(,)
//OUTDATA  DD  SYSOUT=*,LRECL=4096
//OUTBL    DD  DSN=GILLJ.OPCA.JES2HK.EXPORT,DISP=SHR
//SYSIN    DD  *
OPTIONS STRIP(Y) EXPAND(N) SELECT(Y)
LOADDEF AD* DATA(-)
LIST ADCOM ADID(JES2HK)
/*
