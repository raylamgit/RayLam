//DDS0001L JOB REGION=4M,CLASS=A,                                               
// TIME=(1),MSGCLASS=H,NOTIFY=&SYSUID,MSGLEVEL=(1,1)                            
//********************************************************************          
//*   RUN SAMPLE PROGRAM SAM1                                                   
//********************************************************************          
//RUNSAM1 EXEC PGM=SAM1,REGION=4M                                               
//STEPLIB  DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                    
//         DD DSN=LNKLST.DEBUG.V11R1.SEQAMOD,DISP=SHR                           
//*        DD DSN=LNKLST.DEBUG.V10R1.SEQAMOD,DISP=SHR                           
//*        DD DSN=DEBUG.V8R1.SEQAMOD,DISP=SHR                                   
//**  //INSPPREF DD DSN=DDS0001.TEST.DTPREF,DISP=SHR                            
//**  //INSPLOG  DD DSN=DDS0001.TEST.DTLOG,DISP=SHR                             
//CEEOPTS  DD *                                                                 
    TEST(,,,TCPIP&9.76.102.205%8003:)                                           
//CUSTFILE DD DSN=DDS0001.TEST.SAMFILE,DISP=SHR                                 
//SYSPRINT DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//CUSTRPT  DD SYSOUT=*                                                          
//CUSTOUT  DD SYSOUT=*                                                          
//TRANFILE DD *                                                                 
*TRAN  KEY        ACTION   FIELD NAME    VALUE                                  
*----- ---------- -------- ------------- +999999..                              
UPDATE 07025A     ADD      BALANCE       +00000001                              
UPDATE 11112A     ADD      BALANCE       +00000123                              
UPDATE 11204A     ADD      ORDERS        +000999                                
UPDATE 11204A     REPLACE  ORDERS        0000999                                
UPDATE 11204A     REPLACE  BALANCE       022334455                              
UPDATE 11204A     REPLACE  NAME          DOOGIE STOUT                           
UPDATE 13062A     ADD      BALANCE       +00000789                              
DELETE 26620A                                                                   
ADD    55555A                                                                   
/*                                                                              
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * *                             
//*                                                                             
//*   SAMPLE EXEC PARMS TO START DEBUG TOOL:                                    
//*   --------------------------------------                                    
//*      CONNECT TO A VTAM TERMINAL IN BATCH:                                   
//*          PARM='/TEST(,,,MFI%TERMID:)'                                       
//*      CONNECT TO A TERMINAL, AND USE A COMMAND AND PREFERENCE FILE:          
//*          PARM='/TEST(,INSPIN,,MFI%TERMID:INSPPREF)'                         
//*      CONNECT TO A REMOTE DEBUGGER, PORT 8001:                               
//*          PARM='/TEST(,,,TCPIP&1.2.3.4%8001:)'                               
//*                                                                             
//*   SAMPLE OPTIONAL FILES FOR DEBUG TOOL                                      
//*   --------------------------------------                                    
//*    PREFERENCES FILE:                                                        
//*       //INSPPREF DD DSN=DDS0001.TEST.DTPREF,DISP=SHR                        
//*    COMMAND FILE:                                                            
//*       //INSPIN   DD DSN=DDS0001.TEST.DTCMD,DISP=SHR                         
//*    LOG FILE:                                                                
//*       //INSPLOG  DD DSN=DDS0001.TEST.DTLOG,DISP=SHR                         
//*    EQADEBUG FILE (CONCATENATION OF SYSDEBUG AND LISTING FILES):             
//*       //EQADEBUG DD DSN=DDS0001.TEST.SYSDEBUG,DISP=SHR                      
//*       //         DD DSN=TEST.SYSDEBUG,DISP=SHR                              
//*       //         DD DSN=QA.SYSDEBUG,DISP=SHR                                
//*       //         DD DSN=PROD.SYSDEBUG,DISP=SHR                              
//*                                                                             
//*   SAMPLE OPTIONAL FILES FOR FAULT ANALYZER:                                 
//*   -----------------------------------------                                 
//*    IDIOPTS CAN BE USED TO SPECIFY FAULT ANALYZER PARAMETERS                 
//IDIOPTS  DD *                                                                 
 INCLUDE,MAXMINIDUMPPAGES(1000)                                                 
 NODUP(NORMAL(0))                                                               
/*                                                                              
//*    IDILCOB IS A PDS CONCATENATION FOR COBOL COMPILER LISTINGS:              
//*       //IDILCOB  DD DSN=DDS0001.TEST.LISTING,DISP=OLD                       
//*    IDITRACE CAN BE USED TO PRINT TRACE OF LISTINGS SEARCH                   
//IDITRACE DD SYSOUT=*                                                          
