//EMPOT394 JOB ,                                                                
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(1,1),REGION=70M,COND=(16,LT),                
//   NOTIFY=EMPOT39                                                             
//*                                                                             
//STP0000 EXEC PROC=ELAXFCOC,                                                   
// CICS=,                                                                       
// DB2=,                                                                        
// COMP=,                                                                       
//         PARM.COBOL=('ADATA',                                                 
//    'EXIT(ADEXIT(ELAXMGUX))',                                                 
//    'TEST,ADATA,EXIT(ADEXIT(ELAXMGUX)')                                       
//COBOL.SYSPRINT DD DSN=EMPOT39.TEST.LISTING(HOSPEDIT),                         
//          DISP=SHR                                                            
//COBOL.SYSDEBUG DD DSN=EMPOT39.TEST.SYSDEBUG(HOSPEDIT),                        
//          DISP=SHR                                                            
//COBOL.SYSLIN DD DSN=EMPOT39.TEST.OBJ(HOSPEDIT),                               
//          DISP=SHR                                                            
//COBOL.SYSLIB DD DSN=EMPOT39.TEST.COPYLIB,DISP=SHR                             
//COBOL.SYSXMLSD DD DUMMY                                                       
//COBOL.SYSIN DD DSN=DDS0001.TEST.COBOL(TEST1),DISP=SHR                         
//*                                                                             
//******* ADDITIONAL JCL FOR COMPILE HERE ******                                
//LKED EXEC PROC=ELAXFLNK                                                       
//LINK.SYSLIB DD DSN=CEE.SCEELKED,DISP=SHR                                      
//LINK.OBJ0000 DD DSN=EMPOT39.TEST.OBJ(HOSPEDIT),DISP=SHR                       
//LINK.SYSLIN DD *                                                              
     INCLUDE OBJ0000                                                            
/*                                                                              
//LINK.SYSLMOD   DD  DSN=EMPOT39.TEST.LOAD(HOSPEDIT),DISP=SHR                   
//*                                                                             
//******* ADDITIONAL JCL FOR LINK HERE ******                                   
//*GO    EXEC   PROC=ELAXFGO,GO=HOSPEDIT,                                       
//*           LOADDSN=EMPOT39.TEST.LOAD,                                        
//*         PARM.RUN=('/TEST(,,,TCPIP&&9.48.40.168%8001:*)')                    
//******* ADDITIONAL RUNTIME JCL HERE ******                                    
//                                                                              
//*LKED1 EXEC PROC=ELAXFLNK                                                     
//*LINK.SYSLIB DD DSN=EMPOT39.TEST.LOAD,DISP=SHR                                
//*LINK.SYSLIN DD *                                                             
//*     INCLUDE HOSPCALL                                                        
//**                                                                            
//*LINK.SYSLMOD   DD  DSN=EMPOT39.TEST.LOAD(HOSPEDIT),DISP=SHR                  
                                                                                
#2                                                                              
//EMPOT39T JOB ,                                                                
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(,4),REGION=70M,COND=(16,LT)                  
//*                                                                             
//STEP2    EXEC   PGM=HOSPEDIT                                                  
//STEPLIB DD DSN=EMPOT39.TEST.LOAD,DISP=SHR                                     
//CEEOPTS DD *                                                                  
    TEST(,,,TCPIP&9.163.90.81%8003:)                                            
//******* ADDITIONAL RUNTIME JCL HERE ******                                    
//INFILE DD DSN=EMPOT39.HOSPIN.DATA,DISP=SHR                                    
//SYSOUT DD DSN=EMPOT39.TEST.COPYLIB(SYSOUT),DISP=SHR                           
//ERROUT DD DSN=EMPOT39.ERROR.OUT(HOSPERR),DISP=SHR                             
//PHARM  DD DSN=EMPOT39.TEST.RPTOUT(DATACALC),DISP=SHR                          
//OUTFILE DD DSN=EMPOT39.DATA.OUT(OUTFILE),DISP=SHR                             
//SYSTSOUT DD *                                                                 
//*                                                                             
//STEP3 EXEC PGM=SORT                                                           
//SYSPRINT DD  SYSOUT=*                                                         
//SYSOUT   DD  SYSOUT=*                                                         
//SORTIN   DD  DSN=EMPOT39.HOSPDAT.DATA(T3),DISP=SHR                            
//SORTOUT  DD DSN=EMPOT39.HOSPDAT.DATA(T4),DISP=SHR                             
//SYSIN DD *                                                                    
 SORT  FIELDS=(1,5,CH,A)                                                        
/*                                                                              
//*STEP4    EXEC   PGM=HOSPCALC                                                 
//*STEPLIB DD DSN=EMPOT39.TEST.LOAD,DISP=SHR                                    
//******* ADDITIONAL RUNTIME JCL HERE ******                                    
//*INFILE DD DSN=EMPOT39.HOSPSORT.DATA,DISP=SHR                                 
//*SYSOUT DD DSN=EMPOT39.TEST.COPYLIB(DATACALC),DISP=SHR                        
//*DIAG DD DSN=EMPOT39.TEST.RPTOUT(DATACALC),DISP=SHR                           
//*ERROUT DD DSN=EMPOT39.HOSPCALC.ERROR,DISP=SHR                                
//*SYSTSOUT DD *                                                                
//*                                                                             
//*                                                                             
//CEEOPTS DD *                                                                  
    TEST(,,,TCPIP&9.76.97.236%8001:)                                            
//ERROUT DD DSN=EMPOT39.HOSPERR.DATA,                                           
//             UNIT=SYSDA,DISP=(NEW,CATLG,DELETE),                              
//             SPACE=(TRK,(5,5)),                                               
//             DCB=(RECFM=FB,LRECL=100,BLKSIZE=8000)                            
//OUTFILE DD DSN=EMPOT39.HOSPOUT.DATA,UNIT=SYSDA,                               
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(TRK,(5,5)),                                               
//             DCB=(RECFM=FB,LRECL=100,BLKSIZE=8000)                            
                                                                                
//STEP1    EXEC   PGM=HOSPCRFL                                                  
//STEPLIB DD DSN=EMPOT39.TEST.LOAD,DISP=SHR                                     
//******* ADDITIONAL RUNTIME JCL HERE ******                                    
//SYSOUT DD DSN=EMPOT39.TEST.COPYLIB(SYSOUT),DISP=SHR                           
//AMOUNT DD DSN=EMPOT39.HOSPIN.DATA,DISP=SHR                                    
//SYSTSOUT DD *                                                                 
