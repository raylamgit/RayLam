//DDS0001A JOB REGION=4M,                                                       
// TIME=(1),MSGCLASS=H,NOTIFY=DDS0001,MSGLEVEL=(1,1)                            
//********************************************************************  00030000
//*   COBOL COMPILE                                                     00040000
//********************************************************************  00140000
//COBCOMP  EXEC PGM=IGYCRCTL,                                                   
//        PARM='TEST(NONE,SYM,SEPARATE),LIST,MAP,SOURCE,XREF,LIB,DYNAM,         
//             NORENT,NOOPT'                                                    
//STEPLIB  DD DISP=SHR,DSN=COBOL.V3R4.SIGYCOMP                                  
//SYSIN    DD DISP=SHR,DSN=DDS0001.TEST.COBOL(SAM1)                             
//SYSLIB   DD DISP=SHR,DSN=DDS0001.TEST.COPYLIB                                 
//         DD DISP=SHR,DSN=DDS0001.TEST.COBOL                                   
//SYSPRINT DD DISP=SHR,DSN=DDS0001.TEST.LISTING(SAM1)                           
//SYSDEBUG DD DISP=SHR,DSN=DDS0001.TEST.SYSDEBUG(SAM1)                          
//SYSLIN   DD DISP=(MOD,PASS),DSN=&&LOADSET,UNIT=SYSALLDA,                      
//            SPACE=(80,(10,10))                                                
//SYSUDUMP DD SYSOUT=*                                                          
//SYSUT1   DD SPACE=(80,(10,10),,,ROUND),UNIT=SYSALLDA                          
//SYSUT2   DD SPACE=(80,(10,10),,,ROUND),UNIT=SYSALLDA                          
//SYSUT3   DD SPACE=(80,(10,10),,,ROUND),UNIT=SYSALLDA                          
//SYSUT4   DD SPACE=(80,(10,10),,,ROUND),UNIT=SYSALLDA                          
//SYSUT5   DD SPACE=(80,(10,10),,,ROUND),UNIT=SYSALLDA                          
//SYSUT6   DD SPACE=(80,(10,10),,,ROUND),UNIT=SYSALLDA                          
//SYSUT7   DD SPACE=(80,(10,10),,,ROUND),UNIT=SYSALLDA                          
//*                                                                             
//LKED EXEC PGM=IEWL,COND=(5,LT,COBCOMP),                                       
//          PARM='LIST,XREF'                                                    
//SYSLIB   DD DISP=SHR,DSN=CEE.SCEELKED                                         
//SYSLMOD  DD DSN=DDS0001.TEST.LOAD(SAM1),DISP=SHR                              
//SYSLIN   DD DISP=(OLD,DELETE),DSN=&&LOADSET                                   
//         DD DDNAME=SYSIN                                                      
//SYSPRINT DD SYSOUT=*                                                          
//SYSUT1   DD UNIT=SYSALLDA,DCB=BLKSIZE=1024,                                   
//            SPACE=(1024,(200,20))                                             
//                                                                              
