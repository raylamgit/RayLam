//DDS00011 JOB ,                                                                
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(,4),REGION=70M,COND=(16,LT)                  
//*                                                                             
//STEP0   EXEC   PGM=MSTFILUP                                                   
//STEPLIB DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                     
//******* ADDITIONAL RUNTIME JCL HERE ******                                    
//OLDPARTS DD DSN=DDS0001.OLDPARTS.DATA,DISP=SHR                                
//PRODFILE DD DSN=DDS0001.TEST.PRODFILE,DISP=SHR                                
//IPFILE DD DSN=DDS0001.IPFILE.TRANSIN,DISP=SHR                                 
//ERROUT DD DSN=DDS0001.TRANSERR.ERROROUT,DISP=SHR                              
//CURTRANS DD DSN=DDS0001.MSTFTRNS.DATA,DISP=SHR                                
//NEWPARTS DD DSN=DDS0001.NEWPARTS,DISP=(NEW,CATLG,DELETE),                     
//          STORCLAS=USRBASE,SPACE=(TRK,(12,5),RLSE),                           
//          DCB=(LRECL=1101,BLKSIZE=11010,RECFM=FB,DSORG=PS)                    
//REPORT DD DSN=DDS0001.REPRTOUT,DISP=(NEW,CATLG,DELETE),                       
//          STORCLAS=USRBASE,SPACE=(TRK,(12,5),RLSE),                           
//          DCB=(LRECL=133,BLKSIZE=13300,RECFM=FBA,DSORG=PS)                    
//SORTFILE DD *                                                                 
//*                                                                             
//STEP1    EXEC   PGM=HOSPEDIT                                                  
//STEPLIB DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                     
//******* ADDITIONAL RUNTIME JCL HERE ******                                    
//INFILE DD DSN=DDS0001.NEWPARTS,DISP=SHR                                       
//SYSOUT DD DSN=DDS0001.TEST.COPYLIB(SYSOUT),DISP=SHR                           
//RPTOUT DD DSN=DDS0001.RPTOUT,DISP=(NEW,CATLG,DELETE),                         
//          STORCLAS=USRBASE,SPACE=(TRK,(12,5),RLSE),                           
//          DCB=(LRECL=133,BLKSIZE=13300,RECFM=FBA,DSORG=PS)                    
//ERROUT DD DSN=DDS0001.HOSPOUT.ERROR,DISP=SHR                                  
//OUTFILE DD DSN=DDS0001.HOSPOUT.DATA,DISP=SHR                                  
//SYSTSOUT DD *                                                                 
//*                                                                             
//STEP2    EXEC   PGM=HOSPSRCH                                                  
//STEPLIB DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                     
//******* ADDITIONAL RUNTIME JCL HERE ******                                    
//INFILE DD DSN=DDS0001.HOSPOUT.DATA,DISP=SHR                                   
//SYSOUT DD DSN=DDS0001.TEST.COPYLIB(DATASRCH),DISP=SHR                         
//ERROUT DD DSN=DDS0001.HOSPSRCH.ERROR,DISP=SHR                                 
//OUTFILE DD DSN=DDS0001.HOSPSRCH.DATA,DISP=SHR                                 
//SYSTSOUT DD *                                                                 
//*                                                                             
//STEP3 EXEC PGM=SORT                                                           
//SYSPRINT DD  SYSOUT=*                                                         
//SYSOUT   DD  SYSOUT=*                                                         
//SORTIN   DD  DSN=DDS0001.HOSPOUT.DATA,DISP=SHR                                
//SORTOUT  DD DSN=DDS0001.HOSPSORT.DATA,DISP=SHR                                
//SYSIN DD *                                                                    
 SORT  FIELDS=(1,5,CH,A)                                                        
/*                                                                              
//STEP4    EXEC   PGM=HOSPCALC                                                  
//STEPLIB DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                     
//******* ADDITIONAL RUNTIME JCL HERE ******                                    
//INFILE DD DSN=DDS0001.HOSPSORT.DATA,DISP=SHR                                  
//PRODFILE DD DSN=DDS0001.TEST.PRODFILE,DISP=SHR                                
//SYSOUT DD DSN=DDS0001.TEST.COPYLIB(DATACALC),DISP=SHR                         
//RPTOUT DD DSN=DDS0001.RPTOUT3,DISP=(NEW,CATLG,DELETE),                        
//          STORCLAS=USRBASE,SPACE=(TRK,(12,5),RLSE),                           
//          DCB=(LRECL=133,BLKSIZE=13300,RECFM=FBA,DSORG=PS)                    
//ERROUT DD DSN=DDS0001.HOSPCALC.ERROR,DISP=SHR                                 
//DIAGFILE DD DSN=DDS0001.HOSPCALC.DATA,DISP=SHR                                
//SYSTSOUT DD *                                                                 
//                                                                              
//                                                                              
//*                                                                             
