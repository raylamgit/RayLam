//DDS00ORD JOB REGION=4M,CLASS=A,                                               
// TIME=(1),MSGCLASS=H,NOTIFY=&SYSUID,MSGLEVEL=(1,1)                            
//********************************************************************          
//*   RUN SAMPLE PROGRAM SAM1                                                   
//********************************************************************          
//STEP10 EXEC PGM=ORDUPD                                                        
//STEPLIB  DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                    
//         DD DSN=LNKLST.DEBUG.V12R1.SEQAMOD,DISP=SHR                           
//SYSPRINT DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//ORDERS    DD DSN=DDS0001.ORDERS,DISP=SHR                                      
//SHIPTR    DD DSN=DDS0001.SHIPTR,DISP=SHR                                      
//ERRLIST   DD DSN=DDS0001.ERRLIST,DISP=SHR                                     
//ORDERR    DD DSN=DDS0001.TRMEDERR,DISP=SHR                                    
//RPTTR     DD DSN=DDS0001.RPTTR,DISP=SHR                                       
//PATINS    DD DSN=DDS0001.PATINS,DISP=SHR                                      
//PATMSTR   DD DSN=DDS0001.PATMASTR,DISP=SHR                                    
//TRMTEDIT  DD DSN=DDS0017.BNCHMRK1.TRMTEDIT.WEEKLY(+2),DISP=SHR                
//TRMEDERR  DD DSN=DDS0017.BNCHMRK1.TRMEDERR.WEEKLY(+2),DISP=SHR                
//TRMTSORT  DD DSN=DDS0017.BNCHMRK1.TRMTSORT.WEEKLY(+2),DISP=SHR                
