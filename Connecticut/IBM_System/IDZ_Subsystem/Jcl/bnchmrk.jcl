//DDS0001 JOB ,                                                                 
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(1,1),REGION=70M,COND=(16,LT)                 
//*                                                                             
//*EXECUTE PROGRAMS TO UPDATE THE VSAM FILE                                     
//MSTRUPDT EXEC PGM=IKJEFT01,COND=(0,NE)                                        
//STEPLIB   DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                   
            DD DSN=DB2.V9R1.SDSNLOAD,DISP=SHR                                   
            DD DSN=DB2.V9R1.DSNA.RUNLIB.LOAD,DISP=SHR                           
            DD DSN=DB2.V9R1.DSNA.ENFM.SDSNSAMP,DISP=SHR                         
            DD DSN=DB2.V9R1.DSNA.SDSNTEMP,DISP=SHR                              
            DD DSN=DB2.V9R1.DSNA.SDSNEXIT,DISP=SHR                              
//*CEEOPTS DD *                                                                 
//*        TEST(,,,TCPIP&9.65.170.164%8003:)                                    
//STEPLIB DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                     
//SYSPRINT DD SYSOUT=*                                                          
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSOUT   DD SYSOUT=*                                                          
//SYSIN DD DUMMY                                                                
//PATMSTR DD DSN=DDS0001.PATMASTR,DISP=SHR                                      
//********************************************************************          
//* If you're editing this JCL file in the IBM mainframe: zserveros             
//*     Select the dataset(member): DDS0001.TEST.RPTOUT(PATRPT)                 
//*     Right-click, and select: Open Member                                    
//PATRPT  DD DSN=DDS0001.TEST.RPTOUT(PATRPT),DISP=SHR                           
//PATINS  DD DSN=DDS0001.PATINS,DISP=SHR                                        
//DIAGERR DD DSN=DDS0001.PATERR,DISP=SHR                                        
//*PATERR DD DSN=DDS0001.PATERR,DISP=(NEW,CATLG,DELETE),                        
//*   STORCLAS=USRBASE,SPACE=(TRK,(13,2),RLSE),                                 
//*   DCB=(LRECL=1133,BLKSIZE=11330,RECFM=FB,DSORG=PS)                          
//PRSNMSTR DD DSN=DDS0001.PATPERSN,DISP=SHR                                     
//SYSTSIN  DD *                                                                 
  TSOLIB ACTIVATE DA('DB2.V9R1.SDSNLOAD')                                       
  DSN SYSTEM(DSNA)                                                              
  RUN PROGRAM(MSTRUPDT) PLAN(MSTRUPDT) -                                        
  LIB(TEST.LOAD)                                                                
END                                                                             
//*                                                                             
//                                                                              
//*                                                                             
//* STEP 0 - PRE-ALLOCATE ALL OUTPUT FILES                                      
//*                                                                             
//ALLOC EXEC PGM=IEBGENER,COND=(0,NE)                                           
//SYSPRINT DD SYSOUT=*                                                          
//SYSIN DD DUMMY                                                                
//SYSUT1 DD DSN=DDS0001.TRMTEDIT,DISP=(NEW,CATLG,DELETE),                       
//          STORCLAS=USRBASE,SPACE=(TRK,(12,5),RLSE),                           
//          DCB=(LRECL=1101,BLKSIZE=11010,RECFM=FB,DSORG=PS)                    
//SYSUT2 DD DSN=DDS0001.TRMTSORT,DISP=(NEW,CATLG,DELETE),                       
//          STORCLAS=USRBASE,SPACE=(TRK,(12,5),RLSE),                           
//          DCB=(LRECL=1101,BLKSIZE=11010,RECFM=FB,DSORG=PS)                    
//SYSUT3 DD DSN=DDS0001.TRMTSRCH,DISP=(NEW,CATLG,DELETE),                       
//          STORCLAS=USRBASE,SPACE=(TRK,(12,6),RLSE),                           
//          DCB=(LRECL=1101,BLKSIZE=11010,RECFM=FB,DSORG=PS)                    
//SYSUT3 DD DSN=DDS0001.TRMTSRT,DISP=(NEW,CATLG,DELETE),                        
//          STORCLAS=USRBASE,SPACE=(TRK,(12,6),RLSE),                           
//          DCB=(LRECL=1101,BLKSIZE=11010,RECFM=FB,DSORG=PS)                    
//SYSUT4 DD DSN=DDS0001.TRMTCALC,DISP=(NEW,CATLG,DELETE),                       
//          STORCLAS=USRBASE,SPACE=(TRK,(12,6),RLSE),                           
//          DCB=(LRECL=1101,BLKSIZE=11010,RECFM=FB,DSORG=PS)                    
//SYSUT5 DD DSN=DDS0001.PATEDIT,DISP=(NEW,CATLG,DELETE),                        
//          STORCLAS=USRBASE,SPACE=(TRK,(12,6),RLSE),                           
//          DCB=(LRECL=993,BLKSIZE=9930,RECFM=FB,DSORG=PS)                      
//SYSUT6 DD DSN=DDS0001.PATSORT,DISP=(NEW,CATLG,DELETE),                        
//          STORCLAS=USRBASE,SPACE=(TRK,(12,6),RLSE),                           
//          DCB=(LRECL=993,BLKSIZE=9930,RECFM=FB,DSORG=PS)                      
//SYSUT7 DD DSN=DDS0001.PATSRCH,DISP=(NEW,CATLG,DELETE),                        
//          STORCLAS=USRBASE,SPACE=(TRK,(12,6),RLSE),                           
//          DCB=(LRECL=993,BLKSIZE=9930,RECFM=FB,DSORG=PS)                      
//SYSUT8 DD DSN=DDS0001.PATSRTR,DISP=(NEW,CATLG,DELETE),                        
//          STORCLAS=USRBASE,SPACE=(TRK,(12,6),RLSE),                           
//          DCB=(LRECL=993,BLKSIZE=9930,RECFM=FB,DSORG=PS)                      
//SYSUT9 DD DSN=DDS0001.PATSRT2,DISP=(NEW,CATLG,DELETE),                        
//          STORCLAS=USRBASE,SPACE=(TRK,(12,6),RLSE),                           
//          DCB=(LRECL=993,BLKSIZE=9930,RECFM=FB,DSORG=PS)                      
//SYSUT10 DD DSN=DDS0001.WRDSRTR,DISP=(NEW,CATLG,DELETE),                       
//          STORCLAS=USRBASE,SPACE=(TRK,(12,6),RLSE),                           
//          DCB=(LRECL=993,BLKSIZE=9930,RECFM=FB,DSORG=PS)                      
//SYSUT11 DD DSN=DDS0001.PATCALC,DISP=(NEW,CATLG,DELETE),                       
//          STORCLAS=USRBASE,SPACE=(TRK,(12,6),RLSE),                           
//          DCB=(LRECL=993,BLKSIZE=9930,RECFM=FB,DSORG=PS)                      
//SYSUT12 DD DSN=DDS0001.PATRPT,DISP=(NEW,CATLG,DELETE),                        
//          STORCLAS=USRBASE,SPACE=(TRK,(25,5),RLSE),                           
//          DCB=(LRECL=133,BLKSIZE=13300,RECFM=FBA,DSORG=PS)                    
//SYSUT13 DD DSN=DDS0001.WARDRPT,DISP=(NEW,CATLG,DELETE),                       
//          STORCLAS=USRBASE,SPACE=(TRK,(25,5),RLSE),                           
//          DCB=(LRECL=133,BLKSIZE=13300,RECFM=FBA,DSORG=PS)                    
//SYSUT14 DD DSN=DDS0001.TRMEDERR,DISP=(NEW,CATLG,DELETE),                      
//          STORCLAS=USRBASE,SPACE=(TRK,(13,2),RLSE),                           
//          DCB=(LRECL=1141,BLKSIZE=11410,RECFM=FB,DSORG=PS)                    
//SYSUT15 DD DSN=DDS0001.PATRPERR,DISP=(NEW,CATLG,DELETE),                      
//          STORCLAS=USRBASE,SPACE=(TRK,(13,2),RLSE),                           
//          DCB=(LRECL=1141,BLKSIZE=11410,RECFM=FB,DSORG=PS)                    
//SYSUT16 DD DSN=DDS0001.PATERR,DISP=(NEW,CATLG,DELETE),                        
//          STORCLAS=USRBASE,SPACE=(TRK,(13,2),RLSE),                           
//          DCB=(LRECL=1133,BLKSIZE=11330,RECFM=FB,DSORG=PS)                    
//SYSIN DD DUMMY                                                                
//*                                                                             
//* STEP 1.EDIT THE DAILY PATIENT TREATMENTS TRANSACTION FILE                   
//*                                                                             
//TRTMNT EXEC PGM=IKJEFT01,DYNAMNBR=20                                          
//STEPLIB   DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                   
            DD DSN=DB2.V9R1.SDSNLOAD,DISP=SHR                                   
            DD DSN=DB2.V9R1.DSNA.RUNLIB.LOAD,DISP=SHR                           
            DD DSN=DB2.V9R1.DSNA.ENFM.SDSNSAMP,DISP=SHR                         
            DD DSN=DB2.V9R1.DSNA.SDSNTEMP,DISP=SHR                              
            DD DSN=DB2.V9R1.DSNA.SDSNEXIT,DISP=SHR                              
//*CEEOPTS DD *                                                                 
//*         TEST(,,,TCPIP&9.65.184.238%8003:)                                   
//SYSPRINT  DD SYSOUT=*                                                         
//SYSOUT    DD SYSOUT=*                                                         
//TRMTDATA  DD DSN=DDS0001.TRMTDATA,DISP=SHR                                    
//TRMTEDIT  DD DSN=DDS0001.TRMTEDIT,DISP=SHR                                    
//TRMTERR   DD DSN=DDS0001.TRMEDERR,DISP=SHR                                    
//SYSTSPRT  DD SYSOUT=*                                                         
//PATMSTR   DD DSN=DDS0001.PATMASTR,DISP=SHR                                    
//SYSTSIN   DD *                                                                
  TSOLIB ACTIVATE DA('DB2.V9R1.SDSNLOAD')                                       
  DSN SYSTEM(DSNA)                                                              
  RUN PROGRAM(TRTMNT) PLAN(TRTMNT) -                                            
  LIB(TEST.LOAD)                                                                
END                                                                             
/*                                                                              
//*                                                                             
//*                                                                             
//* STEP 2. SORT OF THE DAILY EDITED TREATMENT FILE                             
//*                                                                             
//TRMTSRT EXEC PGM=SORT                                                         
//SYSPRINT DD  SYSOUT=*                                                         
//SYSOUT   DD  SYSOUT=*                                                         
//SORTIN   DD  DSN=DDS0001.TRMTEDIT,DISP=SHR                                    
//SORTOUT  DD DSN=DDS0001.TRMTSORT,DISP=SHR                                     
//SYSIN DD *                                                                    
 SORT  FIELDS=(1,7,CH,A)                                                        
/*                                                                              
//* ALLOCATE ALL OUTPUT FILES BEFORE JOB RUNS                                   
//* Do we need "xxxCALC" files?                                                 
//*                                                                             
//* STEP 3. COBOL TABLE SEARCH                                                  
//*                                                                             
//*EXECUTE PROGRAMS TO APPLY TABLE UPDATES TO THE EDITED TREATMENT FILE         
//TRTSRCH EXEC PGM=TRMTSRCH,COND=(0,NE)                                         
//STEPLIB DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                     
//SYSPRINT DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//TRMTSRCH DD DSN=DDS0001.TRMTSRCH,DISP=SHR                                     
//TRMTSORT DD DSN=DDS0001.TRMTSORT,DISP=SHR                                     
//LABTEST DD DSN=DDS0001.LABTEST,DISP=SHR                                       
//SYSIN DD DUMMY                                                                
//PATMSTR DD DSN=DDS0001.PATMASTR,DISP=SHR                                      
//*                                                                             
//* STEP 4. UPDATE MASTER FILE(PATMASTR) WITH PATIENT TREATMENT RECORDS         
//*                                                                             
//TRMTUPDT EXEC PGM=TRMTUPDT,COND=(0,NE)                                        
//STEPLIB DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                     
//*CEEOPTS DD *                                                                 
//*          TEST(,,,TCPIP&9.65.191.197%8003:)                                  
//SYSPRINT DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//TRMTSRCH DD DSN=DDS0001.TRMTSRCH,DISP=SHR                                     
//SYSIN DD DUMMY                                                                
//PATMSTR DD DSN=DDS0001.PATMASTR,DISP=SHR                                      
//*                                                                             
//* STEP 5. DAILY PATIENT ROOM & EQUIPMENT CHARGES FILE EDITS                   
//*                                                                             
//PATEDIT EXEC PGM=IKJEFT01,DYNAMNBR=20                                         
//STEPLIB   DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                   
            DD DSN=DB2.V9R1.SDSNLOAD,DISP=SHR                                   
            DD DSN=DB2.V9R1.DSNA.RUNLIB.LOAD,DISP=SHR                           
            DD DSN=DB2.V9R1.DSNA.ENFM.SDSNSAMP,DISP=SHR                         
            DD DSN=DB2.V9R1.DSNA.SDSNTEMP,DISP=SHR                              
            DD DSN=DB2.V9R1.DSNA.SDSNEXIT,DISP=SHR                              
//*CEEOPTS DD *                                                                 
//*         TEST(,,,TCPIP&9.76.107.152%8003:)                                   
//SYSPRINT DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//PATDATA DD DSN=DDS0001.PATDATA,DISP=SHR                                       
//PATEDIT DD DSN=DDS0001.PATEDIT,DISP=SHR                                       
//PATERR  DD DSN=DDS0001.PATERR,DISP=SHR                                        
//SYSTSPRT DD SYSOUT=*                                                          
//PATMSTR  DD DSN=DDS0001.PATMASTR,DISP=SHR                                     
//SYSTSIN  DD *                                                                 
  TSOLIB ACTIVATE DA('DB2.V9R1.SDSNLOAD')                                       
  DSN SYSTEM(DSNA)                                                              
  RUN PROGRAM(DALYEDIT) PLAN(DALYEDIT) -                                        
  LIB(TEST.LOAD)                                                                
END                                                                             
/*                                                                              
//*                                                                             
//* STEP 6. SORT OF THE DAILY EDITED PATIENT ROOM CHARGES FILE                  
//*                                                                             
//PATDSRT EXEC PGM=SORT                                                         
//SYSPRINT DD  SYSOUT=*                                                         
//SYSOUT   DD  SYSOUT=*                                                         
//SORTIN   DD  DSN=DDS0001.PATEDIT,DISP=SHR                                     
//SORTOUT  DD DSN=DDS0001.PATSORT,DISP=SHR                                      
//SYSIN DD *                                                                    
 SORT  FIELDS=(1,7,CH,A)                                                        
/*                                                                              
//*                                                                             
//* STEP 7. SEARCH AND UPDATE PATIENT/RM FILE WITH INTERNAL TABLE DATA          
//*                                                                             
//*EXECUTE PROGRAMS TO APPLY TABLE UPDATES TO THE EDITED PATIENT FILE           
//PATSRCH EXEC PGM=PATSRCH,COND=(0,NE)                                          
//STEPLIB DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                     
//SYSPRINT DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//PATSRCH DD DSN=DDS0001.PATSRCH,DISP=SHR                                       
//PATSORT DD DSN=DDS0001.PATSORT,DISP=SHR                                       
//EQUIP DD DSN=DDS0001.EQUIP,DISP=SHR                                           
//SYSIN DD DUMMY                                                                
//PATMSTR DD DSN=DDS0001.PATMASTR,DISP=SHR                                      
//*                                                                             
//*                                                                             
//* STEP 8. UPDATE MASTER FILE WITH PATIENT ROOM/EQUIPMENT CHARGES              
//*                                                                             
//*EXECUTE PROGRAMS TO UPDATE TO THE VSAM MASTER FILE WITH TREATMENTS           
//DALYUPDT EXEC PGM=DALYUPDT,COND=(0,NE)                                        
//STEPLIB DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                     
//*CEEOPTS DD *             u                                                   
//*          TEST(,,,TCPIP&9.49.178.107%8003:)                                  
//*CEEOPTS DD *                                                                 
//*          TEST(,,,TCPIP&9.65.184.238%8003:)                                  
//SYSPRINT DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//PATSRCH DD DSN=DDS0001.PATSRCH,DISP=SHR                                       
//SYSIN DD DUMMY                                                                
//PATMSTR DD DSN=DDS0001.PATMASTR,DISP=SHR                                      
//*                                                                             
//*                                                                             
//* STEP 9. RECALC MASTER FILE RECORDS FOR ALL PATIENTS                         
//*                                                                             
//*EXECUTE PROGRAMS TO UPDATE THE VSAM FILE                                     
//MSTRUPDT EXEC PGM=IKJEFT01,COND=(0,NE)                                        
//STEPLIB   DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                   
            DD DSN=DB2.V9R1.SDSNLOAD,DISP=SHR                                   
            DD DSN=DB2.V9R1.DSNA.RUNLIB.LOAD,DISP=SHR                           
            DD DSN=DB2.V9R1.DSNA.ENFM.SDSNSAMP,DISP=SHR                         
            DD DSN=DB2.V9R1.DSNA.SDSNTEMP,DISP=SHR                              
            DD DSN=DB2.V9R1.DSNA.SDSNEXIT,DISP=SHR                              
//*CEEOPTS DD *                                                                 
//*       TEST(,,,TCPIP&9.65.159.179%8003:)                                     
//STEPLIB DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                     
//SYSPRINT DD SYSOUT=*                                                          
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSOUT   DD SYSOUT=*                                                          
//SYSIN DD DUMMY                                                                
//PATMSTR DD DSN=DDS0001.PATMASTR,DISP=SHR                                      
//*PATRPT DD  DSN=DDS0001.PATRPT,DISP=(NEW,CATLG,DELETE),                       
//*   STORCLAS=USRBASE,SPACE=(TRK,(25,5),RLSE),                                 
//*   DCB=(LRECL=133,BLKSIZE=13300,RECFM=FBA,DSORG=PS)                          
//PATRPT DD DSN=DDS0001.PATRPT,DISP=SHR                                         
//PATINS DD DSN=DDS0001.PATINS,DISP=SHR                                         
//PATERR DD DSN=DDS0001.PATERR,DISP=SHR                                         
//*PATERR DD DSN=DDS0001.PATERR,DISP=(NEW,CATLG,DELETE),                        
//*   STORCLAS=USRBASE,SPACE=(TRK,(13,2),RLSE),                                 
//*   DCB=(LRECL=1133,BLKSIZE=11330,RECFM=FB,DSORG=PS)                          
//PRSNMSTR DD DSN=DDS0001.PATPERSN,DISP=SHR                                     
//SYSTSIN  DD *                                                                 
  TSOLIB ACTIVATE DA('DB2.V9R1.SDSNLOAD')                                       
  DSN SYSTEM(DSNA)                                                              
  RUN PROGRAM(MSTRUPDT) PLAN(MSTRUPDT) -                                        
  LIB(TEST.LOAD)                                                                
END                                                                             
//*                                                                             
//***** NOTE BATCH COBOL CALLING BATCH DB2...                                   
//*                                                                             
//* STEP 6. SORT OF THE DAILY EDITED PATIENT ROOM CHARGES FILE                  
//*                                                                             
//PATRPSRT EXEC PGM=SORT                                                        
//SYSPRINT DD  SYSOUT=*                                                         
//SYSOUT   DD  SYSOUT=*                                                         
//SORTIN   DD  DSN=DDS0001.PATSRCH,DISP=SHR                                     
//SORTOUT  DD DSN=DDS0001.PATSRTR,DISP=SHR                                      
//SYSIN DD *                                                                    
 SORT  FIELDS=(1,7,CH,A)                                                        
/*                                                                              
//*                                                                             
//*                                                                             
//* STEP 6. SORT OF THE DAILY EDITED PATIENT ROOM CHARGES FILE                  
//*                                                                             
//TRMRPSRT EXEC PGM=SORT                                                        
//SYSPRINT DD  SYSOUT=*                                                         
//SYSOUT   DD  SYSOUT=*                                                         
//SORTIN   DD  DSN=DDS0001.TRMTSRCH,DISP=SHR                                    
//SORTOUT  DD DSN=DDS0001.TRMTSRT,DISP=SHR                                      
//SYSIN DD *                                                                    
 SORT  FIELDS=(1,7,CH,A)                                                        
/*                                                                              
//*                                                                             
//*EXECUTE PROGRAM TO WRITE OUT THE PATIENT DETAILED LISTING (REPORT)           
//*                                                                             
//PATRPT EXEC PGM=PATLIST,COND=(0,NE)                                           
//STEPLIB DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                     
//*CEEOPTS DD *                                                                 
//*         TEST(,,,TCPIP&9.76.105.21%8003:)                                    
//SYSPRINT DD SYSOUT=*                                                          
//TRMTSRCH DD DSN=DDS0001.TRMTSRT,DISP=SHR                                      
//PATSRCH DD DSN=DDS0001.PATSRTR,DISP=SHR                                       
//PATRPT DD DSN=DDS0001.PATRPT,DISP=SHR                                         
//PATERR DD DSN=DDS0001.PATERR,DISP=SHR                                         
//TRMTERR DD DSN=DDS0001.PATRPERR,DISP=SHR                                      
//SYSIN DD DUMMY                                                                
//PATMSTR DD DSN=DDS0001.PATMASTR,DISP=SHR                                      
//PATINS DD DSN=DDS0001.PATINS,DISP=SHR                                         
//PATPERSN DD DSN=DDS0001.PATPERSN,DISP=SHR                                     
//*                                                                             
//*RE-SORT THE DAILY EDITED PATIENT ROOM CHARGES FILE FOR REPORTS               
//*                                                                             
//PATRSRT2 EXEC PGM=SORT                                                        
//SYSPRINT DD  SYSOUT=*                                                         
//SYSOUT   DD  SYSOUT=*                                                         
//SORTIN   DD  DSN=DDS0001.PATSRCH,DISP=SHR                                     
//SORTOUT  DD DSN=DDS0001.PATSRT2,DISP=SHR                                      
//SYSIN DD *                                                                    
 SORT  FIELDS=(63,4,CH,A,20,4,CH,A,16,4,CH,A)                                   
/*                                                                              
//******* ADDITIONAL JCL FOR LINK HERE ******                                   
//WARDRPT EXEC PGM=IKJEFT01,DYNAMNBR=20                                         
//STEPLIB   DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                   
            DD DSN=DB2.V9R1.SDSNLOAD,DISP=SHR                                   
            DD DSN=DB2.V9R1.DSNA.RUNLIB.LOAD,DISP=SHR                           
            DD DSN=DB2.V9R1.DSNA.ENFM.SDSNSAMP,DISP=SHR                         
            DD DSN=DB2.V9R1.DSNA.SDSNTEMP,DISP=SHR                              
            DD DSN=DB2.V9R1.DSNA.SDSNEXIT,DISP=SHR                              
//*CEEOPTS DD *                                                                 
//*    TEST(,,,TCPIP&9.65.143.254%8003:)                                        
//SYSPRINT  DD SYSOUT=*                                                         
//SYSOUT    DD SYSOUT=*                                                         
//PATSRCH   DD DSN=DDS0001.PATSRT2,DISP=SHR                                     
//WARDRPT   DD DSN=DDS0001.WARDRPT,DISP=SHR                                     
//PATERR    DD DSN=DDS0001.PATERR,DISP=SHR                                      
//SYSTSPRT  DD SYSOUT=*                                                         
//PATMSTR   DD DSN=DDS0001.PATMASTR,DISP=SHR                                    
//PATPERSN  DD DSN=DDS0001.PATPERSN,DISP=SHR                                    
//SYSTSIN  DD *                                                                 
  TSOLIB ACTIVATE DA('DB2.V9R1.SDSNLOAD')                                       
  DSN SYSTEM(DSNA)                                                              
  RUN PROGRAM(WARDRPT) PLAN(WARDRPT) -                                          
  LIB(TEST.LOAD)                                                                
END                                                                             
/*                                                                              
//                                                                              
//* END OF BENCHMARKS BATCH JOB STREAM                                          
//*                                                                             
//*EXECUTE PROGRAMS TO UPDATE THE VSAM FILE                                     
//MSTRUPDT EXEC PGM=IKJEFT01,COND=(0,NE)                                        
//STEPLIB   DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                   
            DD DSN=DB2.V9R1.SDSNLOAD,DISP=SHR                                   
            DD DSN=DB2.V9R1.DSNA.RUNLIB.LOAD,DISP=SHR                           
            DD DSN=DB2.V9R1.DSNA.ENFM.SDSNSAMP,DISP=SHR                         
            DD DSN=DB2.V9R1.DSNA.SDSNTEMP,DISP=SHR                              
            DD DSN=DB2.V9R1.DSNA.SDSNEXIT,DISP=SHR                              
//CEEOPTS DD *                                                                  
       TEST(,,,TCPIP&9.49.187.135%8003:)                                        
//STEPLIB DD DSN=DDS0001.TEST.LOAD,DISP=SHR                                     
//SYSPRINT DD SYSOUT=*                                                          
//SYSTSPRT  DD SYSOUT=*                                                         
//SYSOUT   DD DSN=DDS0001.TEST.JCL(MYSYSOT),DISP=SHR                            
//SYSIN DD DUMMY                                                                
//PATMSTR DD DSN=DDS0001.PATMASTR,DISP=SHR                                      
//*PATRPT DD  DSN=DDS0001.PATRPT,DISP=(NEW,CATLG,DELETE),                       
//*   STORCLAS=USRBASE,SPACE=(TRK,(25,5),RLSE),                                 
//*   DCB=(LRECL=133,BLKSIZE=13300,RECFM=FBA,DSORG=PS)                          
//PATRPT DD DSN=DDS0001.PATRPT,DISP=SHR                                         
//PATINS DD DSN=DDS0001.PATINS,DISP=SHR                                         
//PATERR DD DSN=DDS0001.PATERR,DISP=SHR                                         
//*PATERR DD DSN=DDS0001.PATERR,DISP=(NEW,CATLG,DELETE),                        
//*   STORCLAS=USRBASE,SPACE=(TRK,(13,2),RLSE),                                 
//*   DCB=(LRECL=1133,BLKSIZE=11330,RECFM=FB,DSORG=PS)                          
//PRSNMSTR DD DSN=DDS0001.PATPERSN,DISP=SHR                                     
//SYSTSIN  DD *                                                                 
  TSOLIB ACTIVATE DA('DB2.V9R1.SDSNLOAD')                                       
  DSN SYSTEM(DSNA)                                                              
  RUN PROGRAM(MSTRUPDT) PLAN(TESTDBW) -                                         
  LIB(TEST.LOAD)                                                                
END                                                                             
//                                                                              
