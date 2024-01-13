CBL APOST,NOOPT,DYNAM,SSRANGE,RENT,DATA(24)                                     
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    IMSSTUB.                                                  
       INSTALLATION.  IBM - SANTA TERESA LABORATORY.                            
       SECURITY.      PROPERTY OF IBM CORPORATION.                              
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
          SOURCE-COMPUTER.  IBM-370.                                            
          OBJECT-COMPUTER.  IBM-370.                                            
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
           SELECT PARMFILE ASSIGN TO PARMFILE.                                  
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
       FD  PARMFILE                                                             
           LABEL RECORDS ARE OMITTED                                            
           RECORDING MODE IS F                                                  
           BLOCK CONTAINS 0 RECORDS                                             
           DATA RECORD IS CARDREC.                                              
       01  CARDREC.                                                             
           05 CARD-IN               PIC X OCCURS 80 TIMES.                      
       WORKING-STORAGE SECTION.                                                 
       01 CARD-STRING.                                                          
           05 CARD-LEN              PIC S9(4) COMP VALUE 1.                     
           05 CARD-DATA             PIC X                                       
               OCCURS 80 TIMES DEPENDING ON CARD-LEN.                           
       LINKAGE SECTION.                                                         
       01 PARM-STRING.                                                          
           05 PARM-LEN              PIC S9(4) COMP.                             
           05 PARM-DATA             PIC X                                       
               OCCURS 100 TIMES DEPENDING ON PARM-LEN.                          
       PROCEDURE DIVISION USING PARM-STRING.                                    
           IF PARM-LEN > 0 GO TO PARM-USED.                                     
           OPEN INPUT PARMFILE.                                                 
           READ PARMFILE.                                                       
       CARD-LOOP.                                                               
           MOVE CARD-IN(CARD-LEN) TO CARD-DATA(CARD-LEN).                       
           ADD 1 TO CARD-LEN.                                                   
           IF CARD-LEN < 81 AND CARD-IN(CARD-LEN) NOT = SPACES                  
               GO TO CARD-LOOP.                                                 
           SUBTRACT 1 FROM CARD-LEN.                                            
           CLOSE PARMFILE.                                                      
           CALL 'DFSRRC00' USING CARD-STRING.                                   
           GOBACK.                                                              
       PARM-USED.                                                               
           CALL 'DFSRRC00' USING PARM-STRING.                                   
           GOBACK.                                                              
