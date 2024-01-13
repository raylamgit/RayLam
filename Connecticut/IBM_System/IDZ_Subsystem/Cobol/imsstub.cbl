CBL APOST,NOOPT,DYNAM,SSRANGE,RENT,DATA(24)                                     
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    IMSSTUB.                                                  
       INSTALLATION.  IBM - SANTA TERESA LABORATORY.                            
       SECURITY.      PROPERTY OF IBM CORPORATION.                              
       ENVIRONMENT DIVISION.                                                    
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
           05 CARD-RECORD           PIC X(80).                                  
       WORKING-STORAGE SECTION.                                                 
       01 CARD-STRING.                                                          
           05 CARD-LEN              PIC S9(4) COMP VALUE +0.                    
           05 CARD-DATA             PIC X(80)      VALUE SPACES.                
       LINKAGE SECTION.                                                         
       01 PARM-STRING.                                                          
           05 PARM-LEN              PIC S9(4) COMP.                             
           05 PARM-DATA             PIC X                                       
               OCCURS 100 TIMES DEPENDING ON PARM-LEN.                          
       PROCEDURE DIVISION USING PARM-STRING.                                    
           IF PARM-LEN > 0                                                      
             CALL 'DFSRRC00' USING PARM-STRING                                  
           ELSE                                                                 
             OPEN INPUT PARMFILE                                                
             READ PARMFILE                                                      
             UNSTRING CARDREC DELIMITED BY SPACE INTO CARD-DATA                 
               COUNT IN CARD-LEN                                                
             CLOSE PARMFILE                                                     
             CALL 'DFSRRC00' USING CARD-STRING                                  
           END-IF.                                                              
           GOBACK.                                                              
