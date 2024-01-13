   CBL NUMPROC(MIG),FLAG(I,W),RENT                                              
       ID DIVISION.                                                             
       PROGRAM-ID. EPSCSMRT.                                                    
      *    THIS IS A CALLED PROGRAM EXAMPLE FOR DEMONSTRATION                   
      *                                                                         
      *    THIS PROGRAM IS INVOKED VIA A CICS LINK STATMENT                     
      *    AND DYNAMICALLY CALLS THE ACTUAL PROGRAM                             
      *                                                                         
      *    (C) 2017 IBM JIM HILDNER.                                            
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SOURCE-COMPUTER. FLEX-ES.                                                
       OBJECT-COMPUTER. FLEX-ES.                                                
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
      *                                                                         
       01  WS-CALLED-PROGRAM    PIC X(8).                                       
                                                                                
       01  STATIC-CALLED-PROGRAMS.                                              
           03 STATIC-CALLED-PROGRAM-TABLE.                                      
              05 FILLER            PIC X(8) VALUE 'EPSMPMT'.                    
              05 FILLER            PIC X(8) VALUE 'NOT VLD'.                    
              05 FILLER            PIC X(8) VALUE ' '.                          
           03 CALLED-PROGRAM-TABLE                                              
                        REDEFINES STATIC-CALLED-PROGRAM-TABLE                   
                        OCCURS 3 TIMES.                                         
              05 CALLED-PROGRAM-NAME PIC X(8).                                  
                                                                                
       COPY EPSPDATA.                                                           
                                                                                
       LINKAGE SECTION.                                                         
      *                                                                         
       01 DFHCOMMAREA.                                                          
       COPY EPSMTCOM.                                                           
                                                                                
       PROCEDURE DIVISION USING DFHCOMMAREA.                                    
      *                                                                         
       A000-MAINLINE.                                                           
           MOVE EPSPCOM-PRINCIPLE-DATA  TO EPSPDATA-PRINCIPLE-DATA.             
           MOVE EPSPCOM-NUMBER-OF-YEARS TO EPSPDATA-NUMBER-OF-YEARS.            
           MOVE 'Y'                     TO EPSPDATA-YEAR-MONTH-IND.             
           MOVE EPSPCOM-QUOTED-INTEREST-RATE                                    
                                        TO                                      
                                   EPSPDATA-QUOTED-INTEREST-RATE.               
           MOVE CALLED-PROGRAM-NAME(1)  TO WS-CALLED-PROGRAM.                   
           MOVE SPACES                  TO EPSPDATA-RETURN-ERROR.               
      *     CALL 'EPSMPMT' USING EPSPDATA.                                      
           CALL WS-CALLED-PROGRAM USING EPSPDATA.                               
           MOVE EPSPDATA-RETURN-MONTH-PAYMENT                                   
                                        TO                                      
                                        EPSPCOM-RETURN-MONTH-PAYMENT.           
           MOVE EPSPDATA-RETURN-ERROR   TO EPSPCOM-ERRMSG.                      
           IF EPSPDATA-RETURN-ERROR = SPACES                                    
              MOVE ZERO TO EPSPCOM-PROGRAM-RETCODE                              
           ELSE                                                                 
              MOVE 8 TO EPSPCOM-PROGRAM-RETCODE                                 
           END-IF.                                                              
           GOBACK                                                               
           .                                                                    
