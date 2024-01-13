      *****************************************************************         
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. CURSRAVG.                                                    
      *REMARKS.    THIS PROGRAM JOINS TABLES, GROUPS DATA BY DEPT,              
      *     AND DISPLAYS THE AVERAGE, MAXIMUM AND MINIMUM                       
      *     HOURS, AND PERFORMANCE EVALUATION BY DEPT.                          
      *                                                                         
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SOURCE-COMPUTER. IBM-370.                                                
       OBJECT-COMPUTER. IBM-370.                                                
                                                                                
       DATA DIVISION.                                                           
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
      * CODE THE NECESSARY DB2 INCLUDE STATEMENTS HERE                          
       01  WS-KTRS-SWITCHES.                                                    
           05 ROW-KTR    PIC S9(03) COMP-3 VALUE +0.                            
                                                                                
      * MODIFY THE TABLE-ROW PICTURE CLAUSES FOR THE HOST                       
      * PROGRAM VARIABLES - LOOK AT THE TABLE/COLUMN DEFINITIONS                
      *         IN YOUR MANUAL (APPENDIX A FROM THE SQL PORTION)                
      *         AND THE SQL TO COBOL DATATYPES IN CHAPTER 3                     
      *                                                                         
      * YOU WILL NEED TWO NULL INDICATORS - BASED ON POTENTIAL NULL             
      * VALUES IN THE EMPL TABLE; ONE FOR DEPT, AND ONE FOR PERF.               
      * QUESTION . . . WHY DON'T YOU NEED A DIFFERENT NULL VALUE                
      * FOR MIN, MAX AND AVG(PERF) ???                                          
                                                                                
            EXEC SQL INCLUDE SQLCA END-EXEC.                                    
                                                                                
       01  TABLE-ROW.                                                           
           05 DEPT-TBL       PIC  X(3).                                         
           05 PERF-TBL-AVG   PIC  S9(05)V99 COMP-3.                             
           05 PERF-TBL-MIN   PIC  S9(04)    COMP.                               
           05 PERF-TBL-MAX   PIC  S9(04)    COMP.                               
           05 HOURS-TBL-AVG  PIC  S9(05)V99 COMP-3.                             
           05 HOURS-TBL-MAX  PIC  S9(05)V99 COMP-3.                             
           05 HOURS-TBL-MIN  PIC  S9(05)V99 COMP-3.                             
                                                                                
       01  OUTPUT-ROW.                                                          
           05 FILLER         PIC X(01) VALUE SPACES.                            
           05 DEPT-RPT       PIC X(03).                                         
           05 FILLER         PIC X(03) VALUE SPACES.                            
           05 PERF-RPT-AVG   PIC Z(03).99.                                      
           05 FILLER         PIC X(01) VALUE SPACES.                            
           05 PERF-RPT-MIN   PIC Z(03).99.                                      
           05 FILLER         PIC X(01) VALUE SPACES.                            
           05 PERF-RPT-MAX   PIC Z(03).99.                                      
           05 FILLER         PIC X(03) VALUE SPACES.                            
           05 HOURS-RPT-AVG  PIC Z(03).99.                                      
           05 FILLER         PIC X(03) VALUE SPACES.                            
           05 HOURS-RPT-MAX  PIC Z(03).99.                                      
           05 FILLER         PIC X(03) VALUE SPACES.                            
           05 HOURS-RPT-MIN  PIC Z(03).99.                                      
                                                                                
       01  NULL-GROUP.                                                          
           05 DEPT-NULL      PIC  S9(04) COMP.                                  
           05 PERF-NULL      PIC  S9(04) COMP.                                  
                                                                                
       01  ROW-MSG.                                                             
           05 FILLER         PIC X(24)                                          
               VALUE '* * *      ROWS READ -->'.                                
           05 ROW-STAT       PIC Z99.                                           
                                                                                
      *  ERROR MSG AREA FOR CALLS TO DSNTIAR - WHICH DECODES YOUR               
      *  SQL RETURN CODES (SQLCODE) FOR YOU.                                    
                                                                                
       01  ERROR-MSG.                                                           
           05 ERROR-LEN      PIC S9(04) COMP VALUE +960.                        
           05 ERROR-TEXT     PIC X(120) OCCURS 8 TIMES                          
           INDEXED BY ERR-IDX.                                                  
       01  ERROR-TEXT-LEN    PIC S9(09) COMP VALUE +120.                        
                                                                                
       PROCEDURE DIVISION.                                                      
                                                                                
       000-SETUP-ERROR-TRAP-RTN.                                                
                                                                                
      *  THIS PORTION OF THE PROGRAM ACTIVATES THE SQL ERROR TRAPPING           
      *  FACILITIES.  AT PRE-COMPILE TIME, THE DB2 PRE-COMPILER                 
      *  GENERATES COBOL INSTRUCTIONS TO INTERROGATE THE SQLCODE                
      *  (RETURN CODE) FROM EACH CALL. IF A SQLERROR CONDITION IS               
      *  DETECTED (NEGATIVE RETURN CODE), EXECUTION WILL BRANCH TO THE          
      *  999-ERROR-TRAP-RTN TO DISPLAY AN APPROPRIATE ERROR MSG.                
                                                                                
      * SET UP YOUR ERROR HANDLING ROUTINES                                     
                                                                                
       000-MAINLINE-RTN.                                                        
                                                                                
      * THE MAINLINE CONTAINS THE DRIVER CODE TO PERFORM OUR DATA               
      * BASE ACCESS AND DISPLAY ROUTINES.                                       
                                                                                
           PERFORM 100-DECLARE-CURSOR-RTN THRU 100-EXIT.                        
                                                                                
           PERFORM 150-OPEN-CURSOR-RTN THRU 150-EXIT.                           
                                                                                
           PERFORM 200-FETCH-RTN THRU 200-EXIT                                  
              UNTIL SQLCODE = +100.                                             
                                                                                
           PERFORM 300-CLOSE-CURSOR-RTN THRU 300-EXIT.                          
                                                                                
           PERFORM 350-TERMINATE-RTN THRU 350-EXIT.                             
                                                                                
           MOVE ZERO TO RETURN-CODE.                                            
           GOBACK.                                                              
                                                                                
                                                                                
       000-EXIT.                                                                
           EXIT.                                                                
                                                                                
                                                                                
       100-DECLARE-CURSOR-RTN.                                                  
                                                                                
      *  THIS STATEMENT CREATES AN "ACTIVE SET", A GROUP OF ROWS                
      *  THAT WOULD BE THE OUTPUT FROM THE EXECUTION OF THE STATEMENT           
      *  IF YOU EXECUTED IT INTERACTIVELY.                                      
                                                                                
                                                                                
      * = = = > CODE THE SQL STATEMENT TO JOIN THE EMPL AND PAY TABLES          
      * = = = > GROUP THEM BE EMPL.DEPT AND DISPLAY THE DEPT AND:               
      * = = = > AVERAGE, MINIMUM AND MAXIMUM - HOURS AND PERF BY DEPT           
                                                                                
               EXEC SQL                                                         
                   DECLARE C1 CURSOR FOR                                        
                   SELECT DEPT, MIN(PERF), MAX(PERF), AVG(PERF),                
                                MIN(HOURS), MAX(HOURS), AVG(HOURS)              
                          FROM DDS0001.EMPL E, DDS0001.PAY P                    
                          WHERE E.NBR = P.NBR                                   
                          GROUP BY DEPT                                         
               END-EXEC.                                                        
                                                                                
       100-EXIT.                                                                
           EXIT.                                                                
                                                                                
       150-OPEN-CURSOR-RTN.                                                     
                                                                                
      *  THIS STATEMENT OPENS THE "ACTIVE SET" IN PREPARATION OF                
      *  ROW FETCH PROCESSING.                                                  
                                                                                
               EXEC SQL OPEN C1                                                 
                        END-EXEC.                                               
                                                                                
       150-EXIT.                                                                
           EXIT.                                                                
                                                                                
       200-FETCH-RTN.                                                           
                                                                                
      *  THIS PARAGRAPH SETS UP THE SQL PARAMETERS, PERFORMS THE                
      *  PARAGRAPH TO FETCH THE ROW, AND DISPLAYS THE RESULTS.                  
      *  ----> HINT <--- USE ISPF EXCLUDE (XX) OR BLOCK COPY                    
      *  FROM YOUR CURSOR DECLARE STATEMENT TO VERIFY PROPER                    
      *  SELECTED TABLE/COLUMN TO FETCHED HOST-VARIABLE MATCHING                
                                                                                
            PERFORM 250-FETCH-A-ROW THRU 250-EXIT.                              
                                                                                
            IF SQLCODE = ZERO                                                   
            THEN                                                                
             MOVE DEPT-TBL         TO DEPT-RPT                                  
             MOVE PERF-TBL-AVG     TO PERF-RPT-AVG                              
             MOVE PERF-TBL-MIN     TO PERF-RPT-MIN                              
             MOVE PERF-TBL-MAX     TO PERF-RPT-MAX                              
             MOVE HOURS-TBL-AVG    TO HOURS-RPT-AVG                             
             MOVE HOURS-TBL-MAX    TO HOURS-RPT-MAX                             
             MOVE HOURS-TBL-MIN    TO HOURS-RPT-MIN                             
                                                                                
             DISPLAY OUTPUT-ROW                                                 
                ELSE                                                            
             DISPLAY '*** END - OF - DATA ***'.                                 
                                                                                
       200-EXIT.                                                                
           EXIT.                                                                
                                                                                
       250-FETCH-A-ROW.                                                         
                                                                                
      *  THIS PARAGRAPH FETCHES A ROW FROM THE EMPL AND PAY TABLES              
      *  AND MOVES SPECIFIC DATA FIELDS INTO THE AVG, MIN AND MAX FIELDS        
                                                                                
               EXEC SQL FETCH C1 INTO                                           
                        :DEPT-TBL:DEPT-NULL,                                    
                        :PERF-TBL-MIN:PERF-NULL,                                
                        :PERF-TBL-MAX:PERF-NULL,                                
                        :PERF-TBL-AVG:PERF-NULL,                                
                        :HOURS-TBL-MIN,                                         
                        :HOURS-TBL-MAX,                                         
                        :HOURS-TBL-AVG,                                         
                                                                                
               END-EXEC.                                                        
                                                                                
      *  COMPLETE THE LOGIC BELOW TO HANDLE THE NULL CONDITION FOR YOUR         
      *  REPORT - WHY IS THIS NECESSARY?                                        
                                                                                
           IF DEPT-NULL < 0                                                     
           THEN                                                                 
               MOVE 'N/A' TO DEPT-TBL.                                          
                                                                                
           IF PERF-NULL < 0                                                     
           THEN                                                                 
               MOVE 0  TO PERF-TBL-MIN                                          
               MOVE 0  TO PERF-TBL-MAX                                          
               MOVE 0  TO PERF-TBL-AVG.                                         
                                                                                
       250-EXIT.                                                                
           EXIT.                                                                
                                                                                
       300-CLOSE-CURSOR-RTN.                                                    
                                                                                
      *  THIS STATEMENT CLOSES THE "ACTIVE SET"                                 
                                                                                
               EXEC SQL CLOSE C1 END-EXEC.                                      
                                                                                
       300-EXIT.                                                                
           EXIT.                                                                
                                                                                
                                                                                
       350-TERMINATE-RTN.                                                       
                                                                                
           MOVE ROW-KTR TO ROW-STAT.                                            
                                                                                
           DISPLAY ROW-MSG.                                                     
                                                                                
       350-EXIT.                                                                
           EXIT.                                                                
                                                                                
       999-ERROR-TRAP-RTN.                                                      
      ************************************************************              
      *       ERROR TRAPPING ROUTINE FOR NEGATIVE SQLCODES       *              
      ************************************************************              
                                                                                
            DISPLAY '**** WE HAVE A SERIOUS PROBLEM HERE *****'.                
            DISPLAY '999-ERROR-TRAP-RTN '.                                      
            MULTIPLY SQLCODE BY -1 GIVING SQLCODE.                              
            DISPLAY 'SQLCODE ==> ' SQLCODE.                                     
            DISPLAY SQLCA.                                                      
            DISPLAY SQLERRM.                                                    
            EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.                       
            EXEC SQL ROLLBACK WORK END-EXEC.                                    
            GOBACK.                                                             
       999-EXIT.                                                                
           EXIT.                                                                
