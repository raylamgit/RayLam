000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID. MTCHMERG.                                                    
000300 AUTHOR.  IBM.                                                            
000400 INSTALLATION.   BIG OL MAINFRAME.                                        
000500 DATE-WRITTEN.   JULY 1990.                                               
000600******************************************************************        
000700*                                                                         
000800*                                                                         
000900******************************************************************        
001000 ENVIRONMENT DIVISION.                                                    
001100 CONFIGURATION SECTION.                                                   
001200 SOURCE-COMPUTER. IBM-3090.                                               
001300 OBJECT-COMPUTER. IBM-3090.                                               
001400 INPUT-OUTPUT SECTION.                                                    
001500 FILE-CONTROL.                                                            
001600     SELECT OLD-PARTS-MF-IN   ASSIGN TO UT-S-OLDPARTS.                    
001700     SELECT CUR-ACT-FILE-IN   ASSIGN TO UT-S-CURTRANS.                    
001800     SELECT PROD-LINE-FILE-IN ASSIGN TO UT-S-PRODFILE.                    
001900     SELECT SORT-WORK-FILE    ASSIGN TO UT-S-SORTFILE.                    
002000     SELECT NEW-PARTS-MF-OUT  ASSIGN TO UT-S-NEWPARTS.                    
002100     SELECT REPORT-FILE-OUT   ASSIGN TO UT-S-REPORT.                      
002200 DATA DIVISION.                                                           
002300 FILE SECTION.                                                            
002400 FD  OLD-PARTS-MF-IN                                                      
002500     RECORDING MODE IS F                                                  
002600     LABEL RECORDS ARE STANDARD                                           
002700     RECORD CONTAINS 100 CHARACTERS                                       
002800     BLOCK CONTAINS 0 RECORDS                                             
002900     DATA RECORD IS OLD-MF-RECORD.                                        
003000 01  OLD-MF-RECORD                       PIC  X(100).                     
003100 FD  CUR-ACT-FILE-IN                                                      
003200     RECORDING MODE IS F                                                  
003300     LABEL RECORDS ARE STANDARD                                           
003400     RECORD CONTAINS 80 CHARACTERS                                        
003500     BLOCK CONTAINS 0 RECORDS                                             
003600     DATA RECORD IS CUR-ACT-RECORD.                                       
003700 01  CUR-ACT-RECORD                      PIC  X(80).                      
003800 FD  PROD-LINE-FILE-IN                                                    
003900     RECORDING MODE IS F                                                  
004000     LABEL RECORDS ARE STANDARD                                           
004100     RECORD CONTAINS 80 CHARACTERS                                        
004200     BLOCK CONTAINS 0 RECORDS                                             
004300     DATA RECORD IS PROD-LINE-RECORD.                                     
004400 01  PROD-LINE-RECORD                    PIC  X(80).                      
004500*                                                                         
004600 SD  SORT-WORK-FILE                                                       
004700     RECORD CONTAINS 80 CHARACTERS                                        
004800     DATA RECORD IS SORT-WORK-REC.                                        
004900 01  SORT-WORK-REC.                                                       
005000     05  SWF-PARTNO                      PIC  X(21).                      
005100     05  SWF-REST-OF-REC                 PIC  X(43).                      
005200     05  SWF-MODEL                       PIC  X(3).                       
005300     05  FILLER                          PIC  X(12).                      
005400     05  SWF-REC-PAY-CODE                PIC  X.                          
005500*                                                                         
005600 FD  NEW-PARTS-MF-OUT                                                     
005700     RECORDING MODE IS F                                                  
005800     LABEL RECORDS ARE STANDARD                                           
005900     RECORD CONTAINS 100 CHARACTERS                                       
006000     BLOCK CONTAINS 0 RECORDS                                             
006100     DATA RECORD IS NEW-MF-RECORD.                                        
006200 01  NEW-MF-RECORD                       PIC  X(100).                     
006300*                                                                         
006400 FD  REPORT-FILE-OUT                                                      
006500     RECORDING MODE IS F                                                  
006600     LABEL RECORDS ARE STANDARD                                           
006700     RECORD CONTAINS 133 CHARACTERS                                       
006800     BLOCK CONTAINS 0 RECORDS                                             
006900     DATA RECORD IS REPORT-OUT-RECORD.                                    
007000 01  REPORT-OUT-RECORD                   PIC  X(133).                     
007100*                                                                         
007200 WORKING-STORAGE SECTION.                                                 
007300 01  WS-OLD-MF-REC.                                                       
007400     05  OMF-PARTNO                      PIC  X(21).                      
007500     05  OMF-PROD-LINE                   PIC  X(2).                       
007600     05  OMF-ACCOUNT                     PIC  X(9).                       
007700     05  OMF-REC-QTY                     PIC S9(7).                       
007800     05  OMF-REC-AMT                     PIC S9(8)V99.                    
007900     05  OMF-PAY-QTY                     PIC S9(7).                       
008000     05  OMF-PAY-AMT                     PIC S9(8)V99.                    
008100     05  OMF-PARTNAME                    PIC  X(15).                      
008200     05  OMF-LAST-ACTY.                                                   
008300         10  OMF-MON                     PIC  X(2).                       
008500         10  OMF-YR                      PIC  X(2).                       
008400         10  OMF-DAY                     PIC  X(2).                       
008600     05  FILLER                          PIC  X(13).                      
008700*                                                                         
008800 01  WS-CUR-ACT-REC.                                                      
008900     05  AF-PARTNO                       PIC  X(21).                      
009000     05  AF-REST-OF-REC.                                                  
009100         10  AF-PROD-LINE                PIC  X(2).                       
009200         10  AF-ACCOUNT                  PIC  X(9).                       
009300         10  AF-QTY                      PIC S9(7).                       
009400         10  AF-AMT                      PIC S9(8)V99.                    
009500         10  AF-PARTNAME                 PIC  X(15).                      
009600     05  AF-MODEL                        PIC  X(3).                       
009700     05  FILLER                          PIC  X(12) VALUE SPACE.          
009800     05  AF-REC-PAY-CODE                 PIC  X.                          
009900*                                                                         
010000 01  WS-PROD-LINE-REC.                                                    
010100     05  PLF-PROD-LINE                   PIC  X(2).                       
010200     05  PLF-MODEL                       PIC  X(3).                       
010300     05  FILLER                          PIC  X(75).                      
010400*                                                                         
010500 01  WS-DATE-LINE.                                                        
010600     05  FILLER                          PIC  X(104) VALUE SPACE.         
010700     05  FILLER                          PIC  X(6)                        
010800                VALUE 'DATE  '.                                           
010900     05  WS-DATE-OUT                     PIC  X(8).                       
011000     05  FILLER                          PIC  X(17) VALUE SPACE.          
011100*                                                                         
011200 01  WS-TITLE-LINE.                                                       
011300     05  FILLER                          PIC  X(41)                       
011400                VALUE ' REPORT 12345'.                                    
011500     05  FILLER                          PIC  X(63)                       
011600                VALUE 'PURCHASE PARTS RECEIVED-PAID REPORT'.              
011700     05  FILLER                          PIC  X(6)                        
011800                VALUE 'PAGE  '.                                           
011900     05  WS-PAGE-OUT                     PIC  Z,ZZ9.                      
012000     05  FILLER                          PIC  X(20) VALUE SPACE.          
012100*                                                                         
012200 01  WS-HEADING-LINE-1.                                                   
012300     05  FILLER                          PIC  X(25)                       
012400                VALUE '   PART NUMBER'.                                   
012500     05  FILLER                          PIC  X(11)                       
012600                VALUE 'PL  MOD'.                                          
012700     05  FILLER                          PIC  X(15)                       
012800                VALUE 'PART NAME'.                                        
012900     05  FILLER                          PIC  X(10)                       
013000                VALUE 'ACCOUNT'.                                          
013100     05  FILLER                          PIC  X(28)                       
013200                VALUE '******* RECEIPTS *******'.                         
013300     05  FILLER                          PIC  X(27)                       
013400                VALUE '******* PAYMENTS *******'.                         
013500     05  FILLER                          PIC  X(17)                       
013600                VALUE 'LAST ACTY'.                                        
013700*                                                                         
013800 01  WS-HEADING-LINE-2.                                                   
013900     05  FILLER                          PIC  X(62) VALUE SPACE.          
014000     05  FILLER                          PIC  X(13)                       
014100                VALUE 'QUANTITY'.                                         
014200     05  FILLER                          PIC  X(15)                       
014300                VALUE 'DOLLARS'.                                          
014400     05  FILLER                          PIC  X(13)                       
014500                VALUE 'QUANTITY'.                                         
014600     05  FILLER                          PIC  X(30)                       
014700                VALUE 'DOLLARS'.                                          
014800*                                                                         
014900 01  WS-DETAIL-LINE.                                                      
015000     05  FILLER                          PIC  X(1) VALUE SPACE.           
015100     05  DL-PARTNO-OUT                   PIC  X(21).                      
015200     05  FILLER                          PIC  X(3) VALUE SPACE.           
015300     05  DL-PROD-LINE-OUT                PIC  X(2).                       
015400     05  FILLER                          PIC  X(2) VALUE SPACE.           
015500     05  DL-MODEL-OUT                    PIC  X(3).                       
015600     05  FILLER                          PIC  X(2) VALUE SPACE.           
015700     05  DL-PARTNAME-OUT                 PIC  X(15).                      
015800     05  FILLER                          PIC  X VALUE SPACE.              
015900     05  DL-ACCOUNT-OUT                  PIC  X(9).                       
016000     05  FILLER                          PIC  X(2) VALUE SPACE.           
016100     05  DL-REC-QTY-OUT                  PIC  Z,ZZZ,ZZ9-.                 
016200     05  FILLER                          PIC  X VALUE SPACE.              
016300     05  DL-REC-AMT-OUT                  PIC  ZZ,ZZZ,ZZ9.99-.             
016400     05  FILLER                          PIC  X(3) VALUE SPACE.           
016500     05  DL-PAY-QTY-OUT                  PIC  Z,ZZZ,ZZ9-.                 
016600     05  FILLER                          PIC  X VALUE SPACE.              
016700     05  DL-PAY-AMT-OUT                  PIC  ZZ,ZZZ,ZZ9.99-.             
016800     05  FILLER                          PIC  X(19) VALUE SPACE.          
016900*                                                                         
017000 01  WS-TOTAL-LINE.                                                       
017100     05  FILLER                          PIC  X(3) VALUE SPACE.           
017200     05  TL-TYPE-OUT                     PIC  X(7).                       
017300     05  FILLER                          PIC  X(28)                       
017400                VALUE ' PART NUMBER TOTAL FOR PART '.                     
017500     05  TL-PARTNO-OUT                   PIC  X(21).                      
017600     05  FILLER                          PIC  X(2) VALUE SPACE.           
017700     05  TL-LINE-VALUES.                                                  
017800         10  TL-REC-QTY-OUT              PIC  Z,ZZZ,ZZ9-.                 
017900         10  FILLER                      PIC  X VALUE SPACE.              
018000         10  TL-REC-AMT-OUT              PIC  ZZ,ZZZ,ZZ9.99-.             
018100         10  FILLER                      PIC  X(3) VALUE SPACE.           
018200         10  TL-PAY-QTY-OUT              PIC  Z,ZZZ,ZZ9-.                 
018300         10  FILLER                      PIC  X VALUE SPACE.              
018400         10  TL-PAY-AMT-OUT              PIC  ZZ,ZZZ,ZZ9.99-.             
018500         10  FILLER                      PIC  X(3) VALUE SPACE.           
018600         10  TL-LAST-ACTY-OUT            PIC  X(6).                       
018700     05  FILLER                          PIC  X(11) VALUE SPACE.          
018800*                                                                         
018900 01  WS-GRAND-TOTAL-LINE.                                                 
019000     05  FILLER                          PIC  X(3) VALUE SPACE.           
019100     05  GTL-TYPE-OUT                    PIC  X(7).                       
019200     05  FILLER                          PIC  X(51)                       
019300                VALUE '  GRAND TOTAL'.                                    
019400     05  GTL-LINE-VALUES.                                                 
019500         10  GTL-REC-QTY-OUT             PIC  Z,ZZZ,ZZ9-.                 
019600         10  FILLER                      PIC  X VALUE SPACE.              
019700         10  GTL-REC-AMT-OUT             PIC  ZZ,ZZZ,ZZ9.99-.             
019800         10  FILLER                      PIC  X(3) VALUE SPACE.           
019900         10  GTL-PAY-QTY-OUT             PIC  Z,ZZZ,ZZ9-.                 
020000         10  FILLER                      PIC  X VALUE SPACE.              
020100         10  GTL-PAY-AMT-OUT             PIC  ZZ,ZZZ,ZZ9.99-.             
020200     05  FILLER                          PIC  X(20) VALUE SPACE.          
020300*                                                                         
020400 01  PRODUCT-LINE-TABLE.                                                  
020500     05  PROD-LINE-TBL OCCURS 50 TIMES                                    
020600                   INDEXED BY PROD-NDX.                                   
020700         10  PLT-PROD-LINE               PIC  X(2).                       
020800         10  PLT-MODEL                   PIC  X(3).                       
020900*                                                                         
021000 01  MONTH-TABLE-DEFINITION.                                              
021100     05  MONTH-DATA.                                                      
021200         10  FILLER               PIC  X(5) VALUE '01JAN'.                
021300         10  FILLER               PIC  X(5) VALUE '02FEB'.                
021400         10  FILLER               PIC  X(5) VALUE '03MAR'.                
021500         10  FILLER               PIC  X(5) VALUE '04APR'.                
021600         10  FILLER               PIC  X(5) VALUE '05MAY'.                
021700         10  FILLER               PIC  X(5) VALUE '06JUN'.                
021800         10  FILLER               PIC  X(5) VALUE '07JUL'.                
021900         10  FILLER               PIC  X(5) VALUE '08AUG'.                
022000         10  FILLER               PIC  X(5) VALUE '09SEP'.                
022100         10  FILLER               PIC  X(5) VALUE '10OCT'.                
022200         10  FILLER               PIC  X(5) VALUE '11NOV'.                
022300         10  FILLER               PIC  X(5) VALUE '12DEC'.                
022400     05  MONTH-TABLE REDEFINES MONTH-DATA.                                
022500         10  MONTHS-TBL  OCCURS 12 TIMES  INDEXED BY MT-NDX.              
022600             15  MT-CODE          PIC  X(2).                              
022700             15  MT-MONTH         PIC  X(3).                              
022800*                                                                         
022900 01  WS-HOLD-AREAS.                                                       
023000     05  WS-MF-HOLD-AREA                 PIC  X(100) VALUE SPACE.         
023100     05  TH-DATE.                                                         
023200         10  WS-YR                       PIC  X(2).                       
023300         10  WS-MON                      PIC  X(2).                       
023400         10  WS-DAY                      PIC  X(2).                       
               10  FILLER                      PIC  X(2).                       
023500     05  MF-DATE.                                                         
023800         10  MF-YR                       PIC  X(2).                       
023600         10  MF-MON                      PIC  X(2).                       
023700         10  MF-DAY                      PIC  X(2).                       
               10  FILLER                      PIC  X(2).                       
023900     05  WS-GDATE.                                                        
024000         10  WS-MONTH                    PIC  X(3).                       
024100         10  FILLER                      PIC  X(1) VALUE SPACE.           
024200         10  WS-YEARS                    PIC  X(2).                       
024300*                                                                         
024400 01  ACCUMULATORS.                                                        
024500     05  AC-CUR-REC-QTY           PIC S9(7)    COMP-3 VALUE ZERO.         
024600     05  AC-CUR-REC-AMT           PIC S9(8)V99 COMP-3 VALUE ZERO.         
024700     05  AC-TOD-REC-QTY           PIC S9(7)    COMP-3 VALUE ZERO.         
024800     05  AC-TOD-REC-AMT           PIC S9(8)V99 COMP-3 VALUE ZERO.         
024900     05  AC-CUR-PAY-QTY           PIC S9(7)    COMP-3 VALUE ZERO.         
025000     05  AC-CUR-PAY-AMT           PIC S9(8)V99 COMP-3 VALUE ZERO.         
025100     05  AC-TOD-PAY-QTY           PIC S9(7)    COMP-3 VALUE ZERO.         
025200     05  AC-TOD-PAY-AMT           PIC S9(8)V99 COMP-3 VALUE ZERO.         
025300     05  AC-GTL-CUR-REC-QTY       PIC S9(7)    COMP-3 VALUE ZERO.         
025400     05  AC-GTL-CUR-REC-AMT       PIC S9(8)V99 COMP-3 VALUE ZERO.         
025500     05  AC-GTL-TOD-REC-QTY       PIC S9(7)    COMP-3 VALUE ZERO.         
025600     05  AC-GTL-TOD-REC-AMT       PIC S9(8)V99 COMP-3 VALUE ZERO.         
025700     05  AC-GTL-CUR-PAY-QTY       PIC S9(7)    COMP-3 VALUE ZERO.         
025800     05  AC-GTL-CUR-PAY-AMT       PIC S9(8)V99 COMP-3 VALUE ZERO.         
025900     05  AC-GTL-TOD-PAY-QTY       PIC S9(7)    COMP-3 VALUE ZERO.         
026000     05  AC-GTL-TOD-PAY-AMT       PIC S9(8)V99 COMP-3 VALUE ZERO.         
026100     05  AC-RECS-REL-SRT          PIC S9(3)    COMP-3 VALUE ZERO.         
026200     05  AC-RECS-RET-SRT          PIC S9(3)    COMP-3 VALUE ZERO.         
026300     05  AC-PROD-IN               PIC S9(3)    COMP-3 VALUE ZERO.         
026400     05  AC-ACT-IN                PIC S9(3)    COMP-3 VALUE ZERO.         
026500     05  AC-ACT-BYPASS            PIC S9(3)    COMP-3 VALUE ZERO.         
026600     05  AC-MASTER-IN             PIC S9(3)    COMP-3 VALUE ZERO.         
026700     05  AC-MASTER-OUT            PIC S9(3)    COMP-3 VALUE ZERO.         
026800*                                                                         
026900 01  REPORT-CONTROLS.                                                     
027000     05  RC-PAGE-NUM              PIC S9(3) COMP-3 VALUE ZERO.            
027100     05  RC-LINE-COUNT            PIC S9(2) COMP-3 VALUE ZERO.            
027200     05  RC-LINES-PER-PAGE        PIC  9(2) VALUE 50.                     
027300     05  RC-PROPER-SPACING        PIC  9.                                 
027400     05  RC-MSG-OUT               PIC ZZ9.                                
027500     05  RC-ERR-MSG               PIC X(35).                              
027600*                                                                         
027700 01  SWITCHES.                                                            
027800     05  SW-EOF-OMF               PIC  X VALUE 'N'.                       
027900         88  EOF-OMF                     VALUE 'Y'.                       
028000     05  SW-EOF-PROD              PIC  X VALUE 'N'.                       
028100         88  EOF-PROD                    VALUE 'Y'.                       
028200     05  SW-EOF-ACT               PIC  X VALUE 'N'.                       
028300         88  EOF-ACT                     VALUE 'Y'.                       
028400     05  SW-EOF-SRT               PIC  X VALUE 'N'.                       
028500         88  EOF-SRT                     VALUE 'Y'.                       
028600     05  SW-FOUND                 PIC  X VALUE 'Y'.                       
028700         88  MODEL-FOUND                 VALUE 'Y'.                       
028800     05  SW-VALID                 PIC  X VALUE 'Y'.                       
028900         88  RECORD-VALID                VALUE 'Y'.                       
029000     05  SW-NEW-MF                PIC  X VALUE 'N'.                       
029100         88  NEW-REC-WAITING             VALUE 'Y'.                       
029200*                                                                         
029300 PROCEDURE DIVISION.                                                      
029400 000-MAINLINE.                                                            
029500     PERFORM 100-INITIALIZE  THRU 100-EXIT.                               
029600     PERFORM 200-PROCESS     THRU 200-EXIT.                               
029700     PERFORM 300-TERMINATION THRU 300-EXIT.                               
029800     MOVE ZERO TO RETURN-CODE.                                            
029900     GOBACK.                                                              
030000 000-EXIT.                                                                
030100     EXIT.                                                                
030200*                                                                         
030300 100-INITIALIZE.                                                          
030500     DISPLAY '***** BEGIN B999KAD3 *****'.                                
030600     DISPLAY ' '.                                                         
030700     OPEN INPUT OLD-PARTS-MF-IN                                           
030800          INPUT CUR-ACT-FILE-IN                                           
030900          INPUT PROD-LINE-FILE-IN                                         
031000         OUTPUT NEW-PARTS-MF-OUT                                          
031100         OUTPUT REPORT-FILE-OUT.                                          
031200     PERFORM 105-READ-PROD THRU 105-EXIT.                                 
031300     IF EOF-PROD THEN                                                     
031400         DISPLAY '*** PROD FILE EMPTY ***'                                
031500         GOBACK.                                                          
031600     SET PROD-NDX TO 1.                                                   
031700     PERFORM 110-LOAD-PROD-TABLE THRU 110-EXIT                            
031800         UNTIL EOF-PROD.                                                  
031900     CLOSE PROD-LINE-FILE-IN.                                             
032000     MOVE  1 TO RC-PAGE-NUM.                                              
032100     MOVE 99 TO RC-LINE-COUNT.                                            
032200*    ACCEPT TH-DATE FROM DATE.                                            
           MOVE FUNCTION CURRENT-DATE(1:8) TO TH-DATE, WS-DATE, MF-DATE.        
032200*    ACCEPT WS-DATE-OUT FROM DATE.                                        
032200*    ACCEPT MF-DATE FROM DATE.                                            
032700     PERFORM 229-READ-ACTIVITY THRU 229-EXIT.                             
032800     IF EOF-ACT THEN                                                      
032900         DISPLAY '*** DATA FILE EMPTY ***'                                
033000         GOBACK.                                                          
033100     PERFORM 278-HEADINGS THRU 278-EXIT.                                  
033200 100-EXIT.                                                                
033300     EXIT.                                                                
033400*                                                                         
033500 105-READ-PROD.                                                           
033600     READ PROD-LINE-FILE-IN                                               
033700         INTO WS-PROD-LINE-REC                                            
033800             AT END MOVE 'Y' TO SW-EOF-PROD.                              
033900     IF NOT EOF-PROD THEN                                                 
034000         ADD 1 TO AC-PROD-IN.                                             
034100 105-EXIT.                                                                
034200     EXIT.                                                                
034300*                                                                         
034400 110-LOAD-PROD-TABLE.                                                     
034500     MOVE PLF-PROD-LINE TO PLT-PROD-LINE (PROD-NDX).                      
034600     MOVE PLF-MODEL     TO PLT-MODEL (PROD-NDX).                          
034700     SET PROD-NDX UP BY 1.                                                
034800     PERFORM 105-READ-PROD THRU 105-EXIT.                                 
034900 110-EXIT.                                                                
035000     EXIT.                                                                
035100*                                                                         
035200 200-PROCESS.                                                             
035300     SORT SORT-WORK-FILE                                                  
035400         ASCENDING KEY SWF-PARTNO                                         
035500             INPUT  PROCEDURE 220-INPUT-PROCEDURE                         
035600             OUTPUT PROCEDURE 250-OUTPUT-PROCEDURE.                       
035700 200-EXIT.                                                                
035800     EXIT.                                                                
035900*                                                                         
036000*                                                                         
036100*                                                                         
036200 220-INPUT-PROCEDURE SECTION.                                             
036300     PERFORM 222-SELECT-RECS  THRU 222-EXIT                               
036400         UNTIL EOF-ACT.                                                   
036500 220-EXIT-IP SECTION.                                                     
036600*                                                                         
036700 222-SELECT-RECS.                                                         
036800     PERFORM 230-EDITS THRU 230-EXIT                                      
036900     IF RECORD-VALID THEN                                                 
037000         PERFORM 232-LOOKUP-MODEL THRU 232-EXIT                           
037100         IF MODEL-FOUND THEN                                              
037200             PERFORM 233-PREP-SRT-REC         THRU 233-EXIT               
037300             PERFORM 240-RELEASE-ACTIVITY-REC THRU 240-EXIT               
037400         ELSE                                                             
037500             DISPLAY 'MODEL NOT FOUND - ' WS-CUR-ACT-REC                  
037600             ADD 1 TO AC-ACT-BYPASS                                       
037700     ELSE                                                                 
037800         DISPLAY RC-ERR-MSG WS-CUR-ACT-REC                                
037900         ADD 1 TO AC-ACT-BYPASS.                                          
038000     PERFORM 229-READ-ACTIVITY THRU 229-EXIT.                             
038100 222-EXIT.                                                                
038200     EXIT.                                                                
038300*                                                                         
038400 229-READ-ACTIVITY.                                                       
038500     READ CUR-ACT-FILE-IN                                                 
038600         INTO WS-CUR-ACT-REC                                              
038700             AT END MOVE 'Y' TO SW-EOF-ACT.                               
038800     IF NOT EOF-ACT THEN                                                  
038900         ADD 1 TO AC-ACT-IN.                                              
039000 229-EXIT.                                                                
039100     EXIT.                                                                
039200*                                                                         
039300 230-EDITS.                                                               
039400     MOVE 'Y' TO SW-VALID.                                                
039500     IF AF-QTY NOT NUMERIC OR AF-AMT NOT NUMERIC THEN                     
039600         MOVE 'N' TO SW-VALID                                             
039700         MOVE 'RECORD IN ERROR-INVALID AMT/QTY: ' TO RC-ERR-MSG           
039800         GO TO 230-EXIT.                                                  
039900     IF AF-REC-PAY-CODE = 'P' OR AF-REC-PAY-CODE = 'R' THEN               
040000         NEXT SENTENCE                                                    
040100     ELSE                                                                 
040200         MOVE 'N' TO SW-VALID                                             
040300         MOVE 'RECORD IN ERROR-INVALID CODE:    ' TO RC-ERR-MSG.          
040400 230-EXIT.                                                                
040500     EXIT.                                                                
040600*                                                                         
040700 232-LOOKUP-MODEL.                                                        
040800     MOVE 'Y' TO SW-FOUND.                                                
040900     SET PROD-NDX TO 1.                                                   
041000     SEARCH PROD-LINE-TBL                                                 
041100         AT END MOVE 'N' TO SW-FOUND                                      
041200         WHEN AF-PROD-LINE = PLT-PROD-LINE (PROD-NDX)                     
041300             MOVE PLT-MODEL (PROD-NDX) TO SWF-MODEL.                      
041400 232-EXIT.                                                                
041500     EXIT.                                                                
041600*                                                                         
041700 233-PREP-SRT-REC.                                                        
041800     IF MODEL-FOUND THEN                                                  
041900         MOVE AF-PARTNO       TO SWF-PARTNO                               
042000         MOVE AF-REST-OF-REC  TO SWF-REST-OF-REC                          
042100         MOVE AF-REC-PAY-CODE TO SWF-REC-PAY-CODE                         
042200     ELSE                                                                 
042300         DISPLAY 'NAME NOT FOUND IN TBL - ' WS-CUR-ACT-REC.               
042400 233-EXIT.                                                                
042500     EXIT.                                                                
042600*                                                                         
042700 240-RELEASE-ACTIVITY-REC.                                                
042800     RELEASE SORT-WORK-REC.                                               
042900     ADD 1 TO AC-RECS-REL-SRT.                                            
043000 240-EXIT.                                                                
043100     EXIT.                                                                
043200*                                                                         
043300*                                                                         
043400*                                                                         
043500 250-OUTPUT-PROCEDURE SECTION.                                            
043600     PERFORM 280-RETURN-ACTIVITY-RECORD THRU 280-EXIT.                    
043700     PERFORM 285-READ-OLD-MASTER        THRU 285-EXIT.                    
043800     PERFORM 255-PROCESS-MF             THRU 255-EXIT                     
043900         UNTIL EOF-SRT AND EOF-OMF.                                       
044000 250-EXIT-OP SECTION.                                                     
044100*                                                                         
044200*                                                                         
044300*                                                                         
044400 255-PROCESS-MF.                                                          
044500     IF AF-PARTNO = OMF-PARTNO THEN                                       
044600         PERFORM 260-UPDATE-OLD-MASTER      THRU 260-EXIT                 
044700         PERFORM 265-PREP-PRINT-LINE        THRU 265-EXIT                 
044800         PERFORM 280-RETURN-ACTIVITY-RECORD THRU 280-EXIT                 
044900     ELSE                                                                 
045000         IF AF-PARTNO > OMF-PARTNO THEN                                   
045100             PERFORM 290-WRITE-NEW-MF    THRU 290-EXIT                    
045200             PERFORM 275-PART-TOTALS     THRU 275-EXIT                    
045300             PERFORM 285-READ-OLD-MASTER THRU 285-EXIT                    
045400         ELSE                                                             
045500             PERFORM 262-CREATE-NEW-MF THRU 262-EXIT.                     
045600 255-EXIT.                                                                
045700     EXIT.                                                                
045800*                                                                         
045900 260-UPDATE-OLD-MASTER.                                                   
046000     IF AF-REC-PAY-CODE = 'R' THEN                                        
046100         ADD  AF-QTY TO OMF-REC-QTY AC-CUR-REC-QTY AC-TOD-REC-QTY         
046200         ADD  AF-AMT TO OMF-REC-AMT AC-CUR-REC-AMT AC-TOD-REC-AMT         
046300         MOVE AF-QTY TO DL-REC-QTY-OUT                                    
046400         MOVE AF-AMT TO DL-REC-AMT-OUT                                    
046500         MOVE ZERO   TO DL-PAY-QTY-OUT DL-PAY-AMT-OUT                     
046600     ELSE                                                                 
046700         ADD  AF-QTY TO OMF-PAY-QTY AC-CUR-PAY-QTY AC-TOD-PAY-QTY         
046800         ADD  AF-AMT TO OMF-PAY-AMT AC-CUR-PAY-AMT AC-TOD-PAY-AMT         
046900         MOVE AF-QTY TO DL-PAY-QTY-OUT                                    
047000         MOVE AF-AMT TO DL-PAY-AMT-OUT                                    
047100         MOVE ZERO   TO DL-REC-QTY-OUT DL-REC-AMT-OUT.                    
047200     MOVE AF-PROD-LINE TO OMF-PROD-LINE.                                  
047300     MOVE AF-ACCOUNT   TO OMF-ACCOUNT.                                    
047400     MOVE AF-PARTNAME  TO OMF-PARTNAME.                                   
047500     MOVE MF-DATE      TO OMF-LAST-ACTY.                                  
047600     PERFORM 276-LOOKUP-MONTH THRU 276-EXIT.                              
047700 260-EXIT.                                                                
047800     EXIT.                                                                
047900*                                                                         
048000 262-CREATE-NEW-MF.                                                       
048200     MOVE SPACES        TO WS-OLD-MF-REC.                                 
048100     MOVE WS-OLD-MF-REC TO WS-MF-HOLD-AREA.                               
048300     MOVE ZERO          TO OMF-REC-QTY.                                   
048400     MOVE ZERO          TO OMF-REC-AMT.                                   
048500     MOVE ZERO          TO OMF-PAY-QTY.                                   
048600     MOVE ZERO          TO OMF-PAY-AMT.                                   
048700     MOVE AF-PARTNO     TO OMF-PARTNO.                                    
048800 262-EXIT.                                                                
048900     EXIT.                                                                
049000*                                                                         
049100 265-PREP-PRINT-LINE.                                                     
049200     IF RC-LINE-COUNT > RC-LINES-PER-PAGE THEN                            
049300         PERFORM 278-HEADINGS THRU 278-EXIT.                              
049400     MOVE AF-PARTNO    TO DL-PARTNO-OUT.                                  
049500     MOVE AF-PROD-LINE TO DL-PROD-LINE-OUT.                               
049600     MOVE AF-MODEL     TO DL-MODEL-OUT.                                   
049700     MOVE AF-PARTNAME  TO DL-PARTNAME-OUT.                                
049800     MOVE AF-ACCOUNT   TO DL-ACCOUNT-OUT.                                 
049900     WRITE REPORT-OUT-RECORD FROM WS-DETAIL-LINE                          
050000         AFTER ADVANCING RC-PROPER-SPACING.                               
050100     ADD RC-PROPER-SPACING TO RC-LINE-COUNT.                              
050200     MOVE 1 TO RC-PROPER-SPACING.                                         
050300 265-EXIT.                                                                
050400     EXIT.                                                                
050500*                                                                         
050600 275-PART-TOTALS.                                                         
050700     MOVE SPACES           TO TL-LINE-VALUES.                             
050800     MOVE 'CURRENT'        TO TL-TYPE-OUT.                                
050900     MOVE OMF-PARTNO       TO TL-PARTNO-OUT.                              
051000     MOVE AC-CUR-REC-QTY   TO TL-REC-QTY-OUT.                             
051100     MOVE AC-CUR-REC-AMT   TO TL-REC-AMT-OUT.                             
051200     MOVE AC-CUR-PAY-QTY   TO TL-PAY-QTY-OUT.                             
051300     MOVE AC-CUR-PAY-AMT   TO TL-PAY-AMT-OUT.                             
051400     WRITE REPORT-OUT-RECORD FROM WS-TOTAL-LINE                           
051500         AFTER ADVANCING 2.                                               
051600     MOVE 'TO-DATE'        TO TL-TYPE-OUT.                                
051700     MOVE AC-TOD-REC-QTY   TO TL-REC-QTY-OUT.                             
051800     MOVE AC-TOD-REC-AMT   TO TL-REC-AMT-OUT.                             
051900     MOVE AC-TOD-PAY-QTY   TO TL-PAY-QTY-OUT.                             
052000     MOVE AC-TOD-PAY-AMT   TO TL-PAY-AMT-OUT.                             
052100     MOVE OMF-MON          TO WS-MON.                                     
052200     MOVE OMF-YR           TO WS-YR.                                      
052300     PERFORM 276-LOOKUP-MONTH THRU 276-EXIT.                              
052400     MOVE WS-GDATE         TO TL-LAST-ACTY-OUT.                           
052500     WRITE REPORT-OUT-RECORD FROM WS-TOTAL-LINE                           
052600         AFTER ADVANCING 1.                                               
052700     MOVE SPACES TO REPORT-OUT-RECORD.                                    
052800     WRITE REPORT-OUT-RECORD                                              
052900         AFTER ADVANCING 1.                                               
053000     ADD  3 TO RC-LINE-COUNT.                                             
053100     MOVE 2 TO RC-PROPER-SPACING.                                         
053200     ADD AC-CUR-REC-QTY    TO AC-GTL-CUR-REC-QTY.                         
053300     ADD AC-CUR-REC-AMT    TO AC-GTL-CUR-REC-AMT.                         
053400     ADD AC-CUR-PAY-QTY    TO AC-GTL-CUR-PAY-QTY.                         
053500     ADD AC-CUR-PAY-AMT    TO AC-GTL-CUR-PAY-AMT.                         
053600     ADD AC-TOD-REC-QTY    TO AC-GTL-TOD-REC-QTY.                         
053700     ADD AC-TOD-REC-AMT    TO AC-GTL-TOD-REC-AMT.                         
053800     ADD AC-TOD-PAY-QTY    TO AC-GTL-TOD-PAY-QTY.                         
053900     ADD AC-TOD-PAY-AMT    TO AC-GTL-TOD-PAY-AMT.                         
054000     MOVE ZERO             TO AC-CUR-REC-QTY.                             
054100     MOVE ZERO             TO AC-CUR-REC-AMT.                             
054200     MOVE ZERO             TO AC-CUR-PAY-QTY.                             
054300     MOVE ZERO             TO AC-CUR-PAY-AMT.                             
054400     MOVE ZERO             TO AC-TOD-REC-QTY.                             
054500     MOVE ZERO             TO AC-TOD-REC-AMT.                             
054600     MOVE ZERO             TO AC-TOD-PAY-QTY.                             
054700     MOVE ZERO             TO AC-TOD-PAY-AMT.                             
054800 275-EXIT.                                                                
054900     EXIT.                                                                
055000*                                                                         
055100 276-LOOKUP-MONTH.                                                        
055200     SET MT-NDX TO 1.                                                     
055300     SEARCH MONTHS-TBL                                                    
055400         AT END DISPLAY '*** DATE ERROR ***'                              
055500         WHEN WS-MON = MT-CODE (MT-NDX)                                   
055600             MOVE MT-MONTH (MT-NDX) TO WS-MONTH.                          
055700     MOVE WS-YR TO WS-YEARS.                                              
055800 276-EXIT.                                                                
055900     EXIT.                                                                
056000*                                                                         
056100 278-HEADINGS.                                                            
056200     MOVE RC-PAGE-NUM TO WS-PAGE-OUT.                                     
056300     WRITE REPORT-OUT-RECORD FROM WS-DATE-LINE                            
056400         AFTER ADVANCING PAGE.                                            
056500     WRITE REPORT-OUT-RECORD FROM WS-TITLE-LINE                           
056600         AFTER ADVANCING 2.                                               
056700     WRITE REPORT-OUT-RECORD FROM WS-HEADING-LINE-1                       
056800         AFTER ADVANCING 3.                                               
056900     WRITE REPORT-OUT-RECORD FROM WS-HEADING-LINE-2                       
057000         AFTER ADVANCING 1.                                               
057100     MOVE  2  TO RC-PROPER-SPACING.                                       
057200     MOVE  7  TO RC-LINE-COUNT.                                           
057300     ADD   1  TO RC-PAGE-NUM.                                             
057400 278-EXIT.                                                                
057500     EXIT.                                                                
057600*                                                                         
057700 280-RETURN-ACTIVITY-RECORD.                                              
057800     RETURN SORT-WORK-FILE                                                
057900         INTO WS-CUR-ACT-REC                                              
058000             AT END MOVE 'Y' TO SW-EOF-SRT                                
058100                    MOVE HIGH-VALUES TO AF-PARTNO.                        
058200     IF NOT EOF-SRT                                                       
058300         ADD 1 TO AC-RECS-RET-SRT.                                        
058400 280-EXIT.                                                                
058500     EXIT.                                                                
058600*                                                                         
058700 285-READ-OLD-MASTER.                                                     
058800     IF WS-MF-HOLD-AREA NOT = SPACES                                      
058900         MOVE WS-MF-HOLD-AREA TO WS-OLD-MF-REC                            
059000         MOVE SPACES TO WS-MF-HOLD-AREA                                   
059100         MOVE OMF-REC-QTY TO AC-TOD-REC-QTY                               
059200         MOVE OMF-REC-AMT TO AC-TOD-REC-AMT                               
059300         MOVE OMF-PAY-QTY TO AC-TOD-PAY-QTY                               
059400         MOVE OMF-PAY-AMT TO AC-TOD-PAY-AMT                               
059500         GO TO 285-EXIT.                                                  
059600     READ OLD-PARTS-MF-IN                                                 
059700         INTO WS-OLD-MF-REC                                               
059800             AT END MOVE 'Y' TO SW-EOF-OMF                                
059900                    MOVE HIGH-VALUES TO OMF-PARTNO.                       
060000     IF NOT EOF-OMF THEN                                                  
060100         ADD 1 TO AC-MASTER-IN                                            
060200         MOVE OMF-REC-QTY TO AC-TOD-REC-QTY                               
060300         MOVE OMF-REC-AMT TO AC-TOD-REC-AMT                               
060400         MOVE OMF-PAY-QTY TO AC-TOD-PAY-QTY                               
060500         MOVE OMF-PAY-AMT TO AC-TOD-PAY-AMT.                              
060600 285-EXIT.                                                                
060700     EXIT.                                                                
060800*                                                                         
060900 290-WRITE-NEW-MF.                                                        
061000     WRITE NEW-MF-RECORD FROM WS-OLD-MF-REC.                              
061100     ADD 1 TO AC-MASTER-OUT.                                              
061200 290-EXIT.                                                                
061300     EXIT.                                                                
061400*                                                                         
061500 300-TERMINATION.                                                         
061600     PERFORM 275-PART-TOTALS  THRU 275-EXIT.                              
061700     PERFORM 310-GRAND-TOTALS THRU 310-EXIT.                              
061800     CLOSE OLD-PARTS-MF-IN                                                
061900           NEW-PARTS-MF-OUT                                               
062000           CUR-ACT-FILE-IN                                                
062100           REPORT-FILE-OUT.                                               
062200****************************************                                  
062300****     DISPLAY REPORT CONTROLS    ****                                  
062400****************************************                                  
062500     MOVE AC-PROD-IN      TO RC-MSG-OUT.                                  
062600     DISPLAY 'PRODUCT LINE IN    ' RC-MSG-OUT.                            
062700     DISPLAY ' '.                                                         
062800     MOVE AC-ACT-IN       TO RC-MSG-OUT.                                  
062900     DISPLAY 'ACTIVITY IN        ' RC-MSG-OUT.                            
063000     MOVE AC-ACT-BYPASS   TO RC-MSG-OUT.                                  
063100     DISPLAY 'ACTIVITY BYPASSED  ' RC-MSG-OUT.                            
063200     DISPLAY ' '.                                                         
063300     MOVE AC-MASTER-IN    TO RC-MSG-OUT.                                  
063400     DISPLAY 'MASTERS IN         ' RC-MSG-OUT.                            
063500     MOVE AC-MASTER-OUT   TO RC-MSG-OUT.                                  
063600     DISPLAY 'MASTERS OUT        ' RC-MSG-OUT.                            
063700     DISPLAY ' '.                                                         
063800     MOVE AC-RECS-REL-SRT TO RC-MSG-OUT.                                  
063900     DISPLAY 'RELEASED ACTIVITY  ' RC-MSG-OUT.                            
064000     MOVE AC-RECS-RET-SRT TO RC-MSG-OUT.                                  
064100     DISPLAY 'RETURNED ACTIVITY  ' RC-MSG-OUT.                            
064200     DISPLAY ' '.                                                         
064300     DISPLAY '***** B999KAD3 EOJ *****'.                                  
064400 300-EXIT.                                                                
064500     EXIT.                                                                
064600*                                                                         
064700 310-GRAND-TOTALS.                                                        
064800     MOVE 'CURRENT' TO GTL-TYPE-OUT.                                      
064900     MOVE SPACES    TO GTL-LINE-VALUES.                                   
065000     MOVE AC-GTL-CUR-REC-QTY TO GTL-REC-QTY-OUT.                          
065100     MOVE AC-GTL-CUR-REC-AMT TO GTL-REC-AMT-OUT.                          
065200     MOVE AC-GTL-CUR-PAY-QTY TO GTL-PAY-QTY-OUT.                          
065300     MOVE AC-GTL-CUR-PAY-AMT TO GTL-PAY-AMT-OUT.                          
065400     WRITE REPORT-OUT-RECORD FROM WS-GRAND-TOTAL-LINE                     
065500         AFTER ADVANCING 3.                                               
065600     MOVE 'TO-DATE' TO GTL-TYPE-OUT.                                      
065700     MOVE SPACES    TO GTL-LINE-VALUES.                                   
065800     MOVE AC-GTL-TOD-REC-QTY TO GTL-REC-QTY-OUT.                          
065900     MOVE AC-GTL-TOD-REC-AMT TO GTL-REC-AMT-OUT.                          
066000     MOVE AC-GTL-TOD-PAY-QTY TO GTL-PAY-QTY-OUT.                          
066100     MOVE AC-GTL-TOD-PAY-AMT TO GTL-PAY-AMT-OUT.                          
066200     WRITE REPORT-OUT-RECORD FROM WS-GRAND-TOTAL-LINE                     
066300         AFTER ADVANCING 2.                                               
066400 310-EXIT.                                                                
066500     EXIT.                                                                
