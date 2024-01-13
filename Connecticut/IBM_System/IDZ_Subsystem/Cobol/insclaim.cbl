000001 IDENTIFICATION DIVISION.                                                 
000002 PROGRAM-ID.      INSUREDCLAIM                                            
000003 AUTHOR.          IBM.                                                    
      ******************************************************************        
      **** This program creates a report totaling INSURANCE claims              
      **** entered over the past week                                           
      ******************************************************************        
000004                                                                          
000005 ENVIRONMENT DIVISION.                                                    
000006 INPUT-OUTPUT SECTION.                                                    
000007 FILE-CONTROL.                                                            
           SELECT CLAIM-FILE                                                    
             ASSIGN TO UT-S-CLAIM                                               
               ORGANIZATION IS SEQUENTIAL.                                      
000010     SELECT PRINT-FILE                                                    
             ASSIGN TO CLAIMRPT                                                 
000011         ORGANIZATION IS SEQUENTIAL.                                      
000012                                                                          
000013 DATA DIVISION.                                                           
000014 FILE SECTION.                                                            
000015 FD  CLAIM-FILE                                                           
000016     RECORD CONTAINS 56 CHARACTERS.                                       
000017 01  CLAIM-RECORD                 PIC X(56).                              
000018                                                                          
000019 FD  PRINT-FILE                                                           
000020     RECORD CONTAINS 132 CHARACTERS.                                      
000021 01  PRINT-LINE                    PIC X(132).                            
000022                                                                          
000023 WORKING-STORAGE SECTION.                                                 
000024 01  FILLER                        PIC X(60)                              
000025         VALUE 'WORKING STORAGE DUMP LABEL'.                              
000026                                                                          
000027 01  CLAIM-RECORD-IN.                                                     
000028     05  INSURED-CONTRACT-NO           PIC 9(7).                          
           05  INSURED-LAST-NANE             PIC X(15).                         
           05  INSURED-FIRST-NANE            PIC X(10).                         
000037     05  POLICY-TYPE                   PIC X.                             
               88  WHOLE-LIFE VALUE 'W'.                                        
               88  TERM-LIFE VALUE 'T'.                                         
               88  UNIVERSAL-LIFE VALUE 'U'.                                    
000038     05  COINSURANCE                   PIC 99.                            
000039     05  POLICY-DETAILS.                                                  
000034         10  POLICY-YEAR               PIC 9(2).                          
000035         10  POLICY-MONTH              PIC 9(2).                          
000036         10  POLICY-DAY                PIC 9(2).                          
000039         10  POLICY-CLAIM-AMOUNT              PIC 9(7)V99.                
000041         10  POLICY-FACE-AMOUNT        PIC 9(7)V99.                       
000043     05  CLAIM-TYPE                    PIC X.                             
000044                                                                          
000045 01  PROGRAM-SWITCHES.                                                    
000046     05  REINSURANCE                   PIC XX  VALUE SPACES.              
000047     05  INSURED-SUB                   PIC 999 VALUE 1.                   
000048                                                                          
000049 01  REPORT-FIELDS.                                                       
000050     05  LINE-COUNT                PIC 9(2)    VALUE 6.                   
000051     05  PAGE-COUNT                PIC 9(2)    VALUE ZEROS.               
000052     05  LINES-PER-PAGE            PIC 9(2)    VALUE 5.                   
000053                                                                          
000054 01  DAILY-RATES.                                                         
000055     05  ECONOMY-RATE              PIC 9(3)V99  VALUE 15.                 
000056     05  COMPACT-RATE              PIC 9(3)V99  VALUE 20.                 
000057     05  MID-RATE                  PIC 9(3)V99  VALUE 24.                 
000058     05  FULL-RATE                 PIC 9(3)V99  VALUE 28.                 
000059     05  LUXURY-RATE               PIC 9(3)V99  VALUE 35.                 
000060     05  INSURANCE-RATE            PIC 99V99    VALUE 10.50.              
000061                                                                          
000062 01  TOT-BILL-INFORMATION.                                                
000063     05  TOT-FACE-AMOUNT           PIC 9(7)V99.                           
000064     05  TOT-CLAIM                 PIC 9(7)V99.                           
000065     05  TOT-FACE-TOTAL            PIC 9(5)V99.                           
000066     05  TOT-CLAIM-TOTAL           PIC 9(5)V99.                           
000067     05  TOT-INSURANCE-TOTAL       PIC 9(9)V99.                           
000068     05  TOT-REINSURANCE           PIC 9(7)V99.                           
000069                                                                          
000070 01  TOTALS-FOR-REPORT.                                                   
000071     05  TOTAL-FACE-AMOUNT         PIC 9(7)V99  VALUE ZEROES.             
000072     05  TOTAL-CLAIM-TOTAL         PIC 9(7)V99  VALUE ZEROES.             
000073     05  TOTAL-REINSURANCE        PIC 9(6)     VALUE ZEROES.              
000074     05  TOTAL-COINSURANCE             PIC 9(4)V99  VALUE ZEROES.         
000075     05  TOTAL-INSURANCE           PIC 9(4)V99  VALUE ZEROES.             
000076     05  TOTAL-AMOUNT-DUE          PIC 9(6)V99  VALUE ZEROES.             
000077                                                                          
000078 01  WS-DATE-FIELDS.                                                      
000079     05  WS-YEAR                   PIC 99.                                
000080     05  WS-MONTH                  PIC 99.                                
000081     05  WS-DAY                    PIC 99.                                
000082                                                                          
000083 01  DAY-OF-WEEK-VAL                PIC 9.                                
000084                                                                          
000085 01  HEADING-LINE-ONE.                                                    
000086     05  FILLER                    PIC X(20)  VALUE SPACES.               
000087     05  FILLER                    PIC X(30)                              
000088              VALUE 'Group Claims Daily Totals'.                          
000089     05  FILLER                    PIC X(16)  VALUE SPACES.               
000090     05  HDG-DAY                   PIC X(9).                              
000091     05  FILLER                    PIC X(3)   VALUE ' - '.                
000092     05  HDG-DATE                  PIC X(8).                              
000093     05  FILLER                    PIC X(41)  VALUE SPACES.               
000094     05  FILLER                    PIC X(5)   VALUE 'Page '.              
000095     05  HDG-PAGE-NUMBER           PIC Z9.                                
000096     05  FILLER                    PIC X(3)   VALUE SPACES.               
000097                                                                          
000098 01  HEADING-LINE-TWO.                                                    
000099     05  FILLER                    PIC X(8)  VALUE 'Contract'.            
000100     05  FILLER                    PIC X(38) VALUE SPACES.                
000101     05  FILLER                    PIC X(4)  VALUE 'Date'.                
000102     05  FILLER                    PIC X(5)  VALUE SPACES.                
000103     05  FILLER                    PIC X(3)  VALUE 'Car'.                 
000104     05  FILLER                    PIC X(3)  VALUE SPACES.                
000105     05  FILLER                    PIC X(4)  VALUE 'Days'.                
000106     05  FILLER                    PIC X(6)  VALUE SPACES.                
000107     05  FILLER                    PIC X(6)  VALUE 'CLAIM'.               
000108     05  FILLER                    PIC X(4)  VALUE SPACES.                
000109     05  FILLER                    PIC X(5)  VALUE 'Miles'.               
000110     05  FILLER                    PIC X(2)  VALUE SPACES.                
000111     05  FILLER                    PIC X(7)  VALUE 'Mileage'.             
000112     05  FILLER                    PIC X(2)  VALUE SPACES.                
000113     05  FILLER                    PIC X(7)  VALUE 'Mileage'.             
000114     05  FILLER                    PIC X(2)  VALUE SPACES.                
000115     05  FILLER                    PIC X(9)  VALUE 'Insurance'.           
000116     05  FILLER                    PIC X(6)  VALUE SPACES.                
000117     05  FILLER                    PIC X(6)  VALUE 'Amount'.              
000118     05  FILLER                    PIC X(5)  VALUE SPACES.                
000119                                                                          
000120 01  HEADING-LINE-THREE.                                                  
000121     05  FILLER                    PIC X     VALUE SPACES.                
000122     05  FILLER                    PIC X(6)  VALUE 'Number'.              
000123     05  FILLER                    PIC X(4)  VALUE SPACES.                
000124     05  FILLER                    PIC X(4)  VALUE 'Name'.                
000125     05  FILLER                    PIC X(29)  VALUE SPACES.               
000126     05  FILLER                    PIC X(8)  VALUE 'Returned'.            
000127     05  FILLER                    PIC X(2)  VALUE SPACES.                
000128     05  FILLER                    PIC X(4)  VALUE 'Type'.                
000129     05  FILLER                    PIC X(2)  VALUE SPACES.                
000130     05  FILLER                    PIC X(8)  VALUE 'ClaimDte'.            
000131     05  FILLER                    PIC X(4)  VALUE SPACES.                
000132     05  FILLER                    PIC X(5)  VALUE 'Total'.               
000133     05  FILLER                    PIC X(3)  VALUE SPACES.                
000134     05  FILLER                    PIC X(6)  VALUE 'Driven'.              
000135     05  FILLER                    PIC X(4)  VALUE SPACES.                
000136     05  FILLER                    PIC X(4)  VALUE 'Rate'.                
000137     05  FILLER                    PIC X(4)  VALUE SPACES.                
000138     05  FILLER                    PIC X(5)  VALUE 'Total'.               
000139     05  FILLER                    PIC X(6)  VALUE SPACES.                
000140     05  FILLER                    PIC X(5)  VALUE 'Total'.               
000141     05  FILLER                    PIC X(9)  VALUE SPACES.                
000142     05  FILLER                    PIC X(3)  VALUE 'Due'.                 
000143     05  FILLER                    PIC X(6)  VALUE SPACES.                
000144                                                                          
000145 01  DETAIL-LINE.                                                         
000146     05  DET-CONTRACT-NO           PIC 9B999B99.                          
000147     05  FILLER                    PIC X(3)  VALUE SPACES.                
000148     05  DET-NAME                  PIC X(30).                             
000149     05  FILLER                    PIC X(3)  VALUE SPACES.                
000150     05  DET-RETURN-DATE           PIC X(8).                              
000151     05  FILLER                    PIC X(4)  VALUE SPACES.                
000152     05  DET-CLAIM-TYPE              PIC X.                               
000153     05  FILLER                    PIC X(5)  VALUE SPACES.                
000154     05  DET-DAYS-INSURED           PIC Z9.                               
000155     05  FILLER                    PIC X(5)  VALUE SPACES.                
000156     05  DET-CLAIM-TOTAL           PIC $$,$$$,$$9.99.                     
000157     05  FILLER                    PIC X(3)  VALUE SPACES.                
000158     05  DET-FACE-TOTAL            PIC $$,$$$,$$9.99.                     
000159     05  FILLER                    PIC X(5)  VALUE SPACES.                
000160     05  DET-MILEAGE-RATE          PIC .99.                               
000161     05  FILLER                    PIC X(5)  VALUE SPACES.                
000162     05  DET-MILEAGE-TOTAL         PIC ZZ9.99.                            
000163     05  FILLER                    PIC X(4)  VALUE SPACES.                
000164     05  DET-INSURANCE-TOTAL       PIC ZZ9.99 BLANK WHEN ZERO.            
000165     05  FILLER                    PIC X(4)  VALUE SPACES.                
000166     05  DET-AMOUNT-DUE            PIC Z,ZZ9.99.                          
000167     05  FILLER                    PIC X(5)  VALUE SPACES.                
000168                                                                          
000169 01  TOTAL-DASH-LINE.                                                     
000170     05  FILLER                    PIC X(59)  VALUE SPACES.               
000171     05  FILLER                    PIC X(5)   VALUE ALL '-'.              
000172     05  FILLER                    PIC X(3)   VALUE SPACES.               
000173     05  FILLER                    PIC X(10)  VALUE ALL '-'.              
000174     05  FILLER                    PIC XX     VALUE SPACES.               
000175     05  FILLER                    PIC X(7)   VALUE ALL '-'.              
000176     05  FILLER                    PIC X(11)  VALUE SPACES.               
000177     05  FILLER                    PIC X(8)   VALUE ALL '-'.              
000178     05  FILLER                    PIC XX     VALUE SPACES.               
000179     05  FILLER                    PIC X(8)   VALUE ALL '-'.              
000180     05  FILLER                    PIC XX     VALUE SPACES.               
000181     05  FILLER                    PIC X(10)  VALUE ALL '-'.              
000182     05  FILLER                    PIC X(5)   VALUE SPACES.               
000183                                                                          
000184 01  TOTAL-LINE.                                                          
000185     05  FILLER                    PIC XX     VALUE SPACES.               
000186     05  FILLER                    PIC X(6)   VALUE 'Totals'.             
000187     05  FILLER                    PIC X(51)  VALUE SPACES.               
000188     05  TOT-DAYS-INSUREDTED           PIC Z,ZZ9.                         
000189     05  FILLER                    PIC X(2)   VALUE SPACES.               
000190     05  TOT-DAILY-CLAIM           PIC $$$$,$$9.99.                       
000191     05  FILLER                    PIC XX     VALUE SPACES.               
000192     05  TOT-MILES-DRIVEN          PIC ZZZ,ZZ9.                           
000193     05  FILLER                    PIC X(9)   VALUE SPACES.               
000194     05  TOT-MILEAGE               PIC $$$,$$9.99.                        
000195     05  FILLER                    PIC X      VALUE SPACES.               
000196     05  TOT-INSURANCE             PIC $$,$$9.99.                         
000197     05  FILLER                    PIC X      VALUE SPACES.               
000198     05  TOTAL-CLAIM               PIC $$$$,$$9.99.                       
000199     05  FILLER                    PIC X(5)   VALUE SPACES.               
000200                                                                          
000201 01  FILLER                        PIC X(12)                              
000202         VALUE 'WS ENDS HERE'.                                            
000203                                                                          
000204 PROCEDURE DIVISION.                                                      
000205 000-PREPARE-CLAIM-REPORT.                                                
000206     OPEN INPUT  CLAIM-FILE                                               
000207          OUTPUT PRINT-FILE.                                              
000208     PERFORM 100-GET-WS-DATE.                                             
000209     PERFORM UNTIL REINSURANCE = 'NO'                                     
000210         READ CLAIM-FILE INTO CLAIM-RECORD-IN                             
000211             AT END                                                       
000212                 MOVE 'NO' TO REINSURANCE                                 
000213             NOT AT END                                                   
000214                 PERFORM 200-PROCESS-CLAIM-RECORDS                        
000215         END-READ                                                         
000216     END-PERFORM.                                                         
000217     PERFORM 700-WRITE-CLAIM-TOTALS.                                      
000218     CLOSE CLAIM-FILE                                                     
000219           PRINT-FILE.                                                    
000220     STOP RUN.                                                            
000221                                                                          
000222 100-GET-WS-DATE.                                                         
000223     ACCEPT WS-DATE-FIELDS FROM DATE.                                     
000224     STRING WS-MONTH '/' WS-DAY  '/' WS-YEAR                              
000225         DELIMITED BY SIZE INTO HDG-DATE                                  
000226     END-STRING.                                                          
000227     ACCEPT DAY-OF-WEEK-VAL FROM DAY-OF-WEEK.                             
000228     EVALUATE DAY-OF-WEEK-VAL                                             
000229         WHEN 1 MOVE '   Monday' TO HDG-DAY                               
000230         WHEN 2 MOVE '  Tuesday' TO HDG-DAY                               
000231         WHEN 3 MOVE 'Wednesday' TO HDG-DAY                               
000232         WHEN 4 MOVE ' Thursday' TO HDG-DAY                               
000233         WHEN 5 MOVE '   Friday' TO HDG-DAY                               
000234         WHEN 6 MOVE ' Saturday' TO HDG-DAY                               
000235         WHEN 7 MOVE '   Sunday' TO HDG-DAY                               
000236     END-EVALUATE.                                                        
000237                                                                          
000238 200-PROCESS-CLAIM-RECORDS.                                               
000239     PERFORM 300-COMPUTE-TOT-CLAIM.                                       
000240     IF LINE-COUNT > LINES-PER-PAGE                                       
000241         PERFORM 400-WRITE-HEADING-LINES                                  
000242     END-IF.                                                              
000243     PERFORM 500-WRITE-DETAIL-LINE.                                       
000244     PERFORM 600-INCREMENT-TOTALS.                                        
000245                                                                          
000246 300-COMPUTE-TOT-CLAIM.                                                   
000247     INITIALIZE TOT-BILL-INFORMATION.                                     
000248     PERFORM 320-COMPUTE-CLAIM-TOTAL.                                     
000249     PERFORM 340-DETAIL-LINE.                                             
000250     PERFORM 360-COMPUTE-INSURANCE-TOTAL.                                 
000251     COMPUTE TOT-CLAIM ROUNDED                                            
000252         = TOT-CLAIM-TOTAL + TOT-FACE-TOTAL                               
000253           + TOT-INSURANCE-TOTAL                                          
000254         SIZE ERROR DISPLAY 'SIZE ERROR ON AMOUNT DUE FOR '               
000255             INSURED-CONTRACT-NO                                          
000256     END-COMPUTE.                                                         
000257                                                                          
000258 320-COMPUTE-CLAIM-TOTAL.                                                 
000259     COMPUTE TOT-FACE-AMOUNT                                              
000260         = POLICY-CLAIM-AMOUNT - POLICY-FACE-AMOUNT                       
000261     END-COMPUTE.                                                         
000262     COMPUTE TOT-CLAIM-TOTAL ROUNDED                                      
000263         = TOT-CLAIM * COINSURANCE                                        
000264         SIZE ERROR                                                       
000265           DISPLAY 'COMPUTED BILL EXCESSIVELY LARGE'                      
000266     END-COMPUTE.                                                         
000267                                                                          
000268 340-DETAIL-LINE.                                                         
000269     EVALUATE POLICY-TYPE                                                 
000270         WHEN WHOLE-LIFE MOVE 11 TO COINSURANCE                           
000270         WHEN TERM-LIFE MOVE 11 TO COINSURANCE                            
000270         WHEN UNIVERSAL-LIFE MOVE 11 TO COINSURANCE                       
000275         WHEN OTHER MOVE ZEROES TO COINSURANCE                            
000276     END-EVALUATE.                                                        
000277     MULTIPLY POLICY-CLAIM-AMOUNT BY COINSURANCE                          
000278         GIVING TOT-FACE-TOTAL                                            
000279         SIZE ERROR DISPLAY 'SIZE ERROR ON CLAIM TOTAL'                   
000280     END-MULTIPLY.                                                        
000281                                                                          
000282 360-COMPUTE-INSURANCE-TOTAL.                                             
000283     IF CLAIM-TYPE = 'Y'                                                  
000284         MULTIPLY INSURANCE-RATE BY COINSURANCE                           
000285             GIVING TOT-INSURANCE-TOTAL                                   
000286             SIZE ERROR DISPLAY 'SIZE ERROR ON INSURANCE TOTAL'           
000287         END-MULTIPLY                                                     
000288     END-IF.                                                              
000289                                                                          
000290 400-WRITE-HEADING-LINES.                                                 
000291     MOVE 1 TO LINE-COUNT.                                                
000292     ADD 1 TO PAGE-COUNT.                                                 
000293     MOVE PAGE-COUNT TO HDG-PAGE-NUMBER.                                  
000294     WRITE PRINT-LINE FROM HEADING-LINE-ONE                               
000295         AFTER ADVANCING PAGE.                                            
000296     WRITE PRINT-LINE FROM HEADING-LINE-TWO                               
000297         AFTER ADVANCING 2 LINES.                                         
000298     WRITE PRINT-LINE FROM HEADING-LINE-THREE.                            
000299                                                                          
000300 500-WRITE-DETAIL-LINE.                                                   
000301     MOVE INSURED-CONTRACT-NO TO DET-CONTRACT-NO.                         
000302     INSPECT DET-CONTRACT-NO REPLACING ALL ' ' BY '-'.                    
000303     MOVE 1 TO INSURED-SUB.                                               
000304     MOVE SPACES TO DET-NAME.                                             
000305     STRING INSURED-LAST-NANE DELIMITED BY '  '                           
000306         ', ' DELIMITED BY SIZE                                           
000307         INSURED-FIRST-NANE DELIMITED BY '  '                             
000308         INTO DET-NAME POINTER INSURED-SUB                                
000309     END-STRING.                                                          
000315     STRING POLICY-MONTH '/' POLICY-DAY '/'                               
000316         POLICY-YEAR DELIMITED BY SIZE                                    
000317         INTO DET-RETURN-DATE                                             
000318     END-STRING.                                                          
000319     MOVE POLICY-TYPE TO DET-CLAIM-TYPE.                                  
000320     MOVE COINSURANCE TO DET-DAYS-INSURED.                                
000321     MOVE TOT-FACE-TOTAL TO DET-FACE-TOTAL.                               
000323     MOVE COINSURANCE TO DET-MILEAGE-RATE.                                
000324     MOVE TOT-CLAIM-TOTAL TO DET-CLAIM-TOTAL.                             
000325     MOVE TOT-INSURANCE-TOTAL TO DET-INSURANCE-TOTAL.                     
000326     MOVE TOT-CLAIM-TOTAL TO DET-CLAIM-TOTAL.                             
000327     MOVE TOTAL-CLAIM TO DET-AMOUNT-DUE.                                  
000328     WRITE PRINT-LINE FROM DETAIL-LINE                                    
000329         AFTER ADVANCING 2 LINES.                                         
000330     ADD 1 TO LINE-COUNT.                                                 
000331                                                                          
000332 600-INCREMENT-TOTALS.                                                    
000333     ADD COINSURANCE TO TOTAL-FACE-AMOUNT                                 
000334         SIZE ERROR DISPLAY 'SIZE ERROR ON TOTAL DAYS INSUREDTED'         
000335     END-ADD.                                                             
000336     ADD TOT-FACE-TOTAL TO TOTAL-FACE-AMOUNT                              
000337         SIZE ERROR DISPLAY 'SIZE ERROR ON TOTAL CLAIM'                   
000338     END-ADD.                                                             
000339     ADD TOT-MILES-DRIVEN TO TOTAL-REINSURANCE                            
000340         SIZE ERROR DISPLAY 'SIZE ERROR ON TOTAL MILES DRIVEN'            
000341     END-ADD.                                                             
000342     ADD TOT-CLAIM-TOTAL TO TOTAL-COINSURANCE                             
000343         SIZE ERROR DISPLAY 'SIZE ERROR ON TOTAL MILEAGE'                 
000344     END-ADD.                                                             
000345     ADD TOT-INSURANCE-TOTAL TO TOTAL-INSURANCE                           
000346         SIZE ERROR DISPLAY 'SIZE ERROR ON TOTAL INSURANCE'               
000347     END-ADD.                                                             
000348     ADD TOTAL-CLAIM TO TOT-INSURANCE-TOTAL                               
000349         SIZE ERROR DISPLAY 'SIZE ERROR ON TOTAL AMOUNT DUE'              
000350     END-ADD.                                                             
000351                                                                          
000352 700-WRITE-CLAIM-TOTALS.                                                  
000353     WRITE PRINT-LINE FROM TOTAL-DASH-LINE                                
000354         AFTER ADVANCING 2 LINES.                                         
000355     MOVE TOTAL-FACE-AMOUNT TO TOT-DAYS-INSUREDTED.                       
000356     MOVE TOTAL-CLAIM TO TOT-DAILY-CLAIM.                                 
000357     MOVE TOTAL-REINSURANCE TO TOT-MILES-DRIVEN.                          
000358     MOVE TOTAL-COINSURANCE TO TOT-MILEAGE.                               
000359     MOVE TOTAL-INSURANCE TO TOT-INSURANCE.                               
000360     MOVE TOTAL-AMOUNT-DUE TO TOTAL-CLAIM.                                
000361     WRITE PRINT-LINE FROM TOTAL-LINE.                                    
