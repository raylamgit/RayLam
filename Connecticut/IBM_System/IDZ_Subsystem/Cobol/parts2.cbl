000010 IDENTIFICATION DIVISION.                                         00000010
000020*                                                                 00000020
000030 PROGRAM-ID.    PARTSUPP.                                         00000030
000040*                                                                 00000040
000050 DATE-WRITTEN.  OCTOBER 1ST 1979.                                 00000050
000060*                                                                 00000060
000070 DATE-COMPILED.                                                   00000070
000080*                                                                 00000080
000110******************************************************************00000110
000140*                                                                *00000140
000150*    PROGRAM NUMBER      PART SUPPLIERS                                   
000170*    PROGRAM NAME        DAILY WREHOUSE LOOKUP                   *00000170
000180*                                                                *00000180
000180*                                                                *00000180
000190******************************************************************00000190
000200*                                                                *00000200
000210*                    PROGRAM SPECIFICATIONS                      *00000210
000220*                                                                *00000220
000230******************************************************************00000230
000240*                                                                *00000240
000270*        MOVE  PRODUCTS TO THE WREHOUSE MASTER AS SALES          *00000270
000300*                                                                *00000300
000310******************************************************************00000310
000330*                    SUBROUTINES CALLED                          *00000330
000340*        NAME                                                    *00000340
000360*      DATEVAL                                                   *00000360
000370*      DSNTIAR                                                   *00000370
000380*                                                                *00000380
000390******************************************************************00000390
000847*                                                                 00000847
000848 ENVIRONMENT DIVISION.                                            00000848
000850*                                                                 00000850
000860 CONFIGURATION SECTION.                                           00000860
000870*                                                                 00000870
000910 SPECIAL-NAMES.               C01 IS TOP-OF-PAGE.                 00000910
000920*                                                                 00000920
000930 INPUT-OUTPUT SECTION.                                            00000930
000940*                                                                 00000940
000950 FILE-CONTROL.                                                    00000950
000960*                                                                 00000960
000970     SELECT TRANS-FILE-IN                                         00000970
000980           ASSIGN TO UT-S-YYACTRAS.                               00000980
000990*                                                                 00000990
001000     SELECT PAST-DUE-FILE-OUT                                     00001000
001010           ASSIGN TO UT-S-YYPSTDUE.                               00001010
001020*                                                                 00001020
001030     SELECT OVRHED-FILE-IN                                        00001030
001040           ASSIGN TO UT-S-YYOVRHED.                               00001040
001050*                                                                 00001050
001060     SELECT OVRFLE-FILE-OUT                                       00001060
001070           ASSIGN TO UT-S-YYOVRFLE.                               00001070
001080*                                                                 00001080
001090     SELECT SUPPLR-WREHOUSE-FILE-OUT                              00001090
001100           ASSIGN TO UT-S-YYLCUSPD.                               00001100
001110*                                                                 00001110
001120     SELECT DETAIL-FILE-IN                                        00001120
001130           ASSIGN TO UT-S-YYDETAIL.                               00001130
001140*                                                                 00001140
001150     SELECT WREHOUSE-FILE-IN                                      00001150
001160           ASSIGN TO UT-S-YYCUSMST.                               00001160
001170*                                                                 00001170
001180     SELECT DETAIL-FILE-OUT                                       00001180
001190           ASSIGN TO UT-S-YYNEWDTL.                               00001190
001200*                                                                 00001200
001210     SELECT WREHOUSE-FILE-OUT                                     00001210
001220           ASSIGN TO UT-S-YYNEWMST.                               00001220
001230*                                                                 00001230
001240     SELECT REPORT-FILE-OUT                                       00001240
001250           ASSIGN TO UT-S-YYRPFLE1.                               00001250
001260*                                                                 00001260
001270     SELECT SUPPLR-BAL-OUT                                        00001270
001280           ASSIGN TO UT-S-YYUCLBAL.                               00001280
001290*                                                                 00001290
001300     SELECT PAST-DUE-CRITERIA                                     00001300
001310           ASSIGN TO UT-S-YYCMTBLE.                               00001310
001320*                                                                 00001320
001330     SELECT LIST-FILE-OUT                                         00001330
001340           ASSIGN TO UT-S-YY12LIST.                               00001340
001350*                                                                 00001350
001360     SELECT CST9-FILE-OUT                                         00001360
001370           ASSIGN TO UT-S-WRHOUSE3.                               00001370
001380*                                                                 00001380
001390     SELECT COLLECT-FILE-OUT                                      00001390
001400           ASSIGN TO UT-S-YYUSCOLL.                               00001400
001410*                                                                 00001410
001420     SELECT VSAM-SUPPLR-FILE                                      00001420
001430           ASSIGN TO DA-YYCLMAST                                  00001430
001440           ORGANIZATION IS INDEXED                                00001440
001450           ACCESS IS SEQUENTIAL                                   00001450
001460           RECORD KEY IS SUPPLR-KEY.                              00001460
001470*                                                                 00001470
001480     SELECT WAREHOUS-FILE-IN                                      00001480
001490           ASSIGN TO DA-YYCLACC0.                                 00001490
001500*                                                                 00001500
001510     SELECT WAREHOUS-FILE-OUT                                     00001510
001520           ASSIGN TO DA-YYCLACC1.                                 00001520
001530*                                                                 00001530
001540     SELECT TERMS-FILE                                            00001540
001550           ASSIGN TO UT-S-SUPPLER.                                00001550
001560*                                                                 00001560
001570     SELECT COST-CONTROL-FILE                                     00001570
001580           ASSIGN TO UT-R-CTLFILE.                                00001580
001610*                                                                 00001610
001620     SELECT TOTAL-PAGE                                            00001620
001630           ASSIGN TO UT-S-PRTFILE1.                               00001630
001640*                                                                 00001640
001650     SELECT LADING-FILE                                           00001650
001660           ASSIGN TO UT-S-PRTFILE2.                               00001660
001670*                                                                 00001670
001680     SELECT LOOKUP-CNS-BIAS-FILE                                  00001680
001690           ASSIGN TO UT-S-YYNSBINT.                               00001690
001700*                                                                 00001700
001710     SELECT LADING-COLR-FILE                                      00001710
001720           ASSIGN TO UT-S-LOCATION.                               00001720
001730*                                                                 00001730
001740     SELECT PL-PAST-DUE-CRITERIA                                  00001740
001750           ASSIGN TO UT-S-YYPLPDUE.                               00001750
001760*                                                                 00001760
001770 DATA DIVISION.                                                   00001770
001780*                                                                 00001780
001790*                                                                 00001790
001800 FILE SECTION.                                                    00001800
001810*                                                                 00001810
001820 FD  TRANS-FILE-IN                                                00001820
001830     LABEL RECORDS ARE STANDARD                                   00001830
001840     RECORDING MODE IS F                                          00001840
001850     BLOCK CONTAINS 0 RECORDS                                     00001850
001860     RECORD CONTAINS 113 CHARACTERS                               00001860
001870     DATA RECORD IS BILL-LADING-RECORD.                           00001870
001880*                                                                 00001880
001890 01  BILL-LADING-RECORD                      PIC X(113).          00001890
001900*                                                                 00001900
001910 FD  OVRHED-FILE-IN                                               00001910
001920     LABEL RECORDS ARE STANDARD                                   00001920
001930     RECORDING MODE IS F                                          00001930
001940     BLOCK CONTAINS 0 RECORDS                                     00001940
001950     RECORD CONTAINS 80  CHARACTERS                               00001950
001960     DATA RECORD IS OVRHED-RECORD.                                00001960
001970*                                                                 00001970
001980 01  OVRHED-RECORD                           PIC X(80).           00001980
001990*                                                                 00001990
002000 FD  PAST-DUE-FILE-OUT                                            00002000
002010     LABEL RECORDS ARE STANDARD                                   00002010
002020     RECORDING MODE IS F                                          00002020
002030     BLOCK CONTAINS 0 RECORDS                                     00002030
002040     RECORD CONTAINS 25 CHARACTERS                                00002040
002050     DATA RECORD IS PAST-DUE-RECORD.                              00002050
002060*                                                                 00002060
002070 01  PAST-DUE-RECORD                         PIC X(25).           00002070
002080*                                                                 00002080
002090 FD  OVRFLE-FILE-OUT                                              00002090
002100     LABEL RECORDS ARE STANDARD                                   00002100
002110     RECORDING MODE IS F                                          00002110
002120     BLOCK CONTAINS 0 RECORDS                                     00002120
002130     RECORD CONTAINS 140 CHARACTERS                               00002130
002140     DATA RECORD IS OVRFLE-RECORD.                                00002140
002150*                                                                 00002150
002160 01  OVRFLE-RECORD                           PIC X(140).          00002160
002170*                                                                 00002170
002180 FD  DETAIL-FILE-IN                                               00002180
002190     LABEL RECORDS ARE STANDARD                                   00002190
002200     RECORDING MODE IS F                                          00002200
002210     BLOCK CONTAINS 0 RECORDS                                     00002210
002220     RECORD CONTAINS 140 CHARACTERS                               00002220
002230     DATA RECORD IS DETAIL-RECORD.                                00002230
002240*                                                                 00002240
002250 01  DETAIL-RECORD                           PIC X(140).          00002250
002260*                                                                 00002260
002270 FD  WREHOUSE-FILE-IN                                             00002270
002280     LABEL RECORDS ARE STANDARD                                   00002280
002290     BLOCK CONTAINS 0 RECORDS                                     00002290
002300     RECORD CONTAINS 450 CHARACTERS                               00002300
002310     DATA RECORDS ARE WREHOUSE-RECORD.                            00002310
002320*                                                                 00002320
002330 01  WREHOUSE-RECORD                         PIC X(450).          00002330
002340*                                                                 00002340
002350 FD  PAST-DUE-CRITERIA                                            00002350
002360     LABEL RECORDS ARE STANDARD                                   00002360
002370     BLOCK CONTAINS 0 RECORDS                                     00002370
002380     RECORD CONTAINS 6000 CHARACTERS                              00002380
002390     DATA RECORDS ARE CRITERIA-RECORD.                            00002390
002400*                                                                 00002400
002410 01  CRITERIA-RECORD                         PIC X(6000).         00002410
002420*                                                                 00002420
002430 FD  DETAIL-FILE-OUT                                              00002430
002440     LABEL RECORDS ARE STANDARD                                   00002440
002450     RECORDING MODE IS F                                          00002450
002460     BLOCK CONTAINS 0 RECORDS                                     00002460
002470     RECORD CONTAINS 140 CHARACTERS                               00002470
002480     DATA RECORD IS DETAIL-RECORD-OUT.                            00002480
002490*                                                                 00002490
002500 01  DETAIL-RECORD-OUT                       PIC X(140).          00002500
002510*                                                                 00002510
002520 FD  WREHOUSE-FILE-OUT                                            00002520
002530     LABEL RECORDS ARE STANDARD                                   00002530
002540     BLOCK CONTAINS 0 RECORDS                                     00002540
002550     RECORD CONTAINS 450 CHARACTERS                               00002550
002560     DATA RECORDS ARE WREHOUSE-RECORD-OUT.                        00002560
002570*                                                                 00002570
002580 01  WREHOUSE-RECORD-OUT                     PIC X(450).          00002580
002590*                                                                 00002590
002600 FD  SUPPLR-WREHOUSE-FILE-OUT                                     00002600
002610     LABEL RECORDS ARE STANDARD                                   00002610
002620     BLOCK CONTAINS 0 RECORDS                                     00002620
002630     RECORD CONTAINS 140 CHARACTERS                               00002630
002640     DATA RECORDS ARE SUPPLR-WREHOUSE-RECORD-OUT.                 00002640
002650*                                                                 00002650
002660 01  SUPPLR-WREHOUSE-RECORD-OUT              PIC X(140).          00002660
002670*                                                                 00002670
002680 FD  REPORT-FILE-OUT                                              00002680
002690     LABEL RECORDS ARE STANDARD                                   00002690
002700     BLOCK CONTAINS 0 RECORDS                                     00002700
002710     RECORD CONTAINS 274 CHARACTERS                               00002710
002720     DATA RECORDS ARE REPORT-RECORD.                              00002720
002730*                                                                 00002730
002740 01  REPORT-RECORD                         PIC X(274).            00002740
002750*                                                                 00002750
002760 FD  SUPPLR-BAL-OUT                                               00002760
002770     LABEL RECORDS ARE STANDARD                                   00002770
002780     BLOCK CONTAINS 0 RECORDS                                     00002780
002790     RECORD CONTAINS 230 CHARACTERS                               00002790
002800     DATA RECORDS ARE SUPPLR-BAL-RECORD.                          00002800
002810*                                                                 00002810
002820 01  SUPPLR-BAL-RECORD                       PIC X(230).          00002820
002830*                                                                 00002830
002840 FD  CST9-FILE-OUT                                                00002840
002850     LABEL RECORDS ARE STANDARD                                   00002850
002860     BLOCK CONTAINS 0 RECORDS                                     00002860
002870     RECORD CONTAINS 180 CHARACTERS                               00002870
002880     DATA RECORDS ARE CST9-RECORD.                                00002880
002890*                                                                 00002890
002900 01  CST9-RECORD                             PIC X(180).          00002900
002910*                                                                 00002910
002920 FD  LIST-FILE-OUT                                                00002920
002930     LABEL RECORDS ARE STANDARD                                   00002930
002940     BLOCK CONTAINS 0 RECORDS                                     00002940
002950     RECORD CONTAINS 90 CHARACTERS                                00002950
002960     DATA RECORDS ARE LIST-RECORD.                                00002960
002970*                                                                 00002970
002980 01  LIST-RECORD                             PIC X(90).           00002980
002990*                                                                 00002990
003000 FD  COLLECT-FILE-OUT                                             00003000
003010     LABEL RECORDS ARE STANDARD                                   00003010
003020     BLOCK CONTAINS 0 RECORDS                                     00003020
003030     RECORD CONTAINS 25 CHARACTERS                                00003030
003040     DATA RECORDS ARE COLLECT-RECORD.                             00003040
003050*                                                                 00003050
003060 01  COLLECT-RECORD                         PIC X(25).            00003060
003070*                                                                 00003070
003080 FD  TERMS-FILE                                                   00003080
003090     LABEL RECORDS ARE STANDARD                                   00003090
003100     BLOCK CONTAINS 0 RECORDS                                     00003100
003110     RECORD CONTAINS 71 CHARACTERS                                00003110
003120     DATA RECORD IS TERMS-RECORD.                                 00003120
003130*                                                                 00003130
003140 01  TERMS-RECORD.                                                00003140
003150     05  TERMS-KEY                           PIC 9(3).            00003150
003160     05  FILLER                              PIC X(68).           00003160
003170*                                                                 00003170
003180 FD  VSAM-SUPPLR-FILE                                             00003180
003190     LABEL RECORDS ARE STANDARD                                   00003190
003200     BLOCK CONTAINS 0 RECORDS                                     00003200
003210     RECORD CONTAINS 1200 CHARACTERS                              00003210
003220     DATA RECORD IS VSAM-SUPPLR-RECORD.                           00003220
003230*                                                                 00003230
003240 01  VSAM-SUPPLR-RECORD.                                          00003240
003250     05  FILLER                              PIC X(1).            00003250
003260     05  SUPPLR-KEY.                                              00003260
003270         10  SUPPLR-HQ                       PIC X(3).            00003270
003280         10  SUPPLR-TS                       PIC X(2).            00003280
003290     05  FILLER                              PIC X(1194).         00003290
003300*                                                                 00003300
003310 FD  WAREHOUS-FILE-IN                                             00003310
003320     LABEL RECORDS ARE STANDARD                                   00003320
003330     BLOCK CONTAINS 0 RECORDS                                     00003330
003340     RECORD CONTAINS 58 CHARACTERS                                00003340
003350     DATA RECORD IS WAREHOUS-RECORD-IN.                           00003350
003360*                                                                 00003360
003370 01  WAREHOUS-RECORD-IN                      PIC X(58).           00003370
003380*                                                                 00003380
004110 FD  WAREHOUS-FILE-OUT                                            00004110
003400     LABEL RECORDS ARE STANDARD                                   00003400
003410     BLOCK CONTAINS 0 RECORDS                                     00003410
003420     RECORD CONTAINS 58 CHARACTERS                                00003420
003430     DATA RECORD IS WAREHOUS-RECORD-OUT.                          00003430
003440*                                                                 00003440
003450 01  WAREHOUS-RECORD-OUT                     PIC X(58).           00003450
003460*                                                                 00003460
003470 FD  COST-CONTROL-FILE                                            00003470
003480     LABEL RECORDS ARE STANDARD                                   00003480
003490     RECORD CONTAINS 80 CHARACTERS                                00003490
003500     DATA RECORD IS COST-CONTROL-RECORD.                          00003500
003510*                                                                 00003510
003520 01  COST-CONTROL-RECORD                     PIC X(80).           00003520
003530*                                                                 00003530
003540 FD  TOTAL-PAGE                                                   00003540
003550     LABEL RECORDS ARE STANDARD                                   00003550
003560     BLOCK CONTAINS 0 RECORDS                                     00003560
003570     RECORD CONTAINS 133 CHARACTERS                               00003570
003580     DATA RECORD IS TOTAL-RECORD.                                 00003580
003590 01  TOTAL-RECORD                             PIC X(133).         00003590
003600*                                                                 00003600
003610 FD  LADING-FILE                                                  00003610
003620     LABEL RECORDS ARE STANDARD                                   00003620
003630     BLOCK CONTAINS 0 RECORDS                                     00003630
003640     RECORD CONTAINS 133 CHARACTERS                               00003640
003650     DATA RECORD IS LADING-RECORD.                                00003650
003660 01  LADING-RECORD                            PIC X(133).         00003660
003670*                                                                 00003670
003680 FD  LOOKUP-CNS-BIAS-FILE                                         00003680
003690     LABEL RECORDS ARE STANDARD                                   00003690
003700     BLOCK CONTAINS 0 RECORDS                                     00003700
003710     RECORD CONTAINS 400 CHARACTERS                               00003710
003720     DATA RECORD IS LOOKUP-CNS-BIAS-REC.                          00003720
003730                                                                  00003730
003740 01  LOOKUP-CNS-BIAS-REC               PIC  X(400).               00003740
003750*                                                                 00003750
003760 FD  LADING-COLR-FILE                                             00003760
003770     LABEL RECORDS ARE STANDARD                                   00003770
003780     BLOCK CONTAINS 0 RECORDS                                     00003780
003790     RECORD CONTAINS 7 CHARACTERS                                 00003790
003800     DATA RECORD IS LOCATION-REC.                                 00003800
003810                                                                  00003810
003820 01  LOCATION-REC                         PIC  X(7).              00003820
003830*                                                                 00003830
003840 FD  PL-PAST-DUE-CRITERIA                                         00003840
003850     LABEL RECORDS ARE STANDARD                                   00003850
003860     BLOCK CONTAINS 0 RECORDS                                     00003860
003870     RECORD CONTAINS 80 CHARACTERS                                00003870
003880     DATA RECORD IS RDZ1PDUE-REC.                                 00003880
003890                                                                  00003890
003900 01  RDZ1PDUE-REC                         PIC  X(80).             00003900
003910     EJECT                                                        00003910
003920 WORKING-STORAGE SECTION.                                         00003920
003930*                                                                 00003930
003940 77  COLR-DISPLAY    PIC 9(7).                                    00003940
003950 77  WAREHOUS-CNT-N1 PIC 9(7).                                    00003950
003960 77  WAREHOUS-CNT-N2 PIC 9(7).                                    00003960
003970 77  WAREHOUS-CNT-1  PIC 9(7).                                    00003970
003980 77  WAREHOUS-CNT-2  PIC 9(7).                                    00003980
003990 77  WAREHOUS-CNT-3  PIC 9(7).                                    00003990
004000 01  WW-COPIED-WORKING-STORAGE.                                   00004000
004010     05  FILLER                              PIC X(26)            00004010
004020         VALUE ' INCLUDED I/O RECORD AREAS'.                      00004020
004030*                                                                 00004030
004070     EJECT                                                        00004070
004080*                                                                 00004080
      ******************************************************************        
001200*******                                                           00001200
001300 01  CTLFILE-REC.                                                 00001300
001400         05  CTLFILE-PRIME               PIC  99V999.             00001400
001500         05  CTLFILE-PAST-DUE-DIFF       PIC  99V999.             00001500
001600         05  CTLFILE-ANTICIPATION        PIC  99V999.             00001600
001700         05  CTLFILE-MAX-PAST-DUE        PIC  99V999.             00001700
001800         05  CTLFILE-MARKET-INV          PIC  99V999.             00001800
001900         05  CTLFILE-SLS-LAST-YR-ME      PIC S9(11)V99 COMP-3.    00001900
002000         05  CTLFILE-SLS-LAST-YR-YTD     PIC S9(11)V99 COMP-3.    00002000
002100         05  CTLFILE-SLS-THIS-YR-YTD     PIC S9(11)V99 COMP-3.    00002100
002200         05  CTLFILE-FORECAST-THIS-MO    PIC S9(11)V99 COMP-3.    00002200
002300         05  CTLFILE-FORECAST-YTD        PIC S9(11)V99 COMP-3.    00002300
002400         05  CTLFILE-SALES-THIS-MO       PIC S9(11)V99 COMP-3.    00002400
002500         05  CTLFILE-SALES-NEXT-MO       PIC S9(11)V99 COMP-3.    00002500
002600         05  CTLFILE-PAST-DUE-FLAG       PIC  X.                  00002600
002700         05  CTLFILE-MONTH-END-FLAG      PIC  X.                  00002700
002800         05  CTLFILE-STMT-DATE           PIC  9(7) COMP-3.        00002800
002900 01  CTLFILE-REC-12 REDEFINES CTLFILE-REC.                        00002900
003000         05  CTLFILE-DB-DATE             PIC  9(6).               00003000
003100         05  CTLFILE-ATB-DATE            PIC  9(6).               00003100
003200         05  CTLFILE-LYONS-DATE          PIC  9(6).               00003200
003300         05  FILLER                      PIC  XX.                 00003300
003400         05  CTLFILE-ADV-PSWD-1          PIC  X(9).               00003400
003500         05  CTLFILE-ADV-PSWD-2          PIC  X(9).               00003500
003600         05  CTLFILE-ADV-PSWD-3          PIC  X(9).               00003600
003700         05  CTLFILE-ADV-PSWD-4          PIC  X(9).               00003700
003800         05  CTLFILE-ADV-PSWD-5          PIC  X(9).               00003800
003900         05  CTLFILE-ADV-PSWD-6          PIC  X(9).               00003900
004000         05  CTLFILE-COST-OF-INSRN       PIC  9(3)V999.           00004000
004100 01  CTLFILE-REC-LENGTH              PIC S9(4) COMP VALUE +80.    00004100
004200*******                                                           00004200
004300*******                                                           00004300
      ******************************************************************        
000010****                                                              00000100
000020**** BILL-LADING RECORD                                           00000200
000030****                                                              00000300
000040                                                                  00000400
000050****  COLR PRODUCTS                                               00000500
000060                                                                  00000600
000070 01  BILL-LADING-REC                        PIC  X(90).           00000700
000080                                                                  00000800
000090****  BILL-LADING CODE CHANGE                                     00000900
000100                                                                  00001000
000110 01  PLATFRM-TRANS-CODE-CHANGE-REC          REDEFINES             00001100
000120     BILL-LADING-REC.                                             00001200
000130                                                                  00001300
000140     05  RDZ456-TRAN-CODE                   PIC  9(03).           00001400
000150     05  RDZ456-PRICE-NUMBER                PIC  9(03).           00001500
000160     05  RDZ456-WREHOUSE-NUMBER             PIC  9(07).           00001600
000170     05  RDZ456-SUPPLR-NUMBER               PIC  X(05).           00001700
000180     05  RDZ456-STORE-NUMBER                PIC  9(04).           00001800
000190     05      FILLER                         REDEFINES             00001900
000200         RDZ456-STORE-NUMBER.                                     00002000
000210                                                                  00002100
000220         10  RDZ456-STORE-PACKED            PIC  9(05)    COMP-3. 00002200
000230         10      FILLER                     PIC  X(01).           00002300
000240                                                                  00002400
000250     05  RDZ456-EXPNSE-NUMBER              PIC  9(07).            00002500
000260     05  RDZ456-OLD-TRAN-CODE               PIC  9(03).           00002600
000270     05  RDZ456-NEW-TRAN-CODE               PIC  9(03).           00002700
000280     05  RDZ456-COMMENT                     PIC  X(25).           00002800
000290     05  RDZ456-DISPUTE-CODE                PIC  X.               00002810
000300     05      FILLER                         PIC  X(16).           00002900
000310     05  RDZ456-PROCESSOR                   PIC  X(03).           00003000
000320     05      FILLER                         PIC  X(10).           00003100
000330                                                                  00003200
000340**** 901 - 999  PRCE1 PRCHS-ORD/DECLINES                          00003300
000350                                                                  00003400
000360 01  PLATFRM-PRCE1-PRCHS-ORD-REC           REDEFINES              00003500
000370     BILL-LADING-REC.                                             00003600
000380                                                                  00003700
000390     05  RDZ0001-TRAN-CODE                  PIC  9(03).           00003800
000400     05  RDZ0001-PRICE-NUMBER               PIC  9(03).           00003900
000410     05  RDZ0001-WREHOUSE-NUMBER            PIC  9(07).           00004000
000420     05  RDZ0001-SUPPLR-NUMBER              PIC  X(05).           00004100
000430     05  RDZ0001-APPR-OFFICER               PIC  X(03).           00004200
000440     05  RDZ0001-REASON-CODE                PIC  9(02).           00004300
000450     05  RDZ0001-BILL-DATE                  PIC  9(06).           00004400
000460     05  RDZ0001-SHIP-DATE                  PIC  9(06).           00004500
000470     05  RDZ0001-REQUEST-DATE               PIC  9(06).           00004600
000480     05  RDZ0001-APPR-AMOUNT                PIC  9(06)V99.        00004700
000490     05      FILLER                         REDEFINES             00004800
000500         RDZ0001-APPR-AMOUNT.                                     00004900
000510                                                                  00005000
000520         10  RDZ0001-APPR-AMOUNT-PACKED     PIC S9(07)V99 COMP-3. 00005100
000530         10      FILLER                     PIC  X(03).           00005200
000540                                                                  00005300
000550     05  RDZ0001-ACTION-DATE                PIC  9(06).           00005400
000560     05  RDZ0001-REQUEST-TIME               PIC  9(04).           00005500
000570     05  RDZ0001-ACTION-TIME                PIC  9(04).           00005600
000580     05  RDZ0001-ORDER-NUMBER               PIC  X(07).           00005700
000590     05  RDZ0001-WREHOUSE-LIST              PIC  9(07).           00005800
000600     05  RDZ0001-NET-TERMS-DAYS             PIC  9(03).           00005900
000610     05  RDZ0001-DEC-HLD-REAS-CD            PIC  X(02).           00006000
000610     05  RDZ0001-ORIG-ORDER                 PIC  X(07).           00006000
000620     05      FILLER                         PIC  X(01).           00006000
000630                                                                  00006100
      *    COPY ACCOUNT.                                                        
                                                                                
      * Copybook Location:                                                      
      * C:\education_workspace\IDz Tech Portal\IDz Resources\Education T        
      * raining Resource\IDzClass\copy\ACCOUNT.cpy                              
                                                                                
000640****       ACCOUNT CURRENT ENTRIES                                00006200
000650****       SALES-AVAIL-ADJ FIELDS                                 00006210
000660****       INTEREST FIELDS                                        00006220
000670                                                                  00006300
000680 01  PLATFRM-ACCOUNT-CURRENT-REC            REDEFINES             00006400
000690     BILL-LADING-REC.                                             00006500
000700                                                                  00006600
000710     05  RDZ987-TRAN-CODE                   PIC  9(03).           00006700
000720     05  RDZ987-PRICE-NUMBER                PIC  9(03).           00006800
000730     05      FILLER                         PIC  X(07).           00006900
000740     05  RDZ987-SUPPLR-NUMBER               PIC  X(05).           00007000
000750     05  RDZ987-ACCT-NUMBER                 PIC  9(15) COMP-3.    00007100
000760     05  RDZ987-ENTRY-NUMBER                PIC  9(03).           00007200
000770     05  FILLER                             REDEFINES             00007300
000780         RDZ987-ENTRY-NUMBER.                                     00007400
000790         10  RDZ987-BANK-NUMBER             PIC 9(03).            00007500
000800     05  RDZ987-ENTRY-DATE                  PIC  9(06).           00007600
000810     05  FILLER                             REDEFINES             00007700
000820         RDZ987-ENTRY-DATE.                                       00007800
000830         10  RDZ987-PRICE-DATE              PIC 9(06).            00007900
000840     05      FILLER                         PIC  X(05).           00008000
000850     05  RDZ987-APPR-AMOUNT                 PIC S9(07)V99.        00008100
000860     05      FILLER                         REDEFINES             00008200
000870         RDZ987-APPR-AMOUNT.                                      00008300
000880                                                                  00008400
000890         10  RDZ987-APPR-AMOUNT-PACKED      PIC S9(09)V99 COMP-3. 00008500
000900         10      FILLER                     PIC  X(03).           00008600
000910                                                                  00008700
000920     05  RDZ987-INTEREST                    PIC  9(05)V99.        00008800
000930     05  RDZ987-SALES-AVAIL-ADJ             PIC  9(06)V99.        00008900
000940     05  RDZ987-DESCRIPTION                 PIC  X(20).                   
000950     05      FILLER                         PIC  X(06).           00009000
000960                                                                  00009100
      *    COPY WAREHOUS.                                                       
                                                                                
      * Copybook Location:                                                      
      * C:\education_workspace\IDz Tech Portal\IDz Resources\Education T        
      * raining Resource\IDzClass\copy\WAREHOUS.cpy                             
                                                                                
000980**** MASTER FILE CHANGE - WREHOUSE                                00009300
000990                                                                  00009400
001000 01  PLATFRM-RDZ-WREHOUSE-REC               REDEFINES             00009500
001010     BILL-LADING-REC.                                             00009700
001020                                                                  00009800
001030     05  RDZ001-CUST-TRAN-CODE              PIC  9(03).           00009900
001040     05  RDZ001-WREHOUSE-NUMBER             PIC  9(07).           00010000
001050     05  RDZ001-SUPPLR-NUMBER               PIC  X(05).           00010400
001060     05  RDZ001-DATA.                                             00010500
001070                                                                  00010600
001080         10  RDZ001-FIELD-NO                PIC  9(03).           00010700
001090         10  RDZ001-BUYR-CREDIT            PIC  X(07).            00010800
001100         10      FILLER                     PIC  X(65).           00010900
001110                                                                  00011000
001120     05  RDZ001-038                         REDEFINES             00011100
001130         RDZ001-DATA.                                             00011200
001140                                                                  00011300
001150         10  RDZ001-TERRITORY               PIC  9(02).           00011400
001160         10      FILLER                     PIC  X(03).           00011500
001170         10  RDZ001-PRCE1-LINE             PIC  9(07).            00011600
001180         10  RDZ001-PRCE1-LINE-CODE        PIC  9(01).            00011700
001190         10  RDZ001-ORDER-MAX               PIC  9(06).           00011800
001200         10      FILLER                     PIC  X(06).           00011900
001210         10  RDZ001-MAX-TERM-DAYS           PIC  9(03).           00012000
001220         10  RDZ001-COMMON-ACCT             PIC  9(07).           00012100
001230         10      FILLER                     PIC  X(40).           00012200
001240                                                                  00012300
001250     05  RDZ001-023                         REDEFINES             00012400
001260         RDZ001-DATA.                                             00012500
001270                                                                  00012600
001280         10  RDZ001-23-FIELD-NO             PIC  9(03).           00012700
001290         10  RDZ001-23-BUYR-CREDIT         PIC  X(20).            00012800
001300         10      FILLER                     PIC  X(34).           00012900
001310         10  RDZ001-OFFICER                 PIC  X(03).           00013000
001320         10  RDZ001-TERM-ID                 PIC  X(04).           00013100
001330         10      FILLER                     PIC  X(11).           00013200
001340                                                                  00013300
001350     05  RDZ001-011                         REDEFINES             00013400
001360         RDZ001-DATA.                                             00013500
001370                                                                  00013600
001380         10  RDZ001-NAME1                   PIC  X(30).           00013700
001390         10  RDZ001-NAME2                   PIC  X(30).           00013800
001400         10      FILLER                     PIC  X(15).           00013900
001410                                                                  00014000
001420     05  RDZ001-012                         REDEFINES             00014100
001430         RDZ001-DATA.                                             00014200
001440                                                                  00014300
001450         10  RDZ001-ADDR                    PIC  X(30).           00014400
001460         10  RDZ001-CITY                    PIC  X(20).           00014500
001470         10  RDZ001-STATE                   PIC  X(05).           00014600
001480         10  RDZ001-ZIP                     PIC  9(05).           00014700
001490         10      FILLER                     PIC  X(15).           00014800
001500                                                                  00014900
001510     05  RDZ001-013                         REDEFINES             00015000
001520         RDZ001-DATA.                                             00015100
001530                                                                  00015200
001540         10  RDZ001-STATE-CODE              PIC  9(02).           00015300
001550         10  RDZ001-13-TERRITORY            PIC  9(01).           00015400
001560         10  RDZ001-INDUSTRY-CODE           PIC  9(03).           00015500
001570         10  RDZ001-DANDB-NO                PIC  9(09).           00015600
001580         10  RDZ001-DANDB-RATE              PIC  X(03).           00015700
001590         10  RDZ001-DANDB-DATE              PIC  9(06).           00015800
001600         10  RDZ001-COLR-CODE               PIC  X(01).           00015900
001610         10  RDZ001-LOC1-CODE               PIC  X(01).           00016000
001620         10  RDZ001-TYPE-CODE               PIC  X(01).           00016100
001630         10  RDZ001-FILE-CODE               PIC  X(01).           00016200
001640         10  RDZ001-LIST-NO                 PIC  9(07).           00016300
001650         10  RDZ001-CUST-PRCE1-LIMIT       PIC  9(08).            00016400
001660         10  RDZ001-PRCE1-UNIT             PIC  9(03).            00016500
001670         10  RDZ001-COLLECTOR-CODE          PIC  9(02).           00016600
001680         10      FILLER                     PIC  X(27).           00016700
001690                                                                  00016800
001700**** IKD, KIU MATERIAL ENTRY TO RESERVE                           00016901
001710                                                                  00016600
001720 01  PLATFRM-RESERVE-REC                    REDEFINES             00016700
001730     BILL-LADING-REC.                                             00016800
001740                                                                  00016900
001750     05  FCTRSV-TRAN-CODE                   PIC  9(03).           00017000
001760     05  FCTRSV-PRICE-NUMBER                PIC  9(03).           00017100
001770     05  FILLER                             PIC  X(07).           00017200
001780     05  FCTRSV-SUPPLR-NUMBER               PIC  X(05).           00017300
001790     05  FILLER                             PIC  X(08).           00017400
001800     05  FCTRSV-ZEROES                      PIC  9(03).           00017500
001810     05  FCTRSV-ENTRY-DATE                  PIC  9(06).           00017600
001820     05  FILLER                             PIC  X(06).           00017700
001830     05  FCTRSV-ENTRY-AMOUNT                PIC  9(06)V99.        00017800
001840     05      FILLER                         REDEFINES             00017900
001850         FCTRSV-ENTRY-AMOUNT.                                     00018000
001860                                                                  00018100
001870         10  FCTRSV-ENTRY-AMOUNT-PACKED     PIC S9(07)V99 COMP-3. 00018200
001880         10      FILLER                     PIC  X(03).           00018300
001890                                                                  00018400
001900     05  FCTRSV-INTEREST-AMOUNT             PIC  9(05)V99.        00018500
001910     05  FCTRSV-DESCRIPTION                 PIC  X(20).                   
001920     05      FILLER                         PIC  X(14).           00018600
001930                                                                  00018700
001940**** 100, 101, 102, 103, 104, 105, 106                            00018800
001950**** MASTER FILE CHANGE - SUPPLR                                  00018900
001960                                                                  00019000
001930                                                                  00018700
001940**** 100, 101, 102, 103, 104, 105, 106                            00018800
001970 01  PLATFRM-RDZ-SUPPLR-REC                 REDEFINES             00019100
001980     BILL-LADING-REC.                                             00019200
001990                                                                  00019300
002000     05  RDZ001L-TRAN-CODE                  PIC  9(03).           00019400
002010     05  RDZ001L-SUPPLR-NUMBER              PIC  X(05).           00019500
002020     05      FILLER                         PIC  X(82).           00019600
002030                                                                  00019700
002040**** 190, 290 SALES EXPNSE PRICE HEADER                           00019800
002050                                                                  00019900
002060 01  PLATFRM-SALES-PRICE-HDR                REDEFINES             00020000
002070     BILL-LADING-REC.                                             00020100
002080                                                                  00020200
002090     05  A90ILBH-TRAN-CODE                  PIC  9(03).           00020300
002100     05  A90ILBH-PRICE-NUMBER               PIC  9(03).           00020400
002110     05      FILLER                         PIC  X(07).           00020500
002120     05  A90ILBH-SUPPLR-NUMBER              PIC  X(05).           00020600
002130     05      FILLER                         PIC  X(17).           00020700
002140     05  A90ILBH-PRICE-DATE                 PIC  9(06).           00020800
002150     05  A90ILBH-PRICE-AMOUNT               PIC  9(06)V99.        00020900
002160     05      FILLER                         REDEFINES             00021000
002170         A90ILBH-PRICE-AMOUNT.                                    00021100
002180                                                                  00021200
002190         10  A90ILBH-ENTRY-AMOUNT-PACKED    PIC S9(07)V99 COMP-3. 00021300
002200         10      FILLER                     PIC  X(03).           00021400
002210                                                                  00021500
002220     05      FILLER                         PIC  X(30).           00021600
002230     05  A90ILBH-FILM-NUMBER.                                     00021700
002240                                                                  00021800
002250         10  A90ILBH-FILM-DATE              PIC  9(05).           00021900
002260         10  A90ILBH-FILM-SEQUENCE          PIC  9(06).           00022000
002270                                                                  00022100
002280     05  A90ILBH-NEW-FILM-NUMBER            REDEFINES             00022200
002290         A90ILBH-FILM-NUMBER.                                     00022300
002300                                                                  00022400
002310         10 A90ILBH-NEW-FILM-DATE           PIC 9(3).             00022500
002320         10 A90ILBH-NEW-PRICE-NUMBER        PIC 9(4).             00022600
002330         10 A90ILBH-NEW-SEQUENCE-NUMBER     PIC 9(4).             00022700
002340                                                                  00022800
002350**** 120, 121, 122, 130, 131, 160, 161, 170, 171, 180, 191, 220,  00022900
002360**** 221, 222, 281, 291                                           00023000
002370**** SALES EXPNSE/ PRCE1 RECORD                                   00023100
002380                                                                  00023200
002390 01  PLATFRM-SALES-REC                      REDEFINES             00023300
002400     BILL-LADING-REC.                                             00023400
002410                                                                  00023500
002420     05  A90I-TRAN-CODE                     PIC  9(03).           00023600
002430     05  A90I-PRICE-NUMBER                  PIC  9(03).           00023700
002440     05  A90I-WREHOUSE-NUMBER               PIC  9(07).           00023800
002450     05  A90I-SUPPLR-NUMBER                 PIC  X(05).           00023900
002460     05  A90I-STORE-NUMBER                  PIC  9(04).           00024000
002470     05      FILLER                         REDEFINES             00024100
002480         A90I-STORE-NUMBER.                                       00024200
002490                                                                  00024300
002500         10  A90I-STORE-PACKED              PIC  9(05)    COMP-3. 00024400
002510         10      FILLER                     PIC  X(01).           00024500
002520                                                                  00024600
002530     05  A90I-EXPNSE-NUMBER                PIC  9(07).            00024700
002540     05  A90I-EXPNSE-DATE                  PIC  9(06).            00024800
002550     05  A90I-PRICE-DATE                    PIC  9(06).           00024900
002560     05  A90I-EXPNSE-AMOUNT                PIC  9(06)V99.         00025000
002570     05      FILLER                         REDEFINES             00025100
002580         A90I-EXPNSE-AMOUNT.                                      00025200
002590                                                                  00025300
002600         10  A90I-EXPNSE-AMOUNT-PACKED     PIC S9(07)V99 COMP-3.  00025400
002610         10      FILLER                     PIC  X(03).           00025500
002620                                                                  00025600
002630     05  A90I-EXPNSE-TERM-CODE             PIC  9(03).            00025700
002640     05  A90I-EXTRA-DAYS                    PIC  9(03).           00025800
002650     05  A90I-FREIGHT-AMOUNT                PIC S9(07)V99 COMP-3. 00025900
002660     05      FILLER                         REDEFINES             00026000
002670         A90I-FREIGHT-AMOUNT.                                     00026100
002680                                                                  00026200
002690         10  A90I-FREIGHT-AMOUNT-UNPACKED   PIC S9(03)V99.        00026300
002700                                                                  00026400
002710     05  A90I-RECOURSE-TO-OTHER             PIC  X(01).           00026500
002720     05  A90I-LAST-TRANS-DATE               PIC  9(07)    COMP-3. 00026600
002730     05  A90I-DEPOSIT-DATE                  PIC  9(07)    COMP-3. 00026700
002740     05  A90I-SEQUENCE-NUMBER               PIC  9(03)    COMP-3. 00026800
002750     05  A90I-PROCESSOR                     PIC  X(03).           00026900
002760     05  A90I-REFERENCE-NO                  PIC  9(05).           00027000
002770     05  A90I-FILM-NUMBER.                                        00027100
002780                                                                  00027200
002790         10  A90I-FILM-DATE                 PIC  9(05).           00027300
002800         10  A90I-FILM-SEQUENCE             PIC  9(06).           00027400
002810                                                                  00027500
002820     05  A90I-NEW-FILM-NUMBER            REDEFINES                00027600
002830         A90I-FILM-NUMBER.                                        00027700
002840                                                                  00027800
002850         10 A90I-NEW-FILM-DATE           PIC 9(3).                00027900
002860         10 A90I-NEW-PRICE-NUMBER        PIC 9(4).                00028000
002870         10 A90I-NEW-SEQUENCE-NUMBER     PIC 9(4).                00028100
002880                                                                  00028200
002890**** 885 CHANGE OF TERMS                                          00028300
002900                                                                  00028400
002910 01  PLATFRM-TERM-CHANGE-REC                REDEFINES             00028500
002920     BILL-LADING-REC.                                             00028600
002930                                                                  00028700
002940     05  SUPLR-TRAN-CODE                    PIC  9(03).           00028800
002950     05  SUPLR-PRICE-NUMBER                 PIC  9(03).           00028900
002960     05  SUPLR-WREHOUSE-NUMBER              PIC  9(07).           00029000
002970     05  SUPLR-SUPPLR-NUMBER                PIC  X(05).           00029100
002980     05  SUPLR-STORE-NUMBER                 PIC  9(04).           00029200
002990     05      FILLER                         REDEFINES             00029300
003000         SUPLR-STORE-NUMBER.                                      00029400
003010                                                                  00029500
003020         10  SUPLR-STORE-PACKED             PIC  9(05)    COMP-3. 00029600
003030         10      FILLER                     PIC  X(01).           00029700
003040                                                                  00029800
003050     05  SUPLR-EXPNSE-NUMBER               PIC  9(07).            00029900
003060     05  SUPLR-EXPNSE-DATE                 PIC  9(06).            00030000
003070     05  SUPLR-PRICE-DATE                   PIC  9(06).           00030100
003080     05  SUPLR-EXPNSE-AMOUNT               PIC  9(06)V99.         00030200
003090     05      FILLER                         REDEFINES             00030300
003100         SUPLR-EXPNSE-AMOUNT.                                     00030400
003110                                                                  00030500
003120         10  SUPLR-EXPNSE-AMOUNT-PACKED    PIC S9(07)V99 COMP-3.  00030600
003130         10      FILLER                     PIC  X(03).           00030700
003140                                                                  00030800
003150     05  SUPLR-NEW-EXPNSE-TERM-CODE        PIC  9(03).            00030900
003160     05  SUPLR-EXTRA-DAYS                   PIC  9(03).           00031000
003170     05      FILLER                         PIC  X(16).           00031100
003180     05  SUPLR-ADDITIONAL-DAYS              PIC  9(03).           00031200
003190     05      FILLER                         PIC  X(03).           00031300
003200     05  SUPLR-PROCESSOR                    PIC  X(03).           00031400
003210     05  SUPLR-DEPOSIT-DATE                 PIC  9(07) COMP-3.    00031500
003220     05  SUPLR-CHECK-PRICE-NUMBER           PIC  9(03).           00031600
003230     05  SUPLR-SEQUENCE-NUMBER              PIC  9(03).           00031700
003240                                                                  00031800
003250**** 811, 825 - 875 MATERIAL ENTRY RECORD                         00031900
003260                                                                  00032000
003270 01  PLATFRM-MATERIAL-ENTRY-REC              REDEFINES            00032100
003280     BILL-LADING-REC.                                             00032200
003290                                                                  00032300
003300     05  PRTN0-TRAN-CODE                    PIC  9(03).           00032400
003310     05  PRTN0-PRICE-NUMBER                 PIC  9(03).           00032500
003320     05  PRTN0-WREHOUSE-NUMBER              PIC  9(07).           00032600
003330     05  PRTN0-SUPPLR-NUMBER                PIC  X(05).           00032700
003340     05  PRTN0-STORE-NUMBER                 PIC  9(04).           00032800
003350     05      FILLER                         REDEFINES             00032900
003360         PRTN0-STORE-NUMBER.                                      00033000
003370                                                                  00033100
003380         10  PRTN0-STORE-PACKED             PIC  9(05)    COMP-3. 00033200
004110         10      FILLER                     PIC  X(01).           00033300
003400                                                                  00033400
003410     05  PRTN0-EXPNSE-NUMBER               PIC  9(07).            00033500
003420     05  PRTN0-EXPNSE-DATE                 PIC  9(06).            00033600
003430     05  PRTN0-PRICE-DATE                   PIC  9(06).           00033700
003440     05  PRTN0-EXPNSE-AMOUNT               PIC  9(06)V99.         00033800
003450     05      FILLER                         REDEFINES             00041100
003460         PRTN0-EXPNSE-AMOUNT.                                     00034000
003470                                                                  00034100
003480         10  PRTN0-EXPNSE-AMOUNT-PACKED    PIC S9(07)V99 COMP-3.  00034200
003490         10      FILLER                     PIC  X(03).           00034300
003500                                                                  00034400
003510     05      FILLER                         PIC  X(01).           00034500
003520     05  PRTN0-CHECK-NUMBER                 PIC  9(07).           00034600
003530     05      FILLER                         PIC  X(18).           00034700
003540     05  PRTN0-SEQUENCE-NUMBER              PIC  9(03)    COMP-3. 00034800
003550     05  PRTN0-PROCESSOR                    PIC  X(03).           00034900
003560     05      FILLER                         PIC  X(10).           00035000
003570                                                                  00035100
003580**** 311, 321 COLR FINDER/SHIPPING                                00035200
003590                                                                  00035300
003600 01  PLATFRM-COLR-REC                       REDEFINES             00035400
003610     BILL-LADING-REC.                                             00035500
003620                                                                  00035600
003630     05  WEIGHT1-TRAN-CODE                  PIC  9(03).           00035700
003640     05  WEIGHT1-PRICE-NUMBER               PIC  9(03).           00035800
003650     05  WEIGHT1-WREHOUSE-NUMBER            PIC  9(07).           00035900
003660     05  WEIGHT1-PRICE-DATE                 PIC  9(06).           00036000
003670     05  WEIGHT1-CHECK-NUMBER               PIC  9(07).           00036100
003680     05  WEIGHT1-CHECK-DATE                 PIC  9(06).           00036200
003690     05  WEIGHT1-CHECK-AMOUNT               PIC  9(06)V99.        00036300
003700     05      FILLER                         REDEFINES             00036400
003710         WEIGHT1-CHECK-AMOUNT.                                    00036500
003720                                                                  00036600
003730         10  WEIGHT1-CHECK-AMOUNT-PACKED    PIC S9(07)V99 COMP-3. 00036700
003740         10      FILLER                     PIC  X(03).           00036800
003750                                                                  00036900
003760     05      FILLER                         PIC  X(01).           00037000
003770     05  WEIGHT1-SUPPLR-NUMBER              PIC  X(05).           00037100
003780     05  WEIGHT1-STORE-NUMBER               PIC  9(04).           00037200
003790     05      FILLER                         REDEFINES             00037300
003800         WEIGHT1-STORE-NUMBER.                                    00037400
003810                                                                  00037500
003820         10  WEIGHT1-STORE-PACKED           PIC  9(05)    COMP-3. 00037600
003830         10      FILLER                     PIC  X(01).           00037700
003840                                                                  00037800
003850     05  WEIGHT1-EXPNSE-NUMBER             PIC  9(07).            00037900
003860     05  WEIGHT1-TRANSIT-ROUTING-NUMBER     PIC  9(08).           00038000
003870     05  WEIGHT1-CHECK-ACCOUNT-NUMBER       PIC  9(10).           00038100
003880     05  WEIGHT1-SEQUENCE-NUMBER            PIC  9(03)    COMP-3. 00038200
003890     05  WEIGHT1-PROCESSOR                  PIC  X(03).           00038300
003900     05  WEIGHT1-EARNED-CODE                PIC  9(01).           00038400
003910     05  WEIGHT1-TAKEN-CODE                 PIC  9(01).           00038500
003920     05  WEIGHT1-ANTICIPATION               PIC  9(05)V99 COMP-3. 00038600
003930     05      FILLER                         PIC  X(04).           00038700
003940                                                                  00038800
003970                                                                  00039100
003980 01  PLATFRM-ON-ACCOUNT-REC                 REDEFINES             00039200
003990     BILL-LADING-REC.                                             00039300
004000                                                                  00039400
004010     05  LOCATN-TRAN-CODE                   PIC  9(03).           00039500
004020     05  LOCATN-PRICE-NUMBER                PIC  9(03).           00039600
004030     05  LOCATN-WREHOUSE-NUMBER             PIC  9(07).           00039700
004040     05  LOCATN-PRICE-DATE                  PIC  9(06).           00039800
004050     05  LOCATN-CHECK-NUMBER                PIC  9(07).           00039900
004060     05  LOCATN-CHECK-DATE                  PIC  9(06).           00040000
004070     05  LOCATN-ITEM-AMOUNT                 PIC  9(06)V99.        00040100
004080     05      FILLER                         REDEFINES             00040200
004090         LOCATN-ITEM-AMOUNT.                                      00040300
004100                                                                  00040400
004110         10  LOCATN-ITEM-AMOUNT-PACKED      PIC S9(07)V99 COMP-3. 00040500
004120         10      FILLER                     PIC  X(03).           00040600
004130                                                                  00040700
004140     05      FILLER                         PIC  X(01).           00040800
004150     05  LOCATN-SUPPLR-NUMBER               PIC  X(05).           00040900
004160     05  LOCATN-STORE-NUMBER                PIC  9(04).           00041000
004170     05      FILLER                         REDEFINES             00041100
004180         LOCATN-STORE-NUMBER.                                     00041200
004190                                                                  00041300
004200         10  LOCATN-STORE-PACKED            PIC  9(05)    COMP-3. 00041400
004210         10      FILLER                     PIC  X(01).           00041500
004220                                                                  00041600
004230     05  LOCATN-ITEM-NUMBER                 PIC  9(07).           00041700
004240     05  LOCATN-IRSOUNT-RATE               PIC  9(02)V99.         00041800
004250     05  LOCATN-SEQUENCE-NUMBER             PIC  9(03)    COMP-3. 00041900
004260     05  LOCATN-REFERENCE-NUMBER            PIC  9(05).           00042000
004270     05  LOCATN-HEADING-CODE                PIC  9(01).           00042100
004280     05  LOCATN-REASON-CODE                 PIC  9(02).           00042200
004290     05  LOCATN-CK-AREA.                                          00042300
004300                                                                  00042400
004310         10  LOCATN-CHECK-AMOUNT            PIC  9(06)V99.        00042500
004320         10      FILLER                     PIC  X(01).           00042600
004330                                                                  00042700
004340     05      FILLER                         REDEFINES             00042800
004350         LOCATN-CK-AREA.                                          00042900
004360                                                                  00043000
004370         10  LOCATN-CHECK-AMOUNT-PACKED     PIC S9(07)V99 COMP-3. 00043100
004380         10      FILLER                     PIC  X(01).           00043200
004390         10  LOCATN-PROCESSOR               PIC  X(03).           00043300
004400                                                                  00043400
004410     05  LOCATN-DEPOSIT-DATE                PIC  9(07)    COMP-3. 00043500
004420     05      FILLER                         PIC  X(06).           00043600
004430                                                                  00043700
004440**** 551 - 765                                                    00043800
004450**** ACCOUNT RECEIVABLE ADD BACK                                  00043900
004460                                                                  00044000
004470 01  PLATFRM-CHARGE-BACK-REC                REDEFINES             00044100
004480     BILL-LADING-REC.                                             00044200
004490                                                                  00044300
004500     05  RDZEDIT-TRAN-CODE                  PIC  9(03).           00044400
004510     05  RDZEDIT-PRICE-NUMBER               PIC  9(03).           00044500
004520     05  RDZEDIT-WREHOUSE-NUMBER            PIC  9(07).           00044600
004530     05  RDZEDIT-PRICE-DATE                 PIC  9(06).           00044700
004540     05  RDZEDIT-CHECK-NUMBER               PIC  9(07).           00044800
004550     05      FILLER                         PIC  X(06).           00044900
004560     05  RDZEDIT-ITEM-AMOUNT                PIC  9(06)V99.        00045000
004570     05      FILLER                         REDEFINES             00045100
004580         RDZEDIT-ITEM-AMOUNT.                                     00045200
004590                                                                  00045300
004600         10  RDZEDIT-ITEM-AMOUNT-PACKED     PIC S9(07)V99 COMP-3. 00045400
004610         10      FILLER                     PIC  X(03).           00045500
004620                                                                  00045600
004630     05      FILLER                         PIC  X(01).           00045700
004640     05  RDZEDIT-SUPPLR-NUMBER              PIC  X(05).           00045800
004650     05  RDZEDIT-STORE-NUMBER               PIC  9(04).           00045900
004660     05      FILLER                         REDEFINES             00046000
004670         RDZEDIT-STORE-NUMBER.                                    00046100
004680                                                                  00046200
004690         10  RDZEDIT-STORE-PACKED           PIC  9(05)    COMP-3. 00046300
004700         10      FILLER                     PIC  X(01).           00046400
004710                                                                  00046500
004720     05  RDZEDIT-ITEM-NUMBER                PIC  9(07).           00046600
004730     05  RDZEDIT-OFFSET-CODE                PIC  X(01).           00046700
004740     05      FILLER                         PIC  X(09).           00046800
004750     05  RDZEDIT-DEPOSIT-DATE               PIC  9(06).           00046900
004760     05  RDZEDIT-ARP-PRICE-NUMBER           PIC  9(03)    COMP-3. 00047000
004770     05  RDZEDIT-SEQUENCE-NUMBER            PIC  9(03)    COMP-3. 00047100
004780     05  RDZEDIT-PROCESSOR                  PIC  X(03).           00047200
004790     05      FILLER                         PIC  X(10).           00047300
004800                                                                  00047400
004810 01  PLATFRM-RECORD-LENGTH                  PIC S9(04)    COMP    00047500
004820                                            VALUE +90.            00047600
      ******************************************************************        
000010****                                                              00000100
000020**** SUPPLR WREHOUSE BALANCES                                     00000200
000030****                                                              00000300
000040                                                                  00000400
      *COPY SUPPLIER.                                                           
                                                                                
      * Copybook Location:                                                      
      * C:\education_workspace\IDz Tech Portal\IDz Resources\Education T        
      * raining Resource\IDzClass\copy\SUPPLIER.cpy                             
                                                                                
000050 01  SUPPLR-BAL-REC.                                              00000500
000060                                                                  00000600
000070     05  SUPPLR-BAL-SUPPLR-NO               PIC  X(03).           00000700
000080     05  SUPPLR-BAL-GROSS-REC               PIC S9(09)V99 COMP-3. 00000800
000090     05  SUPPLR-BAL-NET-REC                 PIC S9(09)V99 COMP-3. 00000900
000100                                                                  00001000
000110**** THE DATE IN THIS COPYBOOK IS IN MMDDYY FORMAT                00001100
000120                                                                  00001200
000130     05  SUPPLR-BAL-DATE                    PIC  9(06)    COMP-3. 00001300
000140     05  SUPPLR-BAL-BILLING-11-30          PIC S9(09)V99 COMP-3.  00001400
000150     05  SUPPLR-BAL-BILLING-31-60          PIC S9(09)V99 COMP-3.  00001500
000160     05  SUPPLR-BAL-BILLING-61-90          PIC S9(09)V99 COMP-3.  00001600
000170     05  SUPPLR-BAL-BILLING-91-180         PIC S9(09)V99 COMP-3.  00001700
000180     05  SUPPLR-BAL-BILLING-181-UP         PIC S9(09)V99 COMP-3.  00001800
000190     05  SUPPLR-BAL-DISPUTE                 PIC S9(09)V99 COMP-3. 00001900
000200     05  SUPPLR-BAL-OSD                     PIC S9(09)V99 COMP-3. 00002000
000210     05  SUPPLR-BAL-LOC1                    PIC S9(09)V99 COMP-3. 00002100
000220     05  SUPPLR-BAL-RET-CK                  PIC S9(09)V99 COMP-3. 00002200
000230     05  SUPPLR-BAL-4XX                     PIC S9(09)V99 COMP-3. 00002300
000240     05  SUPPLR-BAL-MATURED-GROSS           PIC S9(09)V99 COMP-3. 00002400
000250     05  SUPPLR-BAL-MATURED-NET             PIC S9(09)V99 COMP-3. 00002500
000260     05  SUPPLR-BAL-BILLING-11-30-G        PIC S9(09)V99 COMP-3.  00002600
000270     05  SUPPLR-BAL-BILLING-31-60-G        PIC S9(09)V99 COMP-3.  00002700
000280     05  SUPPLR-BAL-BILLING-61-90-G        PIC S9(09)V99 COMP-3.  00002800
000290     05  SUPPLR-BAL-BILLING-91-180-G       PIC S9(09)V99 COMP-3.  00002900
000300     05  SUPPLR-BAL-BILLING-181-UP-G       PIC S9(09)V99 COMP-3.  00003000
000310     05  SUPPLR-BAL-DISPUTE-G               PIC S9(09)V99 COMP-3. 00003100
000320     05  SUPPLR-BAL-OSD-G                   PIC S9(09)V99 COMP-3. 00003200
000330     05  SUPPLR-BAL-LOC1-G                  PIC S9(09)V99 COMP-3. 00003300
000340     05  SUPPLR-BAL-CATALOG-002             PIC S9(09)V99 COMP-3. 00003400
000350     05  SUPPLR-BAL-CLAIM-G                 PIC S9(09)V99 COMP-3. 00003500
000360     05  SUPPLR-BAL-WREHOUSE-NO             PIC  9(07)    COMP-3. 00003600
000361     05  SUPPLR-BAL-NET-SHIPPER-11-30       PIC S9(09)V99 COMP-3.         
000363     05  SUPPLR-BAL-NET-SHIPPER-31-60       PIC S9(09)V99 COMP-3.         
000364     05  SUPPLR-BAL-NET-SHIPPER-61-90       PIC S9(09)V99 COMP-3.         
000365     05  SUPPLR-BAL-NET-SHIPPER-91-120      PIC S9(09)V99 COMP-3.         
000366     05  SUPPLR-BAL-NET-SHIPPER-121-180     PIC S9(09)V99 COMP-3.         
000367     05  SUPPLR-BAL-NET-SHIPPER-181-UP      PIC S9(09)V99 COMP-3.         
000368     05  SUPPLR-BAL-NET-SHIPPER             PIC S9(09)V99 COMP-3.         
000369     05  SUPPLR-BAL-UNSHIPPED-APPR          PIC S9(09)V99 COMP-3.         
000420     05  SUPPLR-BAL-UNSHIPPED-BALANCE      PIC S9(09)V99 COMP-3.          
000421     05  SUPPLR-BAL-UNPRCE1ED-4XX          PIC S9(09)V99 COMP-3.          
000431     05  FILLER                             PIC X(15).                    
000440                                                                  00003700
000450 01  SUPPLR-BAL-REC-LENGTH                  PIC S9(04)    COMP    00003800
000451                                            VALUE +230.           00003900
      ******************************************************************        
00010 ****                                                               0000010
000200**** FACTORING OPEN/CLOSED/CST9ING DETAIL FILE                    00000200
000300**** ONLINE OPEN RECORD   - PROCESS CODE = 1                      00000300
000400**** ONLINE CLOSED RECORD - PROCESS CODE = 2                      00000400
000500****                                                              00000500
000600                                                                  00000600
000700 01  WREHOUSE-DETAIL-ALL-REC1.                                    00000700
000800                                                                  00000800
000900     05  UNIT-ALL-BASE-KEY.                                       00000900
001000                                                                  00001000
001100         10  UNIT-ALL-CUST-NO              PIC  9(07)    COMP-3.  00001100
001200         10  UNIT-ALL-SUPPLR-NO            PIC  X(05).            00001200
001300         10  UNIT-ALL-EXPNSE-NO           PIC  9(09)    COMP-3.   00001300
001400         10  UNIT-ALL-STORE-NO             PIC  9(05)    COMP-3.  00001400
001500         10  UNIT-ALL-DELETION-DATE        PIC  9(07)    COMP-3.  00001500
001600                                                                  00001600
001700     05  UNIT-ALL-DEPOSIT-DATE             PIC  9(07)    COMP-3.  00001700
001800     05  UNIT-ALL-PRICE-NUMBER             PIC  9(03).            00001800
001900     05  UNIT-ALL-PRICE-NUMBER-A    REDEFINES                     00001900
002000         UNIT-ALL-PRICE-NUMBER             PIC  X(03).            00002000
002100     05  UNIT-ALL-SEQUENCE-NUMBER          PIC  9(03)    COMP-3.  00002100
002200     05  UNIT-ALL-REC-CD                   PIC  X(01).            00002200
002300     05  UNIT-ALL-TRAN-CD                  PIC  9(03)    COMP-3.  00002300
002400     05  UNIT-ALL-DATE-POSTED              PIC  9(07)    COMP-3.  00002400
002500     05  UNIT-ALL-EXPNSE-DATE             PIC  9(07)    COMP-3.   00002500
002600     05  UNIT-ALL-GROSS-AMT                PIC S9(07)V99 COMP-3.  00002600
002700     05  UNIT-ALL-NET-AMT                  PIC S9(07)V99 COMP-3.  00002700
002800     05  UNIT-ALL-FREIGHT-AMT              PIC S9(07)V99 COMP-3.  00002800
002900     05  UNIT-ALL-FIRST-IRS               PIC S9(03)V99 COMP-3.   00002900
003000     05  UNIT-ALL-FIRST-DAYS               PIC S9(03)    COMP-3.  00003000
003100     05  UNIT-ALL-SECOND-IRS              PIC S9(03)V99 COMP-3.   00003100
003200     05  UNIT-ALL-SECOND-DAYS              PIC S9(03)    COMP-3.  00003200
003300     05  UNIT-ALL-TERMS-TYPE               PIC  X(01).            00003300
003400     05  UNIT-ALL-NET-DAYS                 PIC S9(03)    COMP-3.  00003400
003500     05  UNIT-ALL-EXTRA-DAYS               PIC S9(03)    COMP-3.  00003500
003600     05  UNIT-ALL-EXTRA-DAYS-CD            PIC  X(01).            00003600
003700     05  UNIT-ALL-1ST-IRS-DUE             PIC S9(07)    COMP-3.   00003700
003800     05  UNIT-ALL-1ST-IRS-AMT             PIC S9(05)V99 COMP-3.   00003800
003900     05  UNIT-ALL-2ND-IRS-DUE             PIC S9(07)    COMP-3.   00003900
004000     05  UNIT-ALL-2ND-IRS-AMT             PIC S9(05)V99 COMP-3.   00004000
004100     05  UNIT-ALL-FINAL-DUE-DT             PIC S9(07)    COMP-3.  00004100
004200     05  UNIT-ALL-SLS-DAYS.                                       00004200
004300                                                                  00004300
004400         10  UNIT-ALL-SLS-DAYS-SHORT       PIC S9(03)    COMP-3.  00004400
004500         10  UNIT-ALL-SLS-DAYS-LONG        PIC S9(03)    COMP-3.  00004500
004600                                                                  00004600
004700     05  UNIT-ALL-SEQ-OF-ENTRY             REDEFINES              00004700
004800         UNIT-ALL-SLS-DAYS                 PIC  9(07)    COMP-3.  00004800
004900     05  UNIT-ALL-LAST-TRAN-CD             PIC S9(03)    COMP-3.  00004900
005000     05  UNIT-ALL-LAST-TRAN-DT             PIC S9(07)    COMP-3.  00005000
005100     05  UNIT-ALL-STATUS-CD                PIC  X(01).            00005100
005200     05  UNIT-ALL-REFERENCE-NO             PIC S9(05)    COMP-3.  00005200
005300     05  UNIT-ALL-DELETION-TRAN-CODE       PIC  9(03)    COMP-3.  00005300
005400     05  UNIT-ALL-PRICE-DATE               PIC  9(07)    COMP-3.  00005400
005500     05  UNIT-ALL-CHECK-NUMBER             PIC  9(07)    COMP-3.  00005500
005600     05  UNIT-ALL-CHECK-AMOUNT             PIC S9(07)V99 COMP-3.  00005600
005700     05  UNIT-ALL-PRIOR-TRAN-CODE          PIC  9(03)    COMP-3.  00005700
005800     05  UNIT-PROCESS-CODE                 PIC  9(01).            00005800
005900                                                                  00005900
006000**** 1 IS AN OPEN ITEM                                            00006000
006100**** 2 IS A CLOSED ITEM                                           00006100
006200                                                                  00006200
006300     05  UNIT-CREATED-TODAY-CODE           PIC  9(01).            00006300
006400                                                                  00006400
006500**** 0 IS AN OLD ITEM                                             00006500
006600**** 1 IS CREATED TODAY NEEDS LOOKUP                              00006600
006700                                                                  00006700
006800     05  UNIT-PROCESSOR-CODE               PIC  X(03).            00006800
006900     05  UNIT-EARNED-CODE                  PIC  X(01).            00006900
007000                                                                  00007000
007100**** 0 IS GROSS                                                   00007100
007200**** 1 IS FIRST                                                   00007200
007300**** 2 IS SECOND                                                  00007300
007400**** 3 IS THIRD                                                   00007400
007500**** 4 IS FOURTH                                                  00007500
007600                                                                  00007600
007700     05  UNIT-TAKEN-CODE                   PIC  X(01).            00007700
007800                                                                  00007800
007900**** 0 IS GROSS                                                   00007900
008000**** 1 IS FIRST                                                   00008000
008100**** 2 IS SECOND                                                  00008100
008200**** 3 IS THIRD                                                   00008200
008300**** 4 IS FOURTH                                                  00008300
008400                                                                  00008400
008500     05  UNIT-ALL-CHECK-DATE               PIC  9(07)    COMP-3.  00008500
008600     05  UNIT-ALL-FILM-NUMBER              PIC  9(11)    COMP-3.  00008600
008700     05  UNIT-ALL-THIRD-IRS               PIC S9(03)V99 COMP-3.   00008700
008800     05  UNIT-ALL-THIRD-DAYS               PIC S9(03)    COMP-3.  00008800
008900     05  UNIT-ALL-FOURTH-IRS              PIC S9(03)V99 COMP-3.   00008900
009000     05  UNIT-ALL-FOURTH-DAYS              PIC S9(03)    COMP-3.  00009000
009100     05  UNIT-ALL-TERMS-CODE               PIC  9(03)    COMP-3.  00009100
009200     05  UNIT-ALL-3RD-IRS-DUE             PIC S9(07)    COMP-3.   00009200
009300     05  UNIT-ALL-3RD-IRS-AMT             PIC S9(05)V99 COMP-3.   00009300
009400     05  UNIT-ALL-4TH-IRS-DUE             PIC S9(07)    COMP-3.   00009400
009500     05  UNIT-ALL-4TH-IRS-AMT             PIC S9(05)V99 COMP-3.   00009500
009600     05  UNIT-ALL-ORIGINAL-PRICE-DATE      PIC  9(07)    COMP-3.  00009600
009700     05  UNIT-ALL-ORIGINAL-PRICE-NO        PIC  9(03)    COMP-3.  00009700
009800     05  UNIT-ALL-DAYS-PAST-DUE            PIC S9(03)    COMP-3.  00009800
009800     05  UNIT-ALL-DISPUTE-CODE             PIC X.                 00009810
009800     05  UNIT-ALL-NO-LOCATION-FLAG         PIC X.                 00009820
009900     05  UNIT-ALL-RECOURSE-TO-OTHER        PIC  X(01).            00009900
009900     05      FILLER                         PIC  X(01).           00009910
010000                                                                  00010000
010100**** UNADJUSTED DEDUCTION & RECEIVED ON ACCT                      00010100
010200**** RECORD CODE = 2                                              00010200
010300**** THESE ARE 3XX AND 4XX TRAN CODES.                            00010300
010400                                                                  00010400
010500 01  WREHOUSE-DETAIL-ALL-REC2               REDEFINES             00010500
010600     WREHOUSE-DETAIL-ALL-REC1.                                    00010600
010700                                                                  00010700
010800     05      FILLER                         PIC  X(51).           00010800
010900     05  UNIT-ALL-IRS-AMT                 PIC S9(05)V99 COMP-3.   00010900
011000     05  UNIT-ALL-IRS-RATE                PIC S9(2)V999 COMP-3.   00011000
011100     05  UNIT-ALL-PRINT-CTL1               PIC  X(01).            00011100
011200     05  UNIT-ALL-REASON-CODE              PIC  X(02).            00011200
011300     05  UNIT-ALL-REFERENCE-NO2            PIC S9(05)    COMP-3.  00011300
011400     05      FILLER                         PIC  X(116).          00011400
011500                                                                  00011500
011600**** MISCELLANEOUS RECORDS                                        00011600
011700**** RECORD CODE = 3                                              00011700
011800                                                                  00011800
011900 01  WREHOUSE-DETAIL-ALL-REC3               REDEFINES             00011900
012000     WREHOUSE-DETAIL-ALL-REC1.                                    00012000
012100                                                                  00012100
012200     05      FILLER                         PIC  X(41).           00012200
012300     05  UNIT-ALL-MISC-AMT                 PIC S9(07)V99 COMP-3.  0012300 
012300     05  UNIT-ALL-INTEREST                 PIC S9(07)V99 COMP-3.  0012400 
012300     05  UNIT-ALL-SALES-AVAIL-ADJ          PIC S9(07)V99 COMP-3.  0012401 
012400     05  UNIT-MISC-PROCESSOR               PIC  X(03).            0012402 
012400     05  UNIT-ALL-MISC-STORUNIT            PIC  9(03)    COMP-3.  0012403 
012300     05  UNIT-DESCRIPTION                  PIC  X(20).            0012404 
012400     05      FILLER                         PIC  X(32).           00012405
012400     05  UNIT-PT-CODE                      PIC  X(01).            0012406 
012400*            SPACE = GL RECORD                                    00012407
012400*            I     = GL RECORD                                    00012408
012400*            N     = NO GL RECORD                                 00012409
012300     05  FILLER                             PIC X(20).            00012411
012300     05  UNIT-LOCATION-TYPE                PIC X(10).             0012412 
012300     05  UNIT-ALL-MISC-BANK                PIC 9(03).             0012413 
012300     05  UNIT-ALL-MISC-ACCT                PIC 9(15).             0012414 
012300     05  UNIT-ALL-LC-NUMBER                PIC X(07).             0012415 
012400     05  UNIT-KEYWORD                      PIC  X(10).            0012416 
012400     05      FILLER                         PIC  X(01).           00012420
012500                                                                  00012500
012600 01  UNIT-ALL-REC-LENGTH                   PIC S9(04)    COMP     0012600 
012700                                            VALUE +180.           00012700
      *COPY MASTER.                                                             
                                                                                
      * Copybook Location:                                                      
      * C:\education_workspace\IDz Tech Portal\IDz Resources\Education T        
      * raining Resource\IDzClass\copy\MASTER.cpy                               
                                                                                
      *******************************************************************       
000010***                                                               00000100
000020**** LOOKUP SUPPLR MASTER                                         00000200
000070**** 06/94 REM RE-LO-SEPAC-SUPPLR-ID AND REPLACE THOSE 4 POSITIONS00000500
000070****       WITH 2 FIELDS: RE-LO-PRIMARY-PRCE1-MGR, PIC 9(3) AND   00000500
000070****       RE-LO-IMP-EXP-IND, PIC X. REM RE-LO-SIGN-OFF-LEVELS AND00000500
000070****       REPLACE IT WITH RE-LO-CAN-MEX-IND, PIC X.              00000500
000030**** 09/94 EXPANDED.                                              00000300
000030**** 05/95 REPLACE CAN-MEX-IND WITH EDILOOKUP-IND                 00000300
000030****       ADD IMP-EXP-TYPE, MULTICURRENCY-IND, FCI-CODE          00000300
000030****                                                              00000300
000080 01  SUPPLR-MASTER-REC.                                           00000600
000090                                                                  00000700
000100     05  RE-LO-COMPANY-CODE                 PIC  X(01).           00000800
000110     05  RE-LO-SUPPLR-NO                    PIC  X(05).           00000900
000120     05  RE-LO-COMM-CHG-CODE                PIC  9(01).           00001000
000130     05  RE-LO-SUPPLR-NAME                  PIC  X(30).           00001100
000130     05  RE-LO-SUPPLR-NAME2                 PIC  X(30).           00001100
000140     05  RE-LO-ADDRESS1                     PIC  X(30).           00001200
000150     05  RE-LO-ADDRESS2                     PIC  X(30).           00001300
000160     05  RE-LO-CITY                         PIC  X(20).           00001400
000170     05  RE-LO-STATE                        PIC  X(05).           00001500
000180     05  RE-LO-ZIP-CODE                     PIC  X(09).           00001600
           05  FILLER                             REDEFINES                     
               RE-LO-ZIP-CODE.                                                  
               10  RE-LO-ZIP                      PIC  9(05).                   
               10  RE-LO-EXPANDED-ZIP             PIC  X(04).                   
000190     05  RE-LO-MIN-MNTHLY-COMM              PIC  9(05)    COMP-3. 00001700
000200     05  RE-LO-INDUSTRY-CODE                PIC S9(03)    COMP-3. 00001800
           05  RE-LO-COUNTRY-CODE                 PIC  X(02).                   
           05  RE-LO-CURRENCY-CODE                PIC  X(03).                   
           05  RE-LO-COLLATERAL-CODE              PIC  X(03).                   
           05  RE-LO-SIC                          PIC  9(04)    COMP-3.         
000210     05  RE-LO-PRINT-YY                     PIC  9(01).           00001900
               88  PRINT-YY-DEFAULT               VALUE 0.                      
               88  PRINT-YY-PRINT-AVG-DUE-DATE    VALUE 1.                      
               88  PRINT-YY-SUPPRESS-NO-SALESPRC  VALUE 2.                      
000250     05  RE-LO-PRINT-AGEING                 PIC  9(01).           00002300
               88  PRINT-AGEING-1-LINE-PER-CUST         VALUE 0.                
               88  PRINT-AGEING-DETAIL-AGEING           VALUE 1.                
               88  PRINT-AGEING-PAST-DUE-1-LINE         VALUE 2.                
               88  PRINT-AGEING-ALT-FOR-LOU-LEVY        VALUE 7.                
               88  PRINT-AGEING-NO-PRINT                VALUE 8.                
               88  PRINT-AGEING-MERCHANT-FACTORS        VALUE 9.                
000320     05  RE-LO-PRINT-SALES-SUMMARY          PIC  9(01).           00002900
               88  PRINT-SALES-SUMM-DEFAULT             VALUE 0.                
               88  PRINT-SALES-SUMM-QUARTERLY           VALUE 1.                
               88  PRINT-SALES-SUMM-MONTHLY             VALUE 2.                
               88  PRINT-SALES-SUMM-MTHLY-TRDSTYL       VALUE 3.                
               88  PRINT-SALES-SUMM-QTRLY-TRDSTYL       VALUE 4.                
000380     05  RE-LO-CHARGES-RATE                 PIC S9(2)V999 COMP-3. 00003500
000390     05  RE-LO-PRCE1-RATE                  PIC S9(2)V999 COMP-3.  00003600
000400     05  RE-LO-BASE-DAYS                    PIC S9(03)    COMP-3. 00003700
000410     05  RE-LO-UNIT-DAYS                    PIC S9(03)    COMP-3. 00003800
000420     05  RE-LO-NUMBER-UNITS                 PIC S9(01)    COMP-3. 00003900
000430     05  RE-LO-COMMISION-RATE               PIC      V999 COMP-3. 00004000
000440     05  RE-LO-INT-RATE-DIFFER              PIC S9(2)V999 COMP-3. 00004100
000450     05  RE-LO-CATALOG001-DAYS              PIC S9(03)    COMP-3. 00004200
000460     05  RE-LO-MINIMUM-INV-AMT              PIC S9(01)V99 COMP-3. 00004300
000470     05  RE-LO-CONTRACT-CODE                PIC  9(01).           00004400
000480     05  RE-LO-SPECIAL-AUDIT-AMT            PIC S9(05)    COMP-3. 00004500
000490     05  RE-LO-ACCT-CURR-AGE-CODE           PIC  9(01).           00004600
000500     05  RE-LO-ASSIGNMENT1                  PIC S9(03)    COMP-3. 00004700
000510     05  RE-LO-ASSIGNMENT2                  PIC S9(03)    COMP-3. 00004800
000520     05  RE-LO-ASSIGNMENT3                  PIC S9(03)    COMP-3. 00004900
000530     05  RE-LO-ASSIGNMENT4                  PIC S9(03)    COMP-3. 00005000
000540     05  RE-LO-ASSIGNMENT5                  PIC S9(03)    COMP-3. 00005100
000550     05  RE-LO-LADING-ACCT-NO               PIC  9(07)    COMP-3. 00005200
000560     05  RE-LO-TERMS-1                      PIC  X(22).           00005300
000570     05  FILLER      REDEFINES      RE-LO-TERMS-1.                00005400
000600         10  RE-LO-TERM-CODE-1              PIC  9(03).           00005700
000610         10  RE-LO-TERM-CODE-2              PIC  9(03).           00005800
000620         10  RE-LO-TERM-CODE-3              PIC  9(03).           00005900
000630         10  RE-LO-TERM-CODE-4              PIC  9(03).           00006000
000640         10  RE-LO-TERM-CODE-5              PIC  9(03).           00006100
000650         10  RE-LO-TERM-CODE-6              PIC  9(03).           00006200
000660         10  RE-LO-TERM-CODE-7              PIC  9(03).           00006300
000670         10  FILLER                         PIC  X(01).           00006400
000690     05  RE-LO-TERMS-2                      PIC  X(22).           00006600
000700     05  FILLER      REDEFINES      RE-LO-TERMS-2.                00006700
000730         10  RE-LO-TERM-CODE-8              PIC  9(03).           00007000
000740         10  RE-LO-TERM-CODE-9              PIC  9(03).           00007100
000750         10  RE-LO-TERM-CODE-10             PIC  9(03).           00007200
000760         10  RE-LO-TERM-CODE-11             PIC  9(03).           00007300
000770         10  RE-LO-TERM-CODE-12             PIC  9(03).           00007400
000780         10  RE-LO-TERM-CODE-13             PIC  9(03).           00007500
000790         10  RE-LO-TERM-CODE-14             PIC  9(03).           00007600
000800         10  FILLER                         PIC  X(01).           00007700
000820     05  RE-LO-UNPROCESSED-SALES            PIC S9(11)V99 COMP-3. 00007900
003850     05  RE-LO-MATURING-11-30               PIC S9(09)    COMP-3. 00037900
000830     05  RE-LO-MATURING-31-60               PIC S9(09)    COMP-3. 00008000
000840     05  RE-LO-MATURING-61-90               PIC S9(09)    COMP-3. 00008100
000850     05  RE-LO-MATURING-91-180              PIC S9(09)    COMP-3. 00008200
           05  RE-LO-MATURING-151-180             PIC S9(09)    COMP-3.         
000860     05  RE-LO-MATURING-181                 PIC S9(09)    COMP-3. 00008300
000870     05  RE-LO-MERCHANDISE-DISPUTE          PIC S9(07)    COMP-3. 00008400
000880     05  RE-LO-OSD-CUST-DEDUCT              PIC S9(07)    COMP-3. 00008500
000890     05  RE-LO-SUPPLR-LOC1                  PIC S9(07)    COMP-3. 00008600
000900     05  RE-LO-EFFECTIVE-IR-DATE            PIC  9(07)    COMP-3. 00008700
000910     05  RE-LO-ASSIGN-AMT1                  PIC S9(07)V99 COMP-3. 00008800
000920     05  RE-LO-ASSIGN-AMT2                  PIC S9(07)V99 COMP-3. 00008900
000930     05  RE-LO-ASSIGN-AMT3                  PIC S9(07)V99 COMP-3. 00009000
000940     05  RE-LO-ASSIGN-AMT4                  PIC S9(07)V99 COMP-3. 00009100
000950     05  RE-LO-ASSIGN-AMT5                  PIC S9(07)V99 COMP-3. 00009200
000960     05  RE-LO-TRADE-STYLES                 PIC  X(03).           00009300
000970     05  RE-LO-GROUP-CODE                   PIC  9(03)    COMP-3. 00009400
000980     05  RE-LO-GROUP-CODE-A REDEFINES RE-LO-GROUP-CODE PIC XX.    00009500
000990     05  RE-LO-STOP-ADVANCE-CODE            PIC  9(01).           00009600
001000     05  RE-LO-ADVANCE-PRIORITY             PIC  9(01).           00009700
001010     05  RE-LO-SALES-PRCE1-CODE            PIC  9(01).            00009800
001020     05  RE-LO-TERMINATING-SUPPLR           PIC  9(01).           00009900
001030     05  RE-LO-TERM-MMYY                    PIC  9(04).           00010000
001040     05  RE-LO-SUPPLR-EXPIRE-MMYY           PIC  9(04).           00010100
001050     05  RE-LO-COA-CODE                     PIC  X(01).           00010200
001060     05  RE-LO-CHARGE-BACK-PRINT            PIC  9(01).           00010300
001070     05  RE-LO-TRANSFER-CODE                PIC  X(01).           00010400
               88  TXFER-DETAILS-TO-VALID-SUPPLR       VALUE '*'.               
               88  PAY-COLLECTED-INSRN-FROM-FC024      VALUE 'C'.               
               88  USE-CALC-AMT-ADD-MTD-INT-COMM       VALUE 'A'.               
               88  PAY-COLLECTED-INSRN-FROM-FC017      VALUE 'R'.               
               88  TRANSFER-CALC-AMT-TO-SUPPLR         VALUE 'T'.               
               88  HOLD-TRANSFER                       VALUE 'H'.               
               88  PAY-INTEREST                        VALUE 'I'.               
001170     05  RE-LO-TRANSFER-SUPPLR              PIC  X(05).           00011400
001180     05  RE-LO-AQR-RATING                   PIC  9(02).           00011500
001190     05  RE-LO-AQR-OADV-STATUS-CD           PIC  X(01).                 00
001200     05  RE-LO-AQR-OADV-STRATEGY-CD         PIC  X(01).                 00
001210     05  RE-LO-REVIEW-DATE                  PIC  9(05)    COMP-3. 00011600
001220     05  RE-LO-FISCAL-DATE                  PIC  9(02).           00011700
001230     05  RE-LO-FIN-STMT-CODE                PIC  X(01).           00011800
001240     05  RE-LO-LAST-FIN-STMT-DATE           PIC  9(05)    COMP-3. 00011900
001250     05  RE-LO-WIRE-TRANSFER-FEE            PIC  999V99   COMP-3. 00012000
001260     05  RE-LO-UNPROC-SALES-X876-PCT        PIC S9V99     COMP-3. 00012100
001270     05  RE-LO-FIXED-ADJ-AMT                PIC S9(09)    COMP-3. 00012200
001280     05  RE-LO-SPECIAL-MAX-RESERVE          PIC S9(07)    COMP-3. 00012300
001290     05  RE-LO-RECEIVABLE-RESV-PCT          PIC     S9V99 COMP-3. 00012400
001300     05  RE-LO-LIABILITY-RESV-PCT           PIC     S9V99 COMP-3. 00012500
001310     05  RE-LO-NEW-SALES-AVAIL-PCT          PIC     S9V99 COMP-3. 00012600
001320     05  RE-LO-NEW-SALES-RESV-PCT           PIC     S9V99 COMP-3. 00012700
001330     05  RE-LO-CTLS-EXPIRE-DATE             PIC  9(07)    COMP-3. 00012800
001340     05  RE-LO-OVER-ADV-DIFFER              PIC S9(2)V999 COMP-3. 00012900
001350     05  RE-LO-MAT-INSRN-DIFFER             PIC S9(2)V999 COMP-3. 00013000
001360     05  RE-LO-MAX-INT-RATE                 PIC S9(2)V999 COMP-3. 00013100
001370     05  RE-LO-MIN-INT-RATE                 PIC S9(2)V999 COMP-3. 00013200
001380     05  RE-LO-NEW-SALES-AVAIL              PIC S9(07)V99 COMP-3. 00013300
001390     05  RE-LO-NET-CHGS-THIS-MO             PIC S9(07)V99 COMP-3. 00013400
001400     05  RE-LO-SPECIAL-RESERVE              PIC S9(07)V99 COMP-3. 00013500
001410     05  RE-LO-SUPPLR-RESERVE               PIC S9(09)V99 COMP-3. 00013600
001420     05  RE-LO-SALES-THIS-MO                PIC S9(13)V99 COMP-3. 00013700
001430     05  RE-LO-RETURNS-THIS-MO              PIC S9(07)V99 COMP-3. 00013800
001440     05  RE-LO-LAST-ADV-DATE                PIC  9(07)    COMP-3. 00013900
001450     05  RE-LO-LAST-ADV-AMT                 PIC S9(07)V99 COMP-3. 00014000
001460     05  RE-LO-ADVANCES-THIS-MO             PIC S9(09)V99 COMP-3. 00014100
001470     05  RE-LO-NEXT-MAT-DATE                PIC  9(07)    COMP-3. 00014200
001480     05  RE-LO-NEXT-MAT-AMT                 PIC S9(09)V99 COMP-3. 00014300
001490     05  RE-LO-ADVANCES-OUTSTAND            PIC S9(11)V99 COMP-3. 00014400
001500     05  RE-LO-ADVANCES-TODAY               PIC S9(07)V99 COMP-3. 00014500
001510     05  RE-LO-TOT-OS-SUPPLIERLOC           PIC S9(09)V99 COMP-3. 00014600
001520     05  RE-LO-SALES-YTD                    PIC S9(09)    COMP-3. 00014700
001530     05  RE-LO-RETURNS-YTD                  PIC S9(09)    COMP-3. 00014800
001540     05  RE-LO-PREV-RESV-MO1                PIC S9(07)V99 COMP-3. 00014900
001550     05  RE-LO-PREV-RESV-MO2                PIC S9(07)V99 COMP-3. 00015000
001560     05  RE-LO-PREV-RESV-MO3                PIC S9(07)V99 COMP-3. 00015100
001570     05  RE-LO-CURR-RESV-MO1                PIC S9(07)V99 COMP-3. 00015200
001580     05  RE-LO-CURR-RESV-MO2                PIC S9(07)V99 COMP-3. 00015300
001590     05  RE-LO-CURR-RESV-MO3                PIC S9(07)V99 COMP-3. 00015400
001600     05  RE-LO-FIXED-ADJ-CODE               PIC  9(01).           00015500
001610     05  RE-LO-SPLIT-MAT-INSRN              PIC  9(01).           00015600
001620     05  RE-LO-NEW-SALES-PCT-SW             PIC  9(01).           00015700
001630     05  RE-LO-FE-PERCENT                   PIC  X(01).           00015800
001670     05  RE-LO-TRANSMISSION-CODE            PIC  X(01).           00016200
               88  RECEIVE-CHARGEBACKS-FROM-FC018      VALUE '1'.               
001710     05  RE-LO-ADVANCE-CONTACT              PIC  X(03).           00016600
001720     05  RE-LO-ACCOUNT-OFFICER              PIC  X(03).           00016700
001730     05  RE-LO-ACCOUNT-SPECIALIST           PIC  X(03).           00016800
001740     05  RE-LO-CUST-DED-PERCENT             PIC     S9V99 COMP-3. 00016900
               88  DEFAULT-PERCENT                VALUE +1.00.                  
001800**** GROUP CODE ALTERNATE INDEX KEY POS 488                       00017500
001780     05  RE-LO-ALT1-KEY.                                          00017300
001820         10  RE-LO-ALT1-GROUP-CODE          PIC  9(03)    COMP-3. 00017700
001830         10  RE-LO-ALT1-SUPPLR              PIC  X(05).           00017800
001850     05  RE-LO-LAST-RDZ-DATE                PIC  9(07)    COMP-3. 00018000
001860     05  RE-LO-PRCE1-UNIT-1                PIC  9(03).            00018100
001870     05  RE-LO-PRCE1-UNIT-2                PIC  9(03).            00018200
001880     05  RE-LO-PRCE1-UNIT-3                PIC  9(03).            00018300
003220     05  RE-LO-PRCE1-UNIT-4                PIC  9(03).            00031700
003230     05  RE-LO-PRCE1-UNIT-5                PIC  9(03).            00031800
003240     05  RE-LO-PRCE1-UNIT-6                PIC  9(03).            00031900
003250     05  RE-LO-PRCE1-UNIT-7                PIC  9(03).            00032000
003260     05  RE-LO-PRCE1-UNIT-8                PIC  9(03).            00032100
003270     05  RE-LO-PRCE1-UNIT-9                PIC  9(03).            00032200
003280     05  RE-LO-PRCE1-UNIT-10               PIC  9(03).            00032300
003290     05  RE-LO-PRCE1-UNIT-11               PIC  9(03).            00032400
003300     05  RE-LO-PRCE1-UNIT-12               PIC  9(03).            00032500
           05  RE-LO-PRCE1-UNIT-IND-AT           PIC  9(03).                    
           05  RE-LO-PRCE1-UNIT-IND-LA           PIC  9(03).                    
           05  RE-LO-PRCE1-UNIT-IND-NY           PIC  9(03).                    
001890     05  RE-LO-FE-DAYS-CODE                 PIC  X(01).           00018400
               88  BUSINESS-DAYS                       VALUE 'B'.               
               88  CALENDAR-DAYS                       VALUE 'C'.               
001910     05  RE-LO-FE-LATE-APPLICATION          PIC  X(01).           00018600
001920     05  RE-LO-SALES-STORUNIT               PIC  9(03).           00018700
001930     05  RE-LO-DATE-OPENED                  PIC  9(07)    COMP-3. 00018800
001940     05  RE-LO-WREHOUSE-BILLING-CODE        PIC  X(01).           00018900
001950     05  RE-LO-INVENTORY-CTL-CODE           PIC  X(01).           00019000
001960     05  RE-LO-COMMISSION-GROUP             PIC  X(01).           00019100
001970     05  RE-LO-INTEREST-PAID                PIC S9(09)V99 COMP-3. 00019200
001980     05  RE-LO-INTEREST-CHARGED             PIC S9(09)V99 COMP-3. 00019300
001990     05  RE-LO-REGULAR-COMMISSION           PIC S9(09)V99 COMP-3. 00019400
002000     05  RE-LO-ACTUAL-COMM-YTD              PIC S9(09)V99 COMP-3. 00019500
002010     05  RE-LO-MINIMUM-ANNUAL-COMM          PIC S9(09)V99 COMP-3. 00019600
002020     05  RE-LO-COMM-DIFFERENCE-YTD          PIC S9(09)V99 COMP-3. 00019700
002060     05  RE-LO-MTD-CHARGES                  PIC S9(09)V99 COMP-3. 00020100
002070     05  RE-LO-UNPROCESSED-LOC1             PIC S9(07)    COMP-3. 00020200
002080     05  RE-LO-SHIPING-METHOD               PIC  X(05).           00020300
002110**** GROUP CODE ALTERNATE INDEX KEY POS 571                       00020600
002090     05  RE-LO-ALT3-KEY.                                          00020400
002130         10  RE-LO-ALT3-MAIL-TO-CODE        PIC  X(03).           00020800
002140         10  RE-LO-ALT3-SUPPLR              PIC  X(05).           00020900
002180**** GROUP CODE ALTERNATE INDEX KEY POS 579                       00021300
002160     05  RE-LO-ALT4-KEY.                                          00021100
002200         10  RE-LO-ALT4-RELATED-SUPPLR      PIC  X(03).           00021500
002210         10  RE-LO-ALT4-SUPPLR              PIC  X(05).           00021600
002230     05  RE-LO-INTEREST-BASIS-CODE          PIC  X(01).           00021800
               88  INTEREST-BASIS-MONTHLY              VALUE 'M'.               
               88  INTEREST-BASIS-DAILY                VALUE 'D'.               
002250     05  RE-LO-INTEREST-PAID-RATE-CODE      PIC  X(01).           00022000
               88  INTEREST-PAID-RATE-PRIME            VALUE 'P'.               
               88  INTEREST-PAID-RATE-COMM-MIA         VALUE 'M'.               
               88  INTEREST-PAID-RATE-MIA-PREM         VALUE 'S'.               
002270     05  RE-LO-MIN-ANNUAL-ASSESSMENT        PIC  X(01).           00022200
               88  MIN-ANNUAL-ASSESS-QTRLY             VALUE 'Q'.               
               88  MIN-ANNUAL-ASSESS-MTHLY             VALUE 'M'.               
               88  MIN-ANNUAL-ASSESS-ANN               VALUE 'A'.               
               88  MIN-ANNUAL-ASSESS-NONE              VALUE 'N'.               
002290     05  RE-LO-COMM-YEAR-BASIS-CODE         PIC  X(01).           00022400
               88  COMM-YEAR-BASIS-CONTRACT            VALUE '1'.               
               88  COMM-YEAR-BASIS-CALENDAR            VALUE '0'.               
002310     05  RE-LO-NEW-BUSINESS-CODE            PIC  X(03).           00022600
002320     05  RE-LO-LPI-CODE                     PIC  X(01).           00022700
               88  LPI-WHOSL-INTEREST                  VALUE 'C'.               
               88  LPI-INTEREST                        VALUE 'I'.               
               88  LPI-ADJUST-COLLECT-DAYS             VALUE 'A'.               
               88  LPI-NOT-APPLICABLE                  VALUE 'N'.               
               88  LPI-RETAIL                          VALUE 'R'.               
               88  LPI-WHOLESALE                       VALUE 'W'.               
               88  LPI-EXTEND                          VALUE 'X'.               
002370     05  RE-LO-LPI-PRINT-CODE               PIC  X(01).           00023200
               88  LPI-PRINT-PAPER                     VALUE 'Y'.               
002390     05  RE-LO-CBI-CODE                     PIC  X(01).           00023400
               88  CBI-DOLLAR-DAYS                     VALUE 'D'.               
               88  CBI-INTEREST                        VALUE 'I'.               
               88  CBI-EXTEND                          VALUE 'X'.               
002410     05  RE-LO-PHONE-COUNTRY                PIC  X(03).           00023600
002410     05  RE-LO-PHONE-AREA-CITY              PIC  X(03).           00023600
002410     05  RE-LO-PHONE-NUMBER                 PIC  X(09).           00023600
002420     05  RE-LO-CHARGE-BACK-DAYS             PIC S9(03)    COMP-3. 00023700
002430     05  RE-LO-ACCT-OFFICER-SECOND          PIC  X(03).           00023800
002440     05  RE-LO-NEW-INT-RATE                 PIC S99V999   COMP-3. 00023900
002450     05  RE-LO-NEW-MIA-RATE                 PIC S99V999   COMP-3. 00024000
002460     05  RE-LO-NEW-OVER-ADV-RATE            PIC S99V999   COMP-3. 00024100
002470     05  RE-LO-DR-COMMISSION-RATE           PIC S99V999   COMP-3. 00024200
002480     05  RE-LO-SPEC-CUST-COMM-RATE-A        PIC S99V999   COMP-3. 00024300
002490     05  RE-LO-SPEC-CUST-COMM-RATE-B        PIC S99V999   COMP-3. 00024400
002500     05  RE-LO-SPEC-CUST-COMM-RATE-C        PIC S99V999   COMP-3. 00024500
002510     05  RE-LO-SPEC-CUST-COMM-RATE-D        PIC S99V999   COMP-3. 00024600
002520     05  RE-LO-SPEC-CUST-COMM-RATE-E        PIC S99V999   COMP-3. 00024700
002530     05  RE-LO-SPEC-CUST-COMM-RATE-F        PIC S99V999   COMP-3. 00024800
002540     05  RE-LO-SPEC-CUST-COMM-RATE-G        PIC S99V999   COMP-3. 00024900
002550     05  RE-LO-SPEC-CUST-COMM-RATE-H        PIC S99V999   COMP-3. 00025000
002560     05  RE-LO-SPEC-CUST-COMM-RATE-I        PIC S99V999   COMP-3. 00025100
002570     05  RE-LO-SPEC-CUST-COMM-RATE-J        PIC S99V999   COMP-3. 00025200
002580     05  RE-LO-COMM-HANDLING-CHARGE         PIC S999V99   COMP-3. 00025300
002590     05  RE-LO-TERMS-3                      PIC  X(22).           00025400
002600     05  FILLER      REDEFINES      RE-LO-TERMS-3.                00025500
002630         10  RE-LO-TERM-CODE-15             PIC  9(03).           00025800
002640         10  RE-LO-TERM-CODE-16             PIC  9(03).           00025900
002650         10  RE-LO-TERM-CODE-17             PIC  9(03).           00026000
002660         10  RE-LO-TERM-CODE-18             PIC  9(03).           00026100
002670         10  RE-LO-TERM-CODE-19             PIC  9(03).           00026200
002680         10  RE-LO-TERM-CODE-20             PIC  9(03).           00026300
002690         10  RE-LO-TERM-CODE-21             PIC  9(03).           00026400
002700         10  FILLER                         PIC  X(01).           00026500
002720     05  RE-LO-TERMS-4                      PIC  X(22).           00026700
002730     05  FILLER      REDEFINES      RE-LO-TERMS-4.                00026800
002760         10  RE-LO-TERM-CODE-22             PIC  9(03).           00027100
002770         10  RE-LO-TERM-CODE-23             PIC  9(03).           00027200
002780         10  RE-LO-TERM-CODE-24             PIC  9(03).           00027300
002790         10  RE-LO-TERM-CODE-25             PIC  9(03).           00027400
002800         10  RE-LO-TERM-CODE-26             PIC  9(03).           00027500
002810         10  RE-LO-TERM-CODE-27             PIC  9(03).           00027600
002820         10  RE-LO-TERM-CODE-28             PIC  9(03).           00027700
002830         10  FILLER                         PIC  X(01).           00027800
002850     05  RE-LO-PRIMARY-PRCE1-MGR           PIC  9(03).            00028000
002850     05  RE-LO-IMP-EXP-IND                  PIC  X(01).           00028000
002860     05  RE-LO-AMT0-NUMBER                  PIC  9(09)    COMP-3. 00028100
002870     05  RE-LO-REBILL-LIMIT                 PIC  9(03).           00028200
002880     05  RE-LO-TERMINAL-ACCESS-FEE          PIC  9(03).           00028300
002940     05  RE-LO-TRANSMISSION-SUPPLR          PIC  9(01).           00028900
               88  TRANSMIT-NO                         VALUE 0.                 
               88  TRANSMIT-YES                        VALUE 1.                 
002950     05  RE-LO-TRANSMISSION-ORDERS          PIC  9(01).           00029000
002960     05  RE-LO-TRANSMISSION-SALES           PIC  9(01).           00029100
002970     05  RE-LO-COMMISSION-RATE-1            PIC  9(2)V999 COMP-3. 00029200
002980     05  RE-LO-COMMISSION-DOLLAR-1          PIC S9(07)    COMP-3. 00029300
002990     05  RE-LO-COMMISSION-RATE-2            PIC  9(2)V999 COMP-3. 00029400
003000     05  RE-LO-COMMISSION-DOLLAR-2          PIC S9(07)    COMP-3. 00029500
003010     05  RE-LO-COMMISSION-RATE-3            PIC  9(2)V999 COMP-3. 00029600
003020     05  RE-LO-COMMISSION-DOLLAR-3          PIC S9(07)    COMP-3. 00029700
003030     05  RE-LO-COMMISSION-RATE-4            PIC  9(2)V999 COMP-3. 00029800
003040     05  RE-LO-COMMISSION-DOLLAR-4          PIC S9(07)    COMP-3. 00029900
003050     05  RE-LO-LC-INVENTORY                 PIC S9(09)V99 COMP-3. 00030000
003060     05  RE-LO-LC-PIECE-GOODS               PIC S9(09)V99 COMP-3. 00030100
003070     05  RE-LO-LC-FINISHED-GOODS            PIC S9(09)V99 COMP-3. 00030200
003080     05  RE-LO-LC-LIMIT                     PIC S9(11)    COMP-3. 00030300
003090     05  RE-LO-SUPPLR-LINE                  PIC S9(11)    COMP-3. 00030400
003100     05  RE-LO-LC-ACCEPTANCES-OPEN          PIC S9(09)V99 COMP-3. 00030500
003110     05  RE-LO-LC-DOCUMENTS-CST9ING         PIC S9(09)V99 COMP-3. 00030600
003120     05  RE-LO-LC-OTHER-COLLATERAL          PIC S9(11)    COMP-3. 00030700
003130     05  RE-LO-LINE-EXPIRE-DATE             PIC  9(07)    COMP-3. 00030800
003140     05  RE-LO-INVENTORY-RESERVE            PIC      9V99 COMP-3. 00030900
003150     05  RE-LO-LC-PG-PERCENT                PIC      9V99 COMP-3. 00031000
003160     05  RE-LO-LC-FG-PERCENT                PIC      9V99 COMP-3. 00031100
003170     05  RE-LO-LIQUID-COLL-RESERVE          PIC      9V99 COMP-3. 00031200
003180     05  RE-LO-LC-GROSS-MARGIN              PIC      9V99 COMP-3. 00031300
003190     05  RE-LO-LC-DUTY-FREIGHT              PIC      9V99 COMP-3. 00031400
003200     05  RE-LO-LADING-COLR-RESERVE          PIC      9V99 COMP-3. 00031500
003210     05  RE-LO-SUPPLR-LOC1-RESERVE          PIC      9V99 COMP-3. 00031600
003310     05  RE-LO-APPROVING-AE                 PIC  X(03).           00032600
003320     05  RE-LO-LIQUID-COLLATERAL            PIC  S9(09) COMP-3.   00032700
003330     05  RE-LO-CHARGESS-PD-MTD            PIC  S9(09)V99 COMP-3.  00032800
003340     05  RE-LO-LAST-AUDIT-DATE              PIC  9(04).           00032900
003350     05  RE-LO-MULTIPLE-FACTOR-CODE         PIC  X(01).           00033000
           05  RE-LO-FACTOR-CODE                  PIC  X(07).                   
003360     05  RE-LO-MTD-LC-SHIPINGS            PIC  S9(09)V99 COMP-3.  00033100
003370     05  RE-LO-MTD-ACCEPT-PAID            PIC  S9(09)V99 COMP-3.  00033200
003380     05  RE-LO-MTD-INT-COMM               PIC  S9(09)V99 COMP-3.  00033300
003390     05  RE-LO-INVENTORY-DATE               PIC  9(4).            00033400
003400     05  RE-LO-YY-EXTRA-COPY                PIC  X.               00033500
003410     05  RE-LO-AGE-EXTRA-COPY               PIC  X.               00033600
003420     05  RE-LO-LIABILITY-EXTRA-COPY         PIC  X.               00033700
003430     05  RE-LO-STATEMENT-EXTRA-COPY         PIC  X.               00033800
003440     05  RE-LO-MANUFACTURING-LOC1           PIC  X.               00033900
003450     05  RE-LO-FAX-COUNTRY                  PIC  X(03).           00034000
003450     05  RE-LO-FAX-AREA-CITY                PIC  X(03).           00034000
003450     05  RE-LO-FAX-NUMBER                   PIC  X(09).           00034000
003460     05  RE-LO-RELATIONSHIPS                PIC  9(7) COMP-3.     00034100
003470     05  RE-LO-PREV-NEW-SALES-AVAIL-PCT     PIC S9V99 COMP-3.     00034200
003480     05  RE-LO-TERMS-5                      PIC  X(22).           00034300
003490     05  FILLER      REDEFINES      RE-LO-TERMS-5.                00034400
003520         10  RE-LO-TERM-CODE-29             PIC  9(03).           00034700
003530         10  RE-LO-TERM-CODE-30             PIC  9(03).           00034800
003540         10  RE-LO-TERM-CODE-31             PIC  9(03).           00034900
003550         10  RE-LO-TERM-CODE-32             PIC  9(03).           00035000
003560         10  RE-LO-TERM-CODE-33             PIC  9(03).           00035100
003570         10  RE-LO-TERM-CODE-34             PIC  9(03).           00035200
003580         10  RE-LO-TERM-CODE-35             PIC  9(03).           00035300
 03590         10  FILLER                         PIC  X(01).           00035400
003610     05  RE-LO-TERMS-6                      PIC  X(22).           00035600
003620     05  FILLER      REDEFINES      RE-LO-TERMS-6.                00035700
003650         10  RE-LO-TERM-CODE-36             PIC  9(03).           00036000
003660         10  RE-LO-TERM-CODE-37             PIC  9(03).           00036100
003670         10  RE-LO-TERM-CODE-38             PIC  9(03).           00036200
003680         10  RE-LO-TERM-CODE-39             PIC  9(03).           00036300
003690         10  RE-LO-TERM-CODE-40             PIC  9(03).           00036400
003700         10  RE-LO-TERM-CODE-41             PIC  9(03).           00036500
003710         10  RE-LO-TERM-CODE-42             PIC  9(03).           00036600
003720         10  FILLER                         PIC  X(01).           00036700
003740     05  RE-LO-COMMISSION-SALES-MTD         PIC S9(9)V99  COMP-3. 00036900
003750     05  RE-LO-COMMISSION-SALES-YTD         PIC S9(9)V99  COMP-3. 00037000
003760     05  RE-LO-EDILOOKUP-IND             PIC  X.                  00037100
003770     05  RE-LO-BILL-N-HOLD-SALES            PIC  X.               00037200
003780     05  RE-LO-ALT5-KEY.                                          000373  
003790         10  RE-LO-ALT5-ADVANCE-GROUP-CODE  PIC X(3).             00037400
003800         10  RE-LO-ALT5-SUPPLR              PIC X(5).             00037400
003810     05  RE-LO-ORDER-AUTO-APPROVAL          PIC S9(5)  COMP-3.    00037500
003820     05  RE-LO-CHARGE-OFF-DEDUCTABLE        PIC S9(5)  COMP-3.    00037600
003830     05  RE-LO-MATURED-NET-SALES            PIC S9(9)V99  COMP-3. 00037700
003840     05  RE-LO-TERM-ACCESS-FEE              PIC  9(04).           00037800
003860     05  RE-LO-INVENTORY-LOAN-LIMIT         PIC S9(11)    COMP-3. 00038000
003870     05  RE-LO-MTD-DF-CF                    PIC S9(9)V99  COMP-3. 00038100
003880     05  RE-LO-CANADIAN-ACCORD-IND          PIC X(01).            00038200
003890     05  RE-LO-INT-ON-COMM-IND              PIC X(01).            00038201
003900     05  RE-LO-TRANSMISSION-IND             PIC X(01).            00038202
003910     05  RE-LO-CS-NUMBER-IND                PIC X(01).            00038203
003920     05  RE-LO-NON-NOTIF-CR-IND             PIC X(01).            00038204
003930     05  RE-LO-NON-NOTIF-AR-IND             PIC X(01).            00038205
003940     05  RE-LO-LOCATION-CODE                PIC 9(01).            00038210
           05  RE-LO-ORDER-RATE                   PIC S9V9(06)  COMP-3.         
           05  RE-LO-TAX-ID                       PIC 9(10)     COMP-3.         
           05  RE-LO-DO-NOT-POST                  PIC X(01).                    
           05  RE-LO-SPEC-SRCHGE-MTD              PIC S9(07)V99 COMP-3.         
           05  RE-LO-SPEC-SRCHGE-YTD              PIC S9(09)V99 COMP-3.         
           05  RE-LO-DO-NOT-OWN-ASSET             PIC X(01).                    
           05  RE-LO-SALES-TAX-IND                PIC X(01).                    
           05  RE-LO-EFF-START-DATE               PIC 9(08)     COMP-3.         
           05  RE-LO-EFF-COMMISSION-DATE          PIC 9(08)     COMP-3.         
           05  RE-LO-AV-LAST-VERIFY-DT            PIC 9(08)     COMP-3.         
           05  RE-LO-AV-FORCE-OR-EXCL-VERIFY      PIC X(01).                    
           05  RE-LO-CUST-STAT-PRINT-CODE         PIC X(01).                    
           05  RE-LO-AV-ADV-GP-STAT-CHG-DT        PIC X(08).                    
           05  RE-LO-AV-ADV-GP-STAT-CHG-BY        PIC X(08).                    
           05  RE-LO-IMP-EXP-TYPE                 PIC 9(01).                    
           05  RE-LO-MULTI-CURRENCY-IND           PIC X(01).                    
           05  RE-LO-FCI-CODE                     PIC X(07).                    
           05  RE-LO-FILLER                       PIC X(41).                    
003950                                                                  00038300
003960 01  RE-LO-REC-LENGTH                       PIC S9(4)     COMP    00038400
003970                                            VALUE +1200.          00038500
      ******************************************************************        
000010****                                                              00000100
000020**** LOOKUP CROSS REFERENCE FILE                                  00000200
000030****                                                              00000210
000040                                                                  00000220
000050 01  LIST-ACCT-REC.                                               00000300
000060                                                                  00000310
000070     05  LIST-KEY.                                                00000400
000080         10  LIST-SUPPLR                    PIC  X(03).           00000500
000090         10  LIST-COMMON-NO                 PIC  X(15).           00000600
000100         10  LIST-SEQUENCE-NO               PIC  999.             00000601
000110                                                                  00000610
000120     05  LIST-ALT-KEY.                                            00000700
000130         10  LIST-SUPPLR-ALT                PIC  X(03).           00000730
000140         10  LIST-CS-NO                     PIC  9(07)    COMP-3. 00000731
000150         10  LIST-PRIORITY-CODE             PIC  9(5).            00000732
000160                                                                  00000740
000170     05  LIST-ALT-KEY-2.                                          00000750
000180         10  LIST-CS-NO-2                   PIC  9(07)    COMP-3. 00000780
000190         10  LIST-SEQ-NO-2                  PIC  9(9).            00000790
000200                                                                  00000791
000210     05  LIST-CROSS-REF-NO                  PIC  9(07)    COMP-3. 00000800
000220     05  LIST-TERRITORY-CODE                PIC  9(01).           00000900
000230     05  LIST-PERMANENT-PRCE1-UNIT         PIC  9(03)    COMP-3.  00001000
000240     05  LIST-FILE-PRCE1-CODE              PIC  X(01).            00001500
000250     05  FILLER                             PIC  X(04).           00001600
000260                                                                  00004100
000270 01  LIST-ACCT-REC-LENGTH                   PIC S9(4)     COMP    00004200
000280                                            VALUE +58.            00004300
      ******************************************************************        
000100*******           LOOKUP SUPPLR  PRCE1-UNIT.                      00000100
000200*******                                                           00000200
000300*******                                                           00000300
000400 01   PRCE1-UNIT-RECORD.                                          00000400
000500     05   PRCE1-UNIT-TABLE-C OCCURS 1000 TIMES.                   00000500
000600         10   PRCE1-UNIT-TABLE-T OCCURS 3 TIMES.                  00000600
000700             15   PRCE1-UNIT          PIC 99.                     00000700
000800                                                                  00000800
000400 01  COLLECTOR-RANGE-TABLE  REDEFINES  PRCE1-UNIT-RECORD.         00000900
000600     02  COLLECTOR-RANGE-PORTION.                                 00001000
000600       05  COLLECTOR-RANGE-ENTRY          OCCURS 307 TIMES.       00001010
000800         10  COLL-RANGE-INDUSTRY     PIC S9(03) COMP-3.           00001100
000900         10  COLL-RANGE-TERRITORY    PIC X(01).                   00001200
001000         10  COLL-RANGE-COLL-UNIT    PIC S9(03) COMP-3.           00001300
001100         10  COLL-RANGE-BEG-CUST     PIC S9(07) COMP-3.           00001400
001200         10  COLL-RANGE-END-CUST     PIC S9(07) COMP-3.           00001500
001300       05  FILLER                      PIC X(09).                 00001600
000600     02  IND-PD-RANGE-PORTION.                                    00001610
001500       05  IND-PD-RANGE-ENTRY  OCCURS 54 TIMES.                   00001700
001600         10  IND-CODE                 PIC S9(03)    COMP-3.       00001800
001800         10  IND-MAX-AMOUNT-1         PIC S9(07)V99 COMP-3.       00001900
002000         10  IND-MAX-AMOUNT-2         PIC S9(07)V99 COMP-3.       00002000
002200         10  IND-MAX-AMOUNT-3         PIC S9(07)V99 COMP-3.       00002100
002400         10  IND-MAX-AMOUNT-4         PIC S9(07)V99 COMP-3.       00002200
002600         10  IND-MAX-AMOUNT-5         PIC S9(07)V99 COMP-3.       00002300
002800         10  IND-MAX-AMOUNT-6         PIC S9(07)V99 COMP-3.       00002400
003000         10  IND-MAX-AMOUNT-7         PIC S9(07)V99 COMP-3.       00002500
003100       05  FILLER                      PIC X(2).                  00002700
003200 01  CRUN-REC-LENGTH                 PIC S9(4) COMP               00002800
003300                                         VALUE +6000.             00002900
003400*******                                                           00110000
      ******************************************************************        
000100****                                                              00000100
000200**** LOOKUP GENERAL LADING ACCOUNT NUMBER FILE                    00000200
000300****                                                              00000300
000400                                                                  00000400
000500 01  GL-ACCT-REC.                                                 00000500
000600                                                                  00000600
000700     05  GL-ACCT-KEYWORD                    PIC  X(10).           00000700
000800     05      FILLER                         PIC  X(01).           00000800
000900     05  GL-ACCT-STORUNIT                   PIC  9(03).           00000900
001000     05      FILLER                         PIC  X(01).           00001000
001100     05  GL-ACCT-BANK                       PIC  9(04).           00001100
001200     05      FILLER                         PIC  X(01).           00001200
001300     05  GL-ACCT-CENTER                     PIC  9(07).           00001300
001400     05      FILLER                         PIC  X(01).           00001400
001500     05  GL-ACCT-ACCOUNT                    PIC  9(07).           00001500
001600     05      FILLER                         PIC  X(01).           00001600
001700     05  GL-ACCT-NAME                       PIC  X(39).           00001700
001800     05      FILLER                         PIC  X(01).           00001800
001900     05  GL-ACCT-TYPE                       PIC  X(04).           00001900
002000                                                                  00002000
002100 01  GL-ACCT-REC-LENGTH                     PIC S9(04)    COMP    00002100
002200                                            VALUE +80.            00002200
      ******************************************************************        
000100****                                                              00000100
000200**** SUPPLR WREHOUSE AGEING ON-LINE RECORD                        00000200
000300****                                                              00000300
000400****         THE BASE CLUSTER KEY IS:                             00000400
000500**** SUPPLR AND WREHOUSE NUMBER                                   00000500
000600****                                                              00000600
000700                                                                  00000700
000800 01  SUPPLR-CUST-REC.                                             00000800
000900                                                                  00000900
001000     05  SUPPLR-CUST-KEY.                                         00001000
001100         10  SUPPLR-CUST-SUPPLR-NO          PIC  X(03).           00001100
001200         10  SUPPLR-CUST-WREHOUSE-NO        PIC  9(07).           00001200
001300     05  SUPPLR-CUST-WREHOUSE-NAME          PIC  X(30).           00001300
001400     05  SUPPLR-CUST-AMT0-RATING            PIC  X(03).           00001400
001400     05  SUPPLR-CUST-ATB-RATING            PIC  X(03).            00001410
001500     05  SUPPLR-CUST-GROSS-REC              PIC S9(11)V99 COMP-3. 00001500
001600     05  SUPPLR-CUST-MATURED-GROSS          PIC S9(09)V99 COMP-3. 00001600
001700     05  SUPPLR-CUST-BILLING-11-30         PIC S9(09)V99 COMP-3.  00001700
001800     05  SUPPLR-CUST-BILLING-31-60         PIC S9(09)V99 COMP-3.  00001800
001900     05  SUPPLR-CUST-BILLING-61-90         PIC S9(09)V99 COMP-3.  00001900
002000     05  SUPPLR-CUST-BILLING-91-180        PIC S9(09)V99 COMP-3.  00002000
002100     05  SUPPLR-CUST-BILLING-181-UP        PIC S9(09)V99 COMP-3.  00002100
002200     05  SUPPLR-CUST-DISPUTE                PIC S9(09)V99 COMP-3. 00002200
002300     05  SUPPLR-CUST-DEDUCTIONS             PIC S9(09)V99 COMP-3. 00002300
002400     05  SUPPLR-CUST-LOC1                   PIC S9(09)V99 COMP-3. 00002400
002500     05  SUPPLR-CUST-COLR                   PIC S9(09)V99 COMP-3. 00002500
002600     05  SUPPLR-CUST-MAX-DAYS-PAST-DUE      PIC  9(03).           00002600
002700     05  SUPPLR-CUST-PRCE1-UNIT            PIC  9(03) COMP-3.     00002700
002800     05  SUPPLR-CUST-PRCE1-LINE            PIC  9(07) COMP-3.     00002800
002900     05  SUPPLR-CUST-PRCE1-LINE-CODE       PIC  9(01).            00002900
003000     05  SUPPLR-CUST-PRCE1-LINE-DATE       PIC  9(07) COMP-3.     00003000
003100     05  SUPPLR-CUST-PERMANENT-UNIT         PIC  9(03) COMP-3.    00003100
003200     05  SUPPLR-CUST-TERRITORY-CODE         PIC  9(02).           00003200
003300     05  SUPPLR-CUST-PRCE1-FILE-CODE       PIC  X(01).            00003300
003400     05  SUPPLR-CUST-PERM-COLL-UNIT         PIC  9(03) COMP-3.    00003400
003500     05  SUPPLR-CUST-INDUSTRY-CODE          PIC  9(03) COMP-3.    00003500
003600     05      FILLER                         PIC  X(04).           00003600
003700                                                                  00003700
003800 01  SUPPLR-CUST-REC-LENGTH                 PIC S9(04)    COMP    00003800
003900                                            VALUE +140.           00003900
      ******************************************************************        
000100*                                                                         
000200**** WREHOUSE MASTER FILE                                         00000600
000300****                                                              00000700
000400**** WREHOUSE HEADER RECORD                                       00000800
000500**** RECORD CODE = 1                                              00000900
000600**** SUPPLR NO = 000000                                           00001000
000800                                                                  00001100
000900 01  WREHOUSE-MASTER-REC.                                         00001200
001000                                                                  00001300
001100     05  WRHSE-WREHOUSE-NO                   PIC  9(07)    COMP-3.00001400
001200     05  WRHSE-RECORD-CODE                   PIC  9(01).          00001500
001300     05  WRHSE-SUPPLR-NO                     PIC  X(05).          00001600
001400     05  WRHSE-WREHOUSE-NAME                 PIC  X(30).          00001700
001400     05  WRHSE-WREHOUSE-NAME2                PIC  X(30).          00001800
001500     05  WRHSE-ADDRESS1                      PIC  X(30).          00001900
001600     05  WRHSE-ADDRESS2                      PIC  X(30).          00002000
001700     05  WRHSE-CITY                          PIC  X(20).          00002100
001800     05  WRHSE-STATE                         PIC  X(05).          00002200
001900     05  WRHSE-ZIP-CODE                      PIC  X(09).          00002300
           05      FILLER                         REDEFINES             00002400
               WRHSE-ZIP-CODE.                                          00002500
004400         10  WRHSE-ZIP                       PIC  9(05).          00002600
004400         10  WRHSE-EXPANDED-ZIP              PIC  X(04).          00002700
002000     05  WRHSE-STATE-CODE                    PIC  9(02).          00002800
           05  WRHSE-COUNTRY-CODE                  PIC  X(02).          00002900
002100     05  WRHSE-TERRITORY                     PIC  X(01).          00003000
002200     05  WRHSE-INDUSTRY                      PIC S9(03)    COMP-3.00003100
           05  WRHSE-SIC                           PIC  9(04)    COMP.  00003200
002300     05  WRHSE-CROSS-REF                     PIC S9(07)    COMP-3.00003300
002400     05  WRHSE-DATE-OPEN                     PIC S9(07)    COMP-3.00003400
           05  WRHSE-AV-LAST-VERIFY-DT             PIC  9(08)    COMP.  00003500
           05  WRHSE-AV-EXCLUDED-STATUS            PIC  X(01).          00003600
002500     05  WRHSE-AMT0-NO                       PIC  9(09)    COMP-3.00003700
002600     05  WRHSE-AMT0-RATING                   PIC  X(03).          00003800
002700     05  WRHSE-AMT0-DATE                     PIC  9(07)    COMP-3.00003900
           05  WRHSE-DO-NOT-POST                   PIC  X(01).          00004000
           05  WRHSE-FACTOR-CODE                   PIC  X(07).          00004100
002800     05  WRHSE-HIGH-VOL-PAST-DUE-CD          PIC  9(01).          00004200
002900     05  WRHSE-LOC1-CODE                     PIC  9(01).          00004300
003000     05  WRHSE-ACCT-TYPE-CODE                PIC  X(01).          00004400
003100     05  WRHSE-PRCE1-FILE-CODE              PIC  X(01).           00004500
003200     05  WRHSE-ATB-RATING                    PIC  X(03).          00004600
003300     05  WRHSE-ATB-DATE                      PIC  9(07)    COMP-3.00004700
003400     05  WRHSE-PREV-DB-RATING                PIC  X(03).          00004800
003500     05  WRHSE-PREV-ATB-RATING               PIC  X(03).          00004900
003600     05  WRHSE-YEAR-BUS-STARTED              PIC  9(05)    COMP-3.00005000
           05  WRHSE-CONTACT-PHONE-COUNTRY         PIC  X(03).          00005100
           05  WRHSE-CONTACT-PHONE-CITY            PIC  X(03).          00005200
003700     05  WRHSE-CONTACT-PHONE                 PIC  X(09).          00005300
           05  WRHSE-FAX-COUNTRY                   PIC  X(03).          00005400
           05  WRHSE-FAX-CITY                      PIC  X(03).          00005500
           05  WRHSE-FAX-NUMBER                    PIC  X(09).          00005600
003800     05  WRHSE-CONTACT-NAME                  PIC  X(20).          00005700
           05  WRHSE-TAX-ID                        PIC  9(10)    COMP.  00005800
003900     05  WRHSE-CATALOG001-WORK-CD            PIC  X(01).          00005900
004000     05  WRHSE-CUST-PRCE1-LIMIT             PIC S9(09)    COMP-3. 00006000
004100     05  WRHSE-OVRD-PRCE1-UNIT              PIC S9(03)    COMP-3. 00006100
004200     05  WRHSE-DATE-DB-LAST-ORDERED          PIC S9(07)    COMP-3.00006200
004300     05  WRHSE-COLLECTOR-CODE                PIC S9(03)    COMP-3.00006300
004500     05  WRHSE-DB-RATING-METHOD              PIC  9(01).          00006400
004600     05  WRHSE-ATB-RATING-METHOD             PIC  9(01).          00006500
004700     05      FILLER                         PIC  X(01).           00006600
004800     05  WRHSE-NET-WORTH-CODE                PIC  X(05).          00006700
004900     05  WRHSE-AQR-RATING                    PIC  9(02).          00006800
005000     05  FILLER                             PIC  X(02).           00006900
005000     05  WRHSE-AQR-DATE                      PIC S9(07)    COMP-3.00007000
005100     05      FILLER                         PIC  X(02).           00007100
005200     05  WRHSE-LAST-RDZ-DATE                 PIC S9(07)    COMP-3.00007200
005300     05      FILLER                         PIC  X(04).           00007300
005400     05  WRHSE-REVIEW-DATE                   PIC S9(07)    COMP-3.00007400
005500     05  WRHSE-MAX-DAYS-PAST-DUE             PIC S9(03)    COMP-3.00007500
005600     05  WRHSE-MAX-PCT-PAST-DUE              PIC S9(03)    COMP-3.00007600
           05  WRHSE-COLL-CONTACT-PHONE-CNTRY      PIC  X(03).          00007700
           05  WRHSE-COLL-CONTACT-PHONE-CITY       PIC  X(03).          00007800
005700     05  WRHSE-COLL-CONTACT-PHONE            PIC  X(09).          00007900
005700     05  WRHSE-PRIVATE-LABEL-CODE            PIC  9(03)    COMP-3.00008000
005700     05  WRHSE-PRCE1-CORPORATE-GROUP        PIC  9(05)    COMP-3. 00008100
           05  WRHSE-AV-EXCL-STAT-CHG-DT           PIC  X(8).           00008110
           05  WRHSE-AV-EXCL-STAT-CHG-BY           PIC  X(8).           00008120
005800     05      FILLER                         PIC  X(82).           00008200
005900                                                                  00008300
006000****                                                              00008400
006100**** SUPPLR CONTROL BLOCK RECORD                                  00008500
006200**** RECORD CODE = 2                                              00008600
006300****                                                              00008700
006400                                                                  00008800
006500 01  WREHOUSE-MASTER-REC2                   REDEFINES             00008900
006600     WREHOUSE-MASTER-REC.                                         00009000
006700                                                                  00009100
006800     05      FILLER                         PIC  X(10).           00009200
006900     05  WRHSE2-DATE-LAST-ACTIVE             PIC S9(07)    COMP-3.00009300
007000     05      FILLER                         PIC  X(05).           00009400
007100     05  WRHSE2-SUPPLR-TERRITORY             PIC  9(02).          00009500
007200     05      FILLER                         PIC  X(03).           00009600
           05  WRHSE2-CURRENCY-CODE                PIC  X(03).          00009700
           05  WRHSE2-DATE-RELATIONSHIP-OPE     PIC  9(08)    COMP.     00009800
           05  WRHSE2-PRCE1-LINE-EXP-DATE         PIC  9(08)    COMP.   00009900
007300     05  WRHSE2-PRCE1-LINE                  PIC S9(07)    COMP-3. 00010000
007400     05  WRHSE2-PRCE1-LINE-DATE             PIC S9(07)    COMP-3. 00010100
007500     05  WRHSE2-PRCE1-LINE-CODE             PIC  9(01).           00010200
007600     05  WRHSE2-MAX-ORDER-LIMIT              PIC S9(07)    COMP-3.00010300
007700     05      FILLER                         PIC  X(04).           00010400
007800     05  WRHSE2-MAX-TERMS-DAYS               PIC S9(03)    COMP-3.00010500
007900     05  WRHSE2-OUTSTAND-PRCHS-ORD           PIC S9(09)V99 COMP-3.00010600
008000     05  WRHSE2-EXECUTED-PRCHS-ORD           PIC S9(09)V99 COMP-3.00010700
008100     05  WRHSE2-UNSHIPPED-BALANCE           PIC S9(07)V99 COMP-3. 00010800
008200     05  WRHSE2-AUTO-APPROVED-AMT            PIC S9(09)V99 COMP-3.00010900
008300     05  WRHSE2-AUTO-APPROVED-COUNT          PIC S9(05)    COMP-3.00011000
008400     05  WRHSE2-MANUAL-APPROVED-AMT          PIC S9(09)V99 COMP-3.00011100
008500     05  WRHSE2-MANUAL-APPROVED-COUNT        PIC S9(05)    COMP-3.00011200
008600     05  WRHSE2-DECLINED-AMT                 PIC S9(07)V99 COMP-3.00011300
008700     05  WRHSE2-DECLINED-COUNT               PIC S9(05)    COMP-3.00011400
008800     05  WRHSE2-BILLING-15-TO-10            PIC S9(09)    COMP-3. 00011500
008900     05  WRHSE2-BILLING-11-TO-30            PIC S9(09)    COMP-3. 00011600
009000     05  WRHSE2-BILLING-31-TO-60            PIC S9(09)    COMP-3. 00011700
009100     05  WRHSE2-BILLING-61-TO-90            PIC S9(09)    COMP-3. 00011800
012900     05  WRHSE2-BILLING-91-TO-120           PIC S9(09)    COMP-3. 00011900
013000     05  WRHSE2-BILLING-121-TO-180          PIC S9(09)    COMP-3. 00012000
           05  WRHSE2-BILLING-151-TO-180          PIC S9(09)    COMP-3. 00012100
013100     05  WRHSE2-BILLING-181-UP              PIC S9(09)    COMP-3. 00012200
           05  WRHSE2-RETURNS-LAST-MONTH           PIC S9(09)V99 COMP-3.00012300
           05  WRHSE2-RETURNS-CURR-MONTH           PIC S9(09)V99 COMP-3.00012400
009200     05  WRHSE2-SALES-LAST-YEAR              PIC S9(09)    COMP-3.00012500
009300     05  WRHSE2-RETURNS-LAST-YEAR            PIC S9(07)    COMP-3.00012600
009400     05  WRHSE2-SALES-THIS-YEAR              PIC S9(09)    COMP-3.00012700
009500     05  WRHSE2-RETURNS-THIS-YEAR            PIC S9(07)    COMP-3.00012800
009600     05  WRHSE2-HIGH-BAL-LAST-QTR            PIC S9(09)    COMP-3.00012900
009700     05  WRHSE2-HIGH-BAL-THIS-QTR            PIC S9(09)    COMP-3.00013000
009800     05  WRHSE2-MERCHANDISE-DISPUTE          PIC S9(07)    COMP-3.00013100
009900     05  WRHSE2-OSD-CUST-DEDUCT              PIC S9(07)    COMP-3.00013200
010000     05  WRHSE2-SUPPLR-LOC1                  PIC S9(07)    COMP-3.00013300
010100     05  WRHSE2-COMMON-ACCT                  PIC S9(07)    COMP-3.00013400
           05  WRHSE2-SRCHGE-OVERRIDE-SW           PIC  X(01).          00013500
010200     05  WRHSE2-DISPUTE-COUNT                PIC S9(05)    COMP-3.00013600
                                                                        00013700
010300**** HIGH BALANCE PREVIOUS QUARTERS                               00013800
010400                                                                  00013900
010500     05  WRHSE2-HIGH-BAL-2ND-PREV            PIC S9(09)    COMP-3.00014000
010600     05  WRHSE2-HIGH-BAL-3RD-PREV            PIC S9(09)    COMP-3.00014100
010700     05  WRHSE2-HIGH-BAL-4TH-PREV            PIC S9(09)    COMP-3.00014200
010800     05  WRHSE2-HIGH-BAL-5TH-PREV            PIC S9(09)    COMP-3.00014300
010900                                                                  00014400
011000**** FUTURE PRCHS-ORD                                             00014500
011100                                                                  00014600
011200     05  WRHSE2-APPROVAL-0-TO-60             PIC S9(09)    COMP-3.00014700
011300     05  WRHSE2-APPROVAL-61-TO-120           PIC S9(09)    COMP-3.00014800
011400     05  WRHSE2-APPROVAL-121-TO-180          PIC S9(09)    COMP-3.00014900
011500     05  WRHSE2-APPROVAL-181-UP              PIC S9(09)    COMP-3.00015000
011600                                                                  00015100
011700**** FUTURE PRICE                                                 00015200
011800                                                                  00015300
011900     05  WRHSE2-EXPNSE-16-TO-60             PIC S9(09)    COMP-3. 00015400
012000     05  WRHSE2-EXPNSE-61-TO-120            PIC S9(09)    COMP-3. 00015500
012100     05  WRHSE2-EXPNSE-121-TO-180           PIC S9(09)    COMP-3. 00015600
012200     05  WRHSE2-EXPNSE-181-UP               PIC S9(09)    COMP-3. 00015700
012300     05  WRHSE2-LAST-APP-ACTION-TRAN         PIC S9(03)    COMP-3.00015800
012400     05  WRHSE2-LAST-APP-ACTION-DATE         PIC S9(07)    COMP-3.00015900
012500     05  WRHSE2-LAST-EXPNSE-DATE            PIC S9(07)    COMP-3. 00016000
012600     05  WRHSE2-MAX-DAYS-PAST-DUE            PIC S9(05)    COMP-3.00016100
012700     05      FILLER                         PIC  X(33).           00016200
 12800     05  WRHSE2-LAST-RDZ-DATE                PIC S9(07)    COMP-3.00016300
013200     05  WRHSE2-SUPPLIERLOC-BALANCE          PIC S9(11)V99 COMP-3.00016400
013210     05  WRHSE2-COLR-4XX                     PIC S9(09)V99 COMP-3.00016500
013220     05  WRHSE2-MATURED-GROSS                PIC S9(09)V99 COMP-3.00016600
013220     05  WRHSE2-SPEC-CUST-COMM-RATE          PIC X(01).           00016700
           05  WRHSE2-SPEC-COMM-RATE               PIC 9V9(02)  COMP-3. 00016800
           05  WRHSE2-SPEC-SRCHGE-RATE             PIC 9V9(06)  COMP-3. 00016900
           05  WRHSE2-ORDER-RATE                   PIC 9V9(06)  COMP-3. 00017000
           05  WRHSE2-TOTAL-IRSOUNT-OWED          PIC S9(07)V99 COMP-3. 00017100
           05  WRHSE2-TOTAL-INTEREST-OWED          PIC S9(07)V99 COMP-3.00017200
           05  WRHSE2-NUM-ORDERS-HELD-EOM          PIC S9(05)    COMP-3.00017300
           05  WRHSE2-AMT-ORDERS-HELD-EOM          PIC S9(09)V99 COMP-3.00017400
013300     05      FILLER                         PIC  X(92).           00017500
013400                                                                  00017600
013500****                                                              00017700
013600**** WREHOUSE CONTROL BLOCK RECORD                                00017800
013700**** RECORD CODE = 3                                              00017900
013800**** SUPPLR NO = 999999                                           00018000
013900****                                                              00018100
014000                                                                  00018200
014100 01  WREHOUSE-MASTER-REC3                   REDEFINES             00018300
014200     WREHOUSE-MASTER-REC.                                         00018400
014300                                                                  00018500
014400     05      FILLER                         PIC  X(10).           00018600
014500     05  WRHSE3-LAST-SALESPRC-DATE           PIC S9(07)    COMP-3.00018700
014600     05      FILLER                         PIC  X(05).           00018800
014700     05  WRHSE3-HIGH-BAL-LAST-QTR            PIC S9(09)    COMP-3.00018900
014800     05  WRHSE3-HIGH-BAL-THIS-QTR            PIC S9(09)    COMP-3.00019000
014900                                                                  00019100
015000**** CURRENT QUARTER                                              00019200
015100                                                                  00019300
015200     05      FILLER                         PIC S9(09)    COMP-3. 00019400
015300     05  WRHSE3-CURRENT-PROMPT-AMT           PIC S9(09)    COMP-3.00019500
015400     05  WRHSE3-CURRENT-LATE-AMT             PIC S9(09)    COMP-3.00019600
015500     05  WRHSE3-CURRENT-DOLLAR-DAYS          PIC S9(11)    COMP-3.00019700
015600     05  WRHSE3-CURRENT-PRICE-PAID        PIC S9(09)    COMP-3.   00019800
015700                                                                  00019900
015800**** FIRST PREVIOUS QUARTER                                       00020000
015900                                                                  00020100
016000     05      FILLER                         PIC S9(03)    COMP-3. 00020200
016100     05  WRHSE3-LAST-PROMPT-PCT              PIC S9(03)    COMP-3.00020300
016200     05  WRHSE3-LAST-LATE-PCT                PIC S9(03)    COMP-3.00020400
016300     05  WRHSE3-LAST-LATE-DAYS               PIC S9(03)    COMP-3.00020500
016400     05  WRHSE3-LAST-PRICE-PAID           PIC S9(09)    COMP-3.   00020600
016500                                                                  00020700
016600**** SECOND PREVIOUS QUARTER                                      00020800
016700                                                                  00020900
016800     05      FILLER                         PIC S9(03)    COMP-3. 00021000
016900     05  WRHSE3-PREV-PROMPT-PCT              PIC S9(03)    COMP-3.00021100
017000     05  WRHSE3-PREV-LATE-PCT                PIC S9(03)    COMP-3.00021200
017100     05  WRHSE3-PREV-LATE-DAYS               PIC S9(03)    COMP-3.00021300
017200     05  WRHSE3-PREV-PRICE-PAID           PIC S9(09)    COMP-3.   00021400
017300                                                                  00021500
017400**** FINANCIAL COST                                               00021600
017500                                                                  00021700
017600     05  WRHSE3-LAST-COLR-DATE               PIC S9(07)    COMP-3.00021800
017700     05  WRHSE3-LAST-COLR-AMT                PIC S9(07)V99 COMP-3.00021900
017800                                                                  00022000
017900**** THIS YEAR                                                    00022100
018000                                                                  00022200
018100     05  WRHSE3-INT-WAIVED-THIS-YR           PIC S9(05)    COMP-3.00022300
018200     05  WRHSE3-IRS-WAIVED-THIS-YR          PIC S9(05)    COMP-3. 00022400
018300     05  WRHSE3-DIFF-ABSORBED-THIS-YR        PIC S9(05)    COMP-3.00022500
018400     05  WRHSE3-RETURNED-CKS-THIS-YR         PIC S9(03)    COMP-3.00022600
018500                                                                  00022700
018600**** LAST YEAR                                                    00022800
018700                                                                  00022900
018800     05  WRHSE3-INT-WAIVED-LAST-YR           PIC S9(05)    COMP-3.00023000
018900     05  WRHSE3-IRS-WAIVED-LAST-YR          PIC S9(05)    COMP-3. 00023100
019000     05  WRHSE3-DIFF-ABSORBED-LAST-YR        PIC S9(05)    COMP-3.00023200
019100     05  WRHSE3-RETURNED-CKS-LAST-YR         PIC S9(03)    COMP-3.00023300
019200                                                                  00023400
019300**** AGING FIELDS                                                 00023500
019400                                                                  00023600
019500     05  WRHSE3-BILLING-15-TO-10            PIC S9(09)    COMP-3. 00023700
019600     05  WRHSE3-BILLING-11-TO-30            PIC S9(09)    COMP-3. 00023800
019700     05  WRHSE3-BILLING-31-TO-60            PIC S9(09)    COMP-3. 00023900
019800     05  WRHSE3-BILLING-61-TO-90            PIC S9(09)    COMP-3. 00024000
025100     05  WRHSE3-BILLING-91-TO-120           PIC S9(09)    COMP-3. 00024100
025200     05  WRHSE3-BILLING-121-TO-180          PIC S9(09)    COMP-3. 00024200
025200     05  WRHSE3-BILLING-151-TO-180          PIC S9(09)    COMP-3. 00024300
025300     05  WRHSE3-BILLING-181-UP              PIC S9(09)    COMP-3. 00024400
019900     05  WRHSE3-SALES-LAST-YEAR              PIC S9(09)    COMP-3.00024500
           05  WRHSE3-RETURNS-LAST-MONTH           PIC S9(09)V99 COMP-3.00024600
           05  WRHSE3-RETURNS-CURR-MONTH           PIC S9(09)V99 COMP-3.00024700
020000     05  WRHSE3-RETURNS-LAST-YEAR            PIC S9(07)    COMP-3.00024800
020100     05  WRHSE3-SALES-THIS-YEAR              PIC S9(09)    COMP-3.00024900
020200     05  WRHSE3-RETURNS-THIS-YEAR            PIC S9(07)    COMP-3.00025000
020300     05  WRHSE3-WEIGHTED-AVG-DAYS            PIC S9(03)    COMP-3.00025100
020400     05  WRHSE3-RETURNED-CHK-INDIC           PIC  9(01).          00025200
020500                                                                  00025300
020600**** HIGH BALANCE QUARTERS                                        00025400
020700                                                                  00025500
020800     05  WRHSE3-HIGH-BAL-2ND-PREV            PIC S9(09)    COMP-3.00025600
020900     05  WRHSE3-HIGH-BAL-3RD-PREV            PIC S9(09)    COMP-3.00025700
021000     05  WRHSE3-HIGH-BAL-4TH-PREV            PIC S9(09)    COMP-3.00025800
021100     05  WRHSE3-HIGH-BAL-5TH-PREV            PIC S9(09)    COMP-3.00025900
021200                                                                  00026000
021300**** FUTURE AGING PRCHS-ORD                                       00026100
021400                                                                  00026200
021500     05  WRHSE3-PRCHS-ORD-0-TO-60            PIC S9(09)    COMP-3.00026300
021600     05  WRHSE3-PRCHS-ORD-61-TO-120          PIC S9(09)    COMP-3.00026400
021700     05  WRHSE3-PRCHS-ORD-121-TO-180         PIC S9(09)    COMP-3.00026500
021800     05  WRHSE3-PRCHS-ORD-181-UP             PIC S9(09)    COMP-3.00026600
021900                                                                  00026700
022000**** FUTURE AGING PRICE                                           00026800
022100                                                                  00026900
022200     05  WRHSE3-PRICE-0-TO-60             PIC S9(09)    COMP-3.   00027000
022300     05  WRHSE3-PRICE-61-TO-120           PIC S9(09)    COMP-3.   00027100
022400     05  WRHSE3-PRICE-121-TO-180          PIC S9(09)    COMP-3.   00027200
022500     05  WRHSE3-PRICE-181-UP              PIC S9(09)    COMP-3.   00027300
022600                                                                  00027400
022700**** 3RD PREVIOUS QUARTER                                         00027500
022800                                                                  00027600
022900     05  WRHSE3-3RD-PROMPT-PCT               PIC S9(03)    COMP-3.00027700
023000     05  WRHSE3-3RD-LATE-PCT                 PIC S9(03)    COMP-3.00027800
023100     05  WRHSE3-3RD-LATE-DAYS                PIC S9(03)    COMP-3.00027900
023200     05  WRHSE3-3RD-PRICE-PAID            PIC S9(09)    COMP-3.   00028000
023300                                                                  00028100
023400**** 4TH PREVIOUS QUARTER                                         00028200
023500                                                                  00028300
023600     05  WRHSE3-4TH-PROMPT-PCT               PIC S9(03)    COMP-3.00028400
023700     05  WRHSE3-4TH-LATE-PCT                 PIC S9(03)    COMP-3.00028500
023800     05  WRHSE3-4TH-LATE-DAYS                PIC S9(03)    COMP-3.00028600
023900     05  WRHSE3-4TH-PRICE-PAID            PIC S9(09)    COMP-3.   00028700
024000                                                                  00028800
024100**** 5TH PREVIOUS QUARTER                                         00028900
024200                                                                  00029000
024300     05  WRHSE3-5TH-PROMPT-PCT               PIC S9(03)    COMP-3.00029100
024400     05  WRHSE3-5TH-LATE-PCT                 PIC S9(03)    COMP-3.00029200
024500     05  WRHSE3-5TH-LATE-DAYS                PIC S9(03)    COMP-3.00029300
024600     05  WRHSE3-5TH-PRICE-PAID            PIC S9(09)    COMP-3.   00029400
024700     05  WRHSE3-LAST-APP-ACTION-TRAN         PIC S9(03)    COMP-3.00029500
024800     05  WRHSE3-LAST-APP-ACTION-DATE         PIC S9(07)    COMP-3.00029600
024900     05      FILLER                         PIC  X(05).           00029700
025000     05  WRHSE3-WREHOUSE-MAX-PAST            PIC S9(05)    COMP-3.00029800
025400     05  WRHSE3-ACCOUNT-BALANCE              PIC S9(09)V99 COMP-3.00029900
025410     05  WRHSE3-COLR-4XX                     PIC S9(09)V99 COMP-3.00030000
025420     05  WRHSE3-MATURED-GROSS                PIC S9(09)V99 COMP-3.00030100
025500     05      FILLER                         PIC  X(28).           00030200
           05  WRHSE3-TOT-IRSOUNT-OWED            PIC S9(09)V99 COMP-3. 00030300
           05  WRHSE3-TOT-INTEREST-OWED            PIC S9(09)V99 COMP-3.00030400
           05  WRHSE3-AMT-ORDERS-HELD-EOM          PIC S9(09)V99 COMP-3.00030500
           05      FILLER                         PIC  X(94).           00030600
                                                                        00030700
       01  WRHSE-REC-LENGTH                        PIC S9(04)    COMP   00030800
                                                  VALUE +450.           00030900
      ******************************************************************        
000010*                                                                         
000020*  LADING COLR COPYBOOK                                                   
000030*                                                                         
000031 01  LADING-COLR-RECORD.                                                  
000050                                                                          
000051     05    LADING-SUPPLR                  PIC X(3).                       
000052     05    LADING-ACCT                    PIC 9(7) COMP-3.                
000080                                                                          
                                                                                
      ******************************************************************        
000100*----------------------------------------------------------------*      00
000200*              PRIVATE LABEL PAST DUE RANGE TABLE                *      00
000300*----------------------------------------------------------------*      00
000400 01  PL-PD-RANGE-TABLE.                                                 00
000500     05  PL-SUPPLR-CODE              PIC X(05).                         00
000600     05  FILLER                      PIC X(1).                          00
000700     05  PL-INDUSTRY-CODE            PIC S9(03).                        00
000800     05  FILLER                      PIC X(1).                          00
000900     05  PL-PAST-DUE-AMT-1-6         PIC S9(07)V99.                     00
001000     05  FILLER                      PIC X(1).                          00
001100     05  PL-PAST-DUE-AMT-7-14        PIC S9(07)V99.                     00
001200     05  FILLER                      PIC X(1).                          00
001300     05  PL-PAST-DUE-AMT-15-19       PIC S9(07)V99.                     00
001400     05  FILLER                      PIC X(1).                          00
001500     05  PL-PAST-DUE-AMT-20-24       PIC S9(07)V99.                     00
001600     05  FILLER                      PIC X(1).                          00
001700     05  PL-PAST-DUE-AMT-25-29       PIC S9(07)V99.                     00
001800     05  FILLER                      PIC X(1).                          00
001900     05  PL-PAST-DUE-AMT-30-36       PIC S9(07)V99.                     00
002000     05  FILLER                      PIC X(1).                          00
002100     05  PL-PAST-DUE-AMT-37-45       PIC S9(07)V99.                     00
002200     05  FILLER                      PIC X(1).                          00
002300 01  PL-PD-REC-LENGTH                PIC S9(4) COMP                     00
002400                                               VALUE +80.               00
                                                                                
      ******************************************************************        
000010****                                                              00000100
000020**** LOOKUP REPORT FILE                                           00000200
000030**** OUT OF RDZ22                                                 00000300
000040**** REPORT CODE 0T IS PASSED TO RDZ26 FOR COLR                   00000400
000050**** REPORT CODE 0X IS PASSED TO RDZ25 FOR SALES                  00000500
000060**** SORT DATES CCYYMMDD                                          00000600
000070****                                                              00000700
000080                                                                  00000800
000090 01  REPORT-DETAIL-DATE-RECORD.                                   00000900
000100                                                                  00001000
000110     05  REPORT-REC-CODE                    PIC  X(02).           00001100
000120     05      FILLER                         PIC  X(31).           00001200
000130     05  REPORT-DATE-REC                    PIC  9(07)    COMP-3. 00001300
000140     05      FILLER                         PIC  X(337).          00001400
000150                                                                  00001500
000160 01  REPORT-RECORD-01                       REDEFINES             00001600
000170     REPORT-DETAIL-DATE-RECORD.                                   00001700
000180                                                                  00001800
000190     05  PRT001-REPORT-CODE                 PIC  9(02).           00001900
000200     05  PRT001-SORT-CUST-NO                PIC  9(07).           00002000
000210     05  PRT001-SORT-SUPPLR                 PIC  X(03).           00002100
000220     05  PRT001-SORT-STORE                  PIC  9(05)    COMP-3. 00002200
000230     05      FILLER                         PIC  X(01).           00002300
000240     05  PRT001-SORT-EXPNSE                PIC  9(07).            00002400
000250     05  PRT001-SORT-TRAN-CD                PIC  9(03).           00002500
000260     05      FILLER                         PIC  X(03).           00002600
000270     05  PRT001-SORT-RDZ-DATE               PIC  9(07)    COMP-3. 00002700
000280     05  REPORT-DETAIL-INFO-01.                                   00002800
000290                                                                  00002900
000300         10  PRT001-CUST-NO                 PIC  9(07)    COMP-3. 00003000
000310         10  PRT001-SUPPLR-NO               PIC  X(05).           00003100
000320         10  PRT001-STORE-NO                PIC  9(05)    COMP-3. 00003200
000330         10  PRT001-EXPNSE-NO              PIC  9(09)    COMP-3.  00003300
000340         10  PRT001-ORIG-TRAN-CODE          PIC  9(03)    COMP-3. 00003400
000350         10  PRT001-TRAN-CODE               PIC  9(03)    COMP-3. 00003500
000360         10  PRT001-GROSS                   PIC S9(07)V99 COMP-3. 00003600
000370         10  PRT001-NET                     PIC S9(07)V99 COMP-3. 00003700
000380         10  PRT001-POSTED-NET              PIC S9(07)V99 COMP-3. 00003800
000390         10  PRT001-IRSOUNT-AMT            PIC S9(05)V99 COMP-3.  00003900
000400         10  PRT001-IRSOUNT-RATE           REDEFINES              00004000
000410             PRT001-IRSOUNT-AMT            PIC S9(4)V999 COMP-3.  00004100
000420         10  PRT001-PRICE-NO                PIC  9(03).           00004200
000430         10  PRT001-CHECK-NO                PIC  9(07).           00004300
000440         10  PRT001-DISPUTE-CODE            PIC  X.               00004400
000450         10  PRT001-RECOURSE-CODE           PIC  X.               00004401
000460         10      FILLER                     PIC  X(190).          00004500
000470                                                                  00004600
000480     05  REPORT-DETAIL-NAME-01              REDEFINES             00004700
000490         REPORT-DETAIL-INFO-01.                                   00004800
000500                                                                  00004900
000510         10      FILLER                     PIC  X(04).           00005000
000520         10  PRT001-BALANCE                 PIC S9(9)V99  COMP-3. 00005100
000530         10  PRT001-NAME                    PIC  X(30).           00005200
000540         10  PRT001-ADDRESS-1               PIC  X(30).           00005300
000550         10  PRT001-ADDRESS-2               PIC  X(30).           00005400
000560         10  PRT001-CITY                    PIC  X(20).           00005500
000570         10  PRT001-STATE                   PIC  X(05).           00005600
000580         10  PRT001-ZIP                     PIC  9(05).           00005700
000590         10      FILLER                     PIC  X(111).          00005800
000600                                                                  00005900
000610 01  REPORT-RECORD-04                       REDEFINES             00006000
000620     REPORT-DETAIL-DATE-RECORD.                                   00006100
000630                                                                  00006200
000640     05  RDZ004-REPORT-CODE                 PIC  9(02).           00006300
000650     05  RDZ004-SORTFLD                     PIC  X(31).           00006400
000660     05      FILLER                         REDEFINES             00006500
000670         RDZ004-SORTFLD.                                          00006600
000680                                                                  00006700
000690         10  RDZ004-SORT-FIELD              PIC  9(01).           00006800
000700         10      FILLER                     PIC  X(02).           00006900
000710         10  RDZ004-SORT-SUPPLR             PIC  X(03).           00007000
000720         10  RDZ004-SORT-PRICE              PIC  9(03).           00007100
000730         10  RDZ004-SORT-CUST-NO            PIC  9(07).           00007200
000740         10  RDZ004-SORT-STORE              PIC  9(05)    COMP-3. 00007300
000750         10      FILLER                     PIC  X(01).           00007400
000760         10  RDZ004-SORT-EXPNSE            PIC  9(07).            00007500
000770         10  RDZ004-DATE                    PIC  9(07)    COMP-3. 00007600
000780                                                                  00007700
000790     05  REPORT-DETAIL-INFO-04.                                   00007800
000800                                                                  00007900
000810         10  RDZ004-CUST-NO                 PIC  9(07)    COMP-3. 00008000
000820         10  RDZ004-SUPPLR-NO               PIC  X(05).           00008100
000830         10  RDZ004-STORE-NO                PIC  9(05)    COMP-3. 00008200
000840         10  RDZ004-EXPNSE-NO              PIC  9(09)    COMP-3.  00008300
000850         10  RDZ004-EXPNSE-DT              PIC  9(07)    COMP-3.  00008400
000860         10  RDZ004-TRAN-CD                 PIC  9(03)    COMP-3. 00008500
000870         10  RDZ004-EXPNSE-GR              PIC  9(07)V99 COMP-3.  00008600
000880         10  RDZ004-REF-NO                  PIC  9(05)    COMP-3. 00008700
000890         10  RDZ004-TERMS-CODE              PIC  9(03)    COMP-3. 00008800
000900         10  RDZ004-XDAYS                   PIC  9(03)    COMP-3. 00008900
000910         10  RDZ004-XDAYS-CODE              PIC  X(01).           00009000
000920         10  RDZ004-SHORT-TERM              PIC S9(03)    COMP-3. 00009100
000930         10  RDZ004-PRICE-NO                PIC  9(03)    COMP-3. 00009200
000940         10  RDZ004-PRICE-DT                PIC  9(07)    COMP-3. 00009300
000950         10  RDZ004-CHECK-NO                PIC  9(07)    COMP-3. 00009400
000960         10  RDZ004-CHECK-DT                PIC  9(07)    COMP-3. 00009500
000970         10  RDZ004-REFNO2                  PIC  9(07)    COMP-3. 00009600
000980         10  RDZ004-PRINT-CTL1              PIC  X(01).           00009700
000990         10  RDZ004-REASON-CODE             PIC  X(02).           00009800
001000         10  RDZ004-OLD-TCODE               PIC  9(03)    COMP-3. 00009900
001010         10  RDZ004-NEW-TCODE               PIC  9(03)    COMP-3. 00010000
001020         10  RDZ004-REJECT-CODE             PIC  9(01).           00010100
001030         10  RDZ004-IRSOUNT                PIC  9(03)V99 COMP-3.  00010200
001040         10  RDZ004-FREIGHT                 PIC S9(07)V99 COMP-3. 00010300
001050         10  RDZ004-DISPUTE-CODE            PIC X.                00010400
001060         10  RDZ004-RECOURSE-CODE           PIC X.                00010500
001070         10      FILLER                     PIC  X(167).          00010600
001080                                                                  00010700
001090 01  REPORT-RECORD-06-RDZ                   REDEFINES             00010800
001100     REPORT-DETAIL-DATE-RECORD.                                   00010900
001110                                                                  00011000
001120     05  RDZ006-CODE                        PIC  9(02).           00011100
001130     05  RDZ006-RDZ-CUST                    PIC  9(07).           00011200
001140     05  RDZ006-RDZ-SUPPLR                  PIC  X(05).           00011300
001150     05  RDZ006-RDZ-TRCODE                  PIC  9(03).           00011400
001160     05  RDZ006-RDZ-FIELD                   PIC  9(03).           00011500
001170     05      FILLER                         PIC  X(08).           00011600
001180     05  RDZ006-REJECT-CODE                 PIC  9(01).           00011700
001190     05  RDZ006-RDZ-DATE                    PIC  9(07)    COMP-3. 00011800
001200     05  REPORT-DETAIL-INFO-06-RDZ.                               00011900
001210                                                                  00012000
001220         10  RDZ006-RDZ-CARD1               PIC  X(80).           00012100
001230         10  RDZ006-RDZ-CARD2               PIC  X(80).           00012200
001240         10  RDZ006-RDZ-CARD3               PIC  X(80).           00012300
001250         10  FILLER                         PIC  X(01).           00012400
001260                                                                  00012500
001270 01  REPORT-RECORD-08                       REDEFINES             00012600
001280     REPORT-DETAIL-DATE-RECORD.                                   00012700
001290                                                                  00012800
001300     05  REPORT-08-REPORT-CODE              PIC  9(02).           00012900
001310     05  REPORT-08-SORTFLD                  PIC  X(31).           00013000
001320     05      FILLER                         REDEFINES             00013100
001330         REPORT-08-SORTFLD.                                       00013200
001340                                                                  00013300
001350         10  DOCUM1-SORT-DEP-DATE           PIC  9(07).           00013400
001360         10  DOCUM1-SORT-PRICE              PIC  9(03).           00013500
001370         10  DOCUM1-SORT-SEQUENCE           PIC  9(03).           00013600
001380         10  DOCUM1-SORT-CUST               PIC  9(07) COMP-3.    00013700
001390         10  DOCUM1-SORT-SUPPLR             PIC  X(03).           00013800
001400         10  DOCUM1-SORT-STORE              PIC  9(05) COMP-3.    00013900
001410         10  DOCUM1-SORT-EXPNSE            PIC  9(07) COMP-3.     00014000
001420         10  DOCUM1-SORT-TRAN-CD            PIC  9(03).           00014100
001430         10  FILLER                         PIC  X(01).           00014200
001440                                                                  00014300
001450     05  REPORT-DETAIL-INFO-08.                                   00014400
001460                                                                  00014500
001470         10  DOCUM1-PROCESS-CODE            PIC  X(01).           00014600
001480         10  DOCUM1-RECORD-CODE             PIC  X(01).           00014700
001490         10  DOCUM1-WREHOUSE-NO             PIC  9(07)    COMP-3. 00014800
001500         10  DOCUM1-SUPPLR-NO               PIC  X(05).           00014900
001510         10  DOCUM1-STORE-NO                PIC  9(05)    COMP-3. 00015000
001520         10  DOCUM1-EXPNSE-NO              PIC  9(09)    COMP-3.  00015100
001530         10  DOCUM1-TRAN-CD                 PIC  9(03)    COMP-3. 00015200
001540         10  DOCUM1-POST-DATE               PIC  9(07)    COMP-3. 00015300
001550         10  DOCUM1-EXPNSE-DT              PIC  9(07)    COMP-3.  00015400
001560         10  DOCUM1-EXPNSE-GR              PIC  9(07)V99 COMP-3.  00015500
001570         10  DOCUM1-EXPNSE-NET             PIC  9(07)V99 COMP-3.  00015600
001580         10  DOCUM1-FREIGHT-AMT             PIC  9(07)V99 COMP-3. 00015700
001590         10  DOCUM1-FIRST-IRS              PIC  9(03)V99 COMP-3.  00015800
001600         10  DOCUM1-FIRST-DAYS              PIC  9(03)    COMP-3. 00015900
001610         10  DOCUM1-SECOND-IRS             PIC  9(03)V99 COMP-3.  00016000
001620         10  DOCUM1-SECOND-DAYS             PIC  9(03)    COMP-3. 00016100
001630         10  DOCUM1-TERMS-TYPE              PIC  X(01).           00016200
001640         10  DOCUM1-NET-DAYS                PIC  9(03)    COMP-3. 00016300
001650         10  DOCUM1-XDAYS                   PIC  9(03)    COMP-3. 00016400
001660         10  DOCUM1-XDAYS-CODE              PIC  X(01).           00016500
001670         10  DOCUM1-FIRST-DUE-DT            PIC  9(07)    COMP-3. 00016600
001680         10  DOCUM1-FIRST-AMT               PIC  9(05)V99 COMP-3. 00016700
001690         10  DOCUM1-SECND-DUE-DT            PIC  9(07)    COMP-3. 00016800
001700         10  DOCUM1-SECND-AMT               PIC  9(05)V99 COMP-3. 00016900
001710         10  DOCUM1-FINAL-DUE-DT            PIC  9(07)    COMP-3. 00017000
001720         10  DOCUM1-SHORT-TERM              PIC S9(03)    COMP-3. 00017100
001730         10  DOCUM1-LONG-TERM               PIC  9(03)    COMP-3. 00017200
001740         10  DOCUM1-LST-TRAN-CD             PIC  9(03)    COMP-3. 00017300
001750         10  DOCUM1-LST-TRAN-DT             PIC  9(07)    COMP-3. 00017400
001760         10  DOCUM1-FE-STATUS               PIC  X(01).           00017500
001770         10  DOCUM1-REF-NO                  PIC  9(05)    COMP-3. 00017600
001780         10  DOCUM1-DELETE-TC               PIC  9(03)    COMP-3. 00017700
001790         10  DOCUM1-DELETE-DT               PIC  9(07)    COMP-3. 00017800
001800         10  DOCUM1-PRICE-NO                PIC  9(03)    COMP-3. 00017900
001810         10  DOCUM1-PRICE-DT                PIC  9(07)    COMP-3. 00018000
001820         10  DOCUM1-CHECK-NO                PIC  9(07)    COMP-3. 00018100
001830         10  DOCUM1-CHECK-DT                PIC  9(07)    COMP-3. 00018200
001840         10  DOCUM1-CHECK-AMT               PIC  9(07)V99 COMP-3. 00018300
001850         10  DOCUM1-SEQUENCE-NO             PIC  9(03)    COMP-3. 00018400
001860         10  DOCUM1-PROCESSOR               PIC  X(03).           00018500
001870         10  DOCUM1-FILM-NUMBER             PIC  9(11)    COMP-3. 00018600
001880         10  DOCUM1-COMMON-ACCT             PIC  9(07)    COMP-3. 00018700
001890         10  DOCUM1-THIRD-IRS              PIC  9(03)V99 COMP-3.  00018800
001900         10  DOCUM1-THIRD-DAYS              PIC  9(03)    COMP-3. 00018900
001910         10  DOCUM1-FOURTH-IRS             PIC  9(03)V99 COMP-3.  00019000
001920         10  DOCUM1-FOURTH-DAYS             PIC  9(03)    COMP-3. 00019100
001930         10  DOCUM1-TERMS-CODE              PIC  9(03)    COMP-3. 00019200
001940         10  DOCUM1-THIRD-DUE-DT            PIC  9(07)    COMP-3. 00019300
001950         10  DOCUM1-THIRD-AMT               PIC  9(05)V99 COMP-3. 00019400
001960         10  DOCUM1-FOURTH-DUE-DT           PIC  9(07)    COMP-3. 00019500
001970         10  DOCUM1-FOURTH-AMT              PIC  9(05)V99 COMP-3. 00019600
001980         10  DOCUM1-ORIG-PRICE-DT           PIC  9(07)    COMP-3. 00019700
001990         10  DOCUM1-ORIG-PRICE-NO           PIC  9(03)    COMP-3. 00019800
002000         10  DOCUM1-DAYS-PAST-DUE           PIC  9(03)    COMP-3. 00019900
002010         10  DOCUM1-ANTICIPATION            PIC  9(05)V99 COMP-3. 00020000
002020         10  DOCUM1-EARNED-CODE             PIC  9(01).           00020100
002030         10  DOCUM1-TAKEN-CODE              PIC  X(01).           00020200
002040         10  DOCUM1-EXEMPT-DAYS             PIC  9(03).           00020300
002050         10  DOCUM1-DISPUTE-CODE            PIC  X(01).           00020400
002060         10  DOCUM1-RECOURSE-CODE           PIC  X(01).           00020500
002070         10  DOCUM1-FE-XPENS-DAYS           PIC  9(03)   COMP-3.  00020600
002080         10      FILLER                     PIC  X(58).           00020700
002090                                                                  00020800
002100     05  REPORT-DETAIL-08-MISC              REDEFINES             00020900
002110         REPORT-DETAIL-INFO-08.                                   00021000
002120                                                                  00021100
002130         10      FILLER                     PIC  X(39).           00021200
002140         10  DOCUM1-IRSOUNT-AMT            PIC S9(05)V99 COMP-3.  00021300
002150         10  DOCUM1-IRSOUNT-RATE           PIC S9(2)V999 COMP-3.  00021400
002160         10  DOCUM1-PRINT-CTL               PIC  X(01).           00021500
002170         10  DOCUM1-REASON-CODE             PIC  X(02).           00021600
002180         10  DOCUM1-REF-NO2                 PIC S9(05)    COMP-3. 00021700
002190         10  DOCUM1-DEPOSIT-DATE            PIC  9(07)    COMP-3. 00021800
002200         10  DOCUM1-DBS-PRICE               PIC  9(03)    COMP-3. 00021900
002210         10  DOCUM1-DBS-SEQ                 PIC S9(03)    COMP-3. 00022000
002220         10  DOCUM1-DBS-PROC                PIC  X(03).           00022100
002230         10      FILLER                     PIC  X(178).          00022200
002240                                                                  00022300
002250 01  REPORT-RECORD-08-JES                   REDEFINES             00022400
002260     REPORT-DETAIL-DATE-RECORD.                                   00022500
002270                                                                  00022600
002280     05      FILLER                         PIC  X(02).           00022700
002290     05  DOCUM1-SORT-JES-CUST               PIC  9(07).           00022800
002300     05      FILLER                         PIC  X(10).           00022900
002310     05  DOCUM1-SORT-JES-SUPPLR             PIC  X(03).           00023000
002320     05      FILLER                         PIC  X(11).           00023100
002330     05  REPORT-DETAIL-INFO-08-JES.                               00023200
002340                                                                  00023300
002350         10      FILLER                     PIC  X(01).           00023400
002360         10  DOCUM1-JES-RECCODE             PIC  9(01).           00023500
002370         10  DOCUM1-JES-CUST-NO             PIC  9(07)    COMP-3. 00023600
002380         10  DOCUM1-JES-SUPPLR-NO           PIC  X(05).           00023700
002390         10  DOCUM1-JES-CODE                PIC  X(01).           00023800
002400         10  DOCUM1-JES-TERMS               PIC  9(03)    COMP-3. 00023900
002410         10  DOCUM1-JES-BAD-CO              PIC S9(07)    COMP-3. 00024000
002420         10  DOCUM1-JES-BANK-NO             PIC  9(03)    COMP-3. 00024100
002430         10  DOCUM1-JES-BANK-AMOUNT         PIC S9(09)    COMP-3. 00024200
002440         10      FILLER                     PIC  X(216).          00024300
002450                                                                  00024400
002460 01  REPORT-RECORD-08-RDZ                   REDEFINES             00024500
002470     REPORT-DETAIL-DATE-RECORD.                                   00024600
002480                                                                  00024700
002490     05      FILLER                         PIC  X(09).           00024800
002500     05  DOCUM1-RDZ-SUPPLR                  PIC  X(05).           00024900
002510     05  DOCUM1-RDZ-TRCODE                  PIC  9(03).           00025000
002520     05      FILLER                         PIC  X(16).           00025100
002530     05  REPORT-DETAIL-INFO-08-RDZ.                               00025200
002540                                                                  00025300
002550         10      FILLER                     PIC  X(02).           00025400
002560         10  DOCUM1-RDZ-CARD                PIC  X(80).           00025500
002570         10      FILLER                     PIC  X(159).          00025600
002580                                                                  00025700
002590**** NO WREHOUSE FOUND AND NAME/ADDRESS RECORD FOR RDZ26          00025800
002600                                                                  00025900
002610 01  REPORT-RECORD-08-NCF                   REDEFINES             00026000
002620     REPORT-DETAIL-DATE-RECORD.                                   00026100
002630                                                                  00026200
002640     05      FILLER                         PIC  X(02).           00026300
002650     05  DOCUM1-SORT-NCF-CUST               PIC  9(07).           00026400
002660     05      FILLER                         PIC  X(24).           00026500
002670     05  REPORT-DETAIL-INFO-08-NCF.                               00026600
002680                                                                  00026700
002690         10      FILLER                     PIC  X(01).           00026800
002700         10  DOCUM1-NCF-RECCODE             PIC  9(01).           00026900
002710         10  DOCUM1-NCF-CUST-NO             PIC  9(07)    COMP-3. 00027000
002720         10  DOCUM1-NCF-NAME1               PIC  X(30).           00027100
002730         10  DOCUM1-NCF-ADDR1               PIC  X(30).           00027200
002740         10  DOCUM1-NCF-ADDR2               PIC  X(30).           00027300
002750         10  DOCUM1-NCF-CITY                PIC  X(20).           00027400
002760         10  DOCUM1-NCF-STATE               PIC  X(05).           00027500
002770         10  DOCUM1-NCF-ZIP                 PIC  9(05).           00027600
002780         10  DOCUM1-NCF-AUDIT-CODE          PIC  X(01).           00027700
002790         10      FILLER                     PIC  X(114).          00027800
002800                                                                  00027900
002810 01  REPORT-FILE-RECORD                     REDEFINES             00028000
002820     REPORT-DETAIL-DATE-RECORD.                                   00028100
002830                                                                  00028200
002840     05      FILLER                         PIC  X(02).           00028300
002850     05  REPORT-SORTFLD                     PIC  X(31).           00028400
002860     05      FILLER                         REDEFINES             00028500
002870         REPORT-SORTFLD.                                          00028600
002880                                                                  00028700
002890         10  DOCUM2-SORT-BACK-DATE          PIC  9(06).           00028800
002900         10  DOCUM2-SORT-SUPPLR             PIC  X(03).           00028900
002910         10  DOCUM2-SORT-PRICE-DATE         PIC  9(06).           00029000
002920         10  DOCUM2-SORT-PRICE-NO           PIC  9(03).           00029100
002930         10  DOCUM2-SORT-TERMS              PIC  9(03).           00029200
002940         10  DOCUM2-SORT-EXPNSE            PIC  9(07).            00029300
002950         10  DOCUM2-SORT-RPT-CODE           PIC  9(02).           00029400
002960         10      FILLER                     PIC  X(01).           00029500
002970                                                                  00029600
002980     05  REPORT-DETAIL-INFO-09.                                   00029700
002990                                                                  00029800
003000         10  DOCUM2-PROCESS-CODE            PIC  X(01).           00029900
003010         10  DOCUM2-RECORD-CODE             PIC  X(01).           00030000
003020         10  DOCUM2-WREHOUSE-NO             PIC  9(07)    COMP-3. 00030100
003030         10  DOCUM2-SUPPLR-NO               PIC  X(05).           00030200
003040         10  DOCUM2-STORE-NO                PIC  9(05)    COMP-3. 00030300
003050         10  DOCUM2-EXPNSE-NO              PIC  9(09)    COMP-3.  00030400
003060         10  DOCUM2-TRAN-CD                 PIC  9(03)    COMP-3. 00030500
003070         10  DOCUM2-POST-DATE               PIC  9(07)    COMP-3. 00030600
003080         10  DOCUM2-EXPNSE-DT              PIC  9(07)    COMP-3.  00030700
003090         10  DOCUM2-EXPNSE-GR              PIC  9(07)V99 COMP-3.  00030800
003100         10  DOCUM2-EXPNSE-NET             PIC  9(07)V99 COMP-3.  00030900
003110         10  DOCUM2-FREIGHT-AMT             PIC  9(07)V99 COMP-3. 00031000
003120         10  DOCUM2-FIRST-IRS              PIC  9(03)V99 COMP-3.  00031100
003130         10  DOCUM2-FIRST-DAYS              PIC  9(03)    COMP-3. 00031200
003140         10  DOCUM2-SECOND-IRS             PIC  9(03)V99 COMP-3.  00031300
003150         10  DOCUM2-SECOND-DAYS             PIC  9(03)    COMP-3. 00031400
003160         10  DOCUM2-TERMS-TYPE              PIC  X(01).           00031500
003170         10  DOCUM2-NET-DAYS                PIC  9(03)    COMP-3. 00031600
003180         10  DOCUM2-XDAYS                   PIC  9(03)    COMP-3. 00031700
003190         10  DOCUM2-XDAYS-CODE              PIC  X(01).           00031800
003200         10  DOCUM2-FIRST-DUE-DT            PIC  9(07)    COMP-3. 00031900
003210         10  DOCUM2-FIRST-AMT               PIC  9(05)V99 COMP-3. 00032000
003220         10  DOCUM2-SECND-DUE-DT            PIC  9(07)    COMP-3. 00032100
003230         10  DOCUM2-SECND-AMT               PIC  9(05)V99 COMP-3. 00032200
003240         10  DOCUM2-FINAL-DUE-DT            PIC  9(07)    COMP-3. 00032300
003250         10  DOCUM2-SHORT-TERM              PIC S9(03)    COMP-3. 00032400
003260         10  DOCUM2-LONG-TERM               PIC  9(03)    COMP-3. 00032500
003270         10  DOCUM2-LST-TRAN-CD             PIC  9(03)    COMP-3. 00032600
003280         10  DOCUM2-LST-TRAN-DT             PIC  9(07)    COMP-3. 00032700
003290         10  DOCUM2-FE-STATUS               PIC  X(01).           00032800
003300         10  DOCUM2-REF-NO                  PIC  9(05)    COMP-3. 00032900
003310         10  DOCUM2-FILM-NUMBER             PIC  9(11)    COMP-3. 00033000
003320         10  DOCUM2-PROCESSOR-CODE          PIC  X(03).           00033100
003330         10  DOCUM2-THIRD-IRS              PIC  9(03)V99 COMP-3.  00033200
003340         10  DOCUM2-THIRD-DAYS              PIC  9(03)    COMP-3. 00033300
003350         10  DOCUM2-FOURTH-IRS             PIC  9(03)V99 COMP-3.  00033400
003360         10  DOCUM2-FOURTH-DAYS             PIC  9(03)    COMP-3. 00033500
003370         10  DOCUM2-TERMS-CODE              PIC  9(03)    COMP-3. 00033600
003380         10  DOCUM2-THIRD-DUE-DT            PIC  9(07)    COMP-3. 00033700
004110         10  DOCUM2-THIRD-AMT               PIC  9(05)V99 COMP-3. 00033800
003400         10  DOCUM2-FOURTH-DUE-DT           PIC  9(07)    COMP-3. 00041100
003410         10  DOCUM2-FOURTH-AMT              PIC  9(05)V99 COMP-3. 00034000
003420         10  DOCUM2-ORIG-PRICE-DT           PIC  9(07)    COMP-3. 00034100
003430         10  DOCUM2-ORIG-PRICE-NO           PIC  9(03)    COMP-3. 00034200
003440         10  DOCUM2-DEPOSIT-DATE            PIC  9(07)    COMP-3. 00034300
003450         10  DOCUM2-SEQUENCE-NUMBER         PIC  9(03)    COMP-3. 00034400
003460         10  DOCUM2-ORIG-TRAN-CD            PIC  9(03)    COMP-3. 00034500
003470         10  DOCUM2-SPEC-CUST-RATE          PIC  X(01).           00034600
003480         10  DOCUM2-RECOURSE-CODE           PIC  X(01).           00034700
003490         10  DOCUM2-DISPUTE-CODE            PIC  X(01).           00034800
003500         10      FILLER                     PIC  X(092).          00034900
003510                                                                  00035000
003520 01  REPORT-RECORD-09-10                    REDEFINES             00035100
003530     REPORT-DETAIL-DATE-RECORD.                                   00035200
003540                                                                  00035300
003550     05  DOCUM2-10-REPORT-CODE              PIC  9(02).           00035400
003560     05  DOCUM2-10-SORT-SUPPLR              PIC  X(03).           00035500
003570     05  DOCUM2-10-SORT-DATE                PIC  9(06).           00035600
003580     05  DOCUM2-10-SORT-CODE                PIC  9(02).           00035700
003590     05  DOCUM2-10-SORT-PRICE               PIC  9(03).           00035800
003600     05      FILLER                         PIC  X(14).           00035900
003610     05  DOCUM2-10-SORT-RPT-CODE            PIC  9(02).           00036000
003620     05      FILLER                         PIC  X(01).           00036100
003630     05  REPORT-DETAIL-INFO-09-10.                                00036200
003640                                                                  00036300
003650         10  DOCUM2-10-TRAN-CODE            PIC  9(03)    COMP-3. 00036400
003660         10  DOCUM2-10-PRICE-NO             PIC  9(03)    COMP-3. 00036500
003670         10  DOCUM2-10-SUPPLR-NO            PIC  X(05).           00036600
003680         10  DOCUM2-10-ENTRY-NO             PIC  9(03)    COMP-3. 00036700
003690         10  DOCUM2-10-ENTRY-DATE           PIC  9(07)    COMP-3. 00036800
003700         10  DOCUM2-10-AMOUNT               PIC S9(07)V99 COMP-3. 00036900
003710         10  DOCUM2-10-INTEREST             PIC  9(05)V99 COMP-3. 00037000
003720         10  DOCUM2-10-OVER-ADVANCE         PIC  9(07)V99 COMP-3. 00037100
003730         10  DOCUM2-10-DESCRIPTION          PIC  X(20).                   
003740         10      FILLER                     PIC  X(192).          00037200
                                                                                
      ******************************************************************        
000100*******               LOOKUP ONLINE TERMS CODE FILE               00000100
000200*******                                                           00000200
000300*******                                                           00000300
000400 01        SUPPLER-RECORD.                                        00000400
000500           05  SUPPLER-TERM-CODE       PIC  9(3).                 00000500
000600           05  SUPPLER-DESCRIPTION     PIC  X(28).                00000600
000700           05  SUPPLER1-KEY.                                      00000700
000800               10  SUPPLER-1ST-IRS        PIC  99V99.             00000800
000900               10  SUPPLER-1ST-IRS-DAYS   PIC  9(3).              00000900
001000               10  SUPPLER-2ND-IRS        PIC  99V99.             00001000
001100               10  SUPPLER-2ND-IRS-DAYS   PIC  9(3).              00001100
001200               10  SUPPLER-3RD-IRS        PIC  99V99.             00001200
001300               10  SUPPLER-3RD-IRS-DAYS   PIC  9(3).              00001300
001400               10  SUPPLER-4TH-IRS        PIC  99V99.             00001400
001500               10  SUPPLER-4TH-IRS-DAYS   PIC  9(3).              00001500
001600               10  SUPPLER-NET-DAYS        PIC  9(3).             00001600
001700               10  SUPPLER-TERM-TYPE       PIC  9.                00001700
001800               10  SUPPLER-EXTRA-DAYS      PIC  9(3).             00001800
001900               10  SUPPLER-EXTRA-DAYS-CODE PIC  9.                00001900
002000           05  SUPPLER-DATE-STAMP          PIC  S9(7) COMP-3.     00002000
002100 01        SUPPLER-LENGTH              PIC S9(4)      COMP        00002100
002200                                           VALUE +71.             00002200
002300*******                                                           00002300
                                                                                
      ******************************************************************        
000100*******          COPYBOOK - 'RCUSCOLC'                            00000100
000200*******          PAST DUE CATALOG001 RECORD                       00000200
000300*******                                                           00000300
000400*******    THE DATE IN THIS COPYBOOK IS IN YYMMDD FORMAT          00000400
000500******* *********************************************             00000500
000600 01  PAST-DUE-COLLECT-REC.                                        00000600
000700         05  PAST-DUE-CUST-NO                PIC S9(07) COMP-3.   00000700
000800         05  PAST-DUE-INDUSTRY-CODE          PIC S9(03) COMP-3.   00000800
000900         05  PAST-DUE-TERRITORY              PIC 9(01).           00000900
001000         05  PAST-DUE-DATE                   PIC S9(07) COMP-3.   00001000
001100         05  PAST-DUE-MAX-DAY                PIC S9(03) COMP-3.   00001100
001200         05  PAST-DUE-TOTAL-AMOUNT           PIC S9(7)V99 COMP-3. 00001200
001300         05  PAST-DUE-PERMANENT-COLL         PIC S9(03) COMP-3.   00001300
001400         05  PAST-DUE-REPORT-FLAG            PIC X.               00001400
001500         05  FILLER                          PIC X(04).           00001500
001600                                                                  00001600
001700 01  PAST-DUE-REC-LENGTH                        PIC S9(04) COMP   00001700
001800                                                 VALUE +25.       00001800
      ***                                                                       
000100****                                                              00000100
000200**** SUPPLR WREHOUSE AGEING ON-LINE RECORD                        00000200
000300****                                                              00000300
000400****         THE BASE CLUSTER KEY IS:                             00000400
000500**** SUPPLR AND WREHOUSE NUMBER                                   00000500
000600****                                                              00000600
000700                                                                  00000700
000200**** LOOKUP DETAIL FILE                                           00000200
000300****                                                              00000300
000500**** ONLINE EXPNSE RECORD                                         00000500
000600**** RECORD CODE = 1                                              00000600
000700                                                                  00000700
000800 01  WREHOUSE-DETAIL-REC1.                                        00000800
000900                                                                  00000900
001000     05  UNIT-CUST-NO                      PIC  9(07)    COMP-3.  00001000
001100     05  UNIT-SUPPLR-NO                    PIC  X(05).            00001100
001200     05  UNIT-STORE-NO                     PIC  9(05)    COMP-3.  00001200
001300     05  UNIT-EXPNSE-NO                   PIC  9(09)    COMP-3.   00001300
001400     05  UNIT-REC-CD                       PIC  X(01).            00001400
001500     05  UNIT-TRAN-CD                      PIC  9(03)    COMP-3.  00001500
001600     05  UNIT-DATE-POSTED                  PIC  9(07)    COMP-3.  00001600
001700     05  UNIT-EXPNSE-DATE                 PIC  9(07)    COMP-3.   00001700
001800     05  UNIT-GROSS-AMT                    PIC S9(07)V99 COMP-3.  00001800
001900     05  UNIT-NET-AMT                      PIC S9(07)V99 COMP-3.  00001900
002000     05  UNIT-FREIGHT-AMT                  PIC S9(07)V99 COMP-3.  00002000
002100     05  UNIT-FIRST-IRS                   PIC S9(03)V99 COMP-3.   00002100
002200     05  UNIT-FIRST-DAYS                   PIC S9(03)    COMP-3.  00002200
002300     05  UNIT-SECOND-IRS                  PIC S9(03)V99 COMP-3.   00002300
002400     05  UNIT-SECOND-DAYS                  PIC S9(03)    COMP-3.  00002400
002500     05  UNIT-TERMS-TYPE                   PIC  X(01).            00002500
002600     05  UNIT-NET-DAYS                     PIC S9(03)    COMP-3.  00002600
002700     05  UNIT-EXTRA-DAYS                   PIC S9(03)    COMP-3.  00002700
002800     05  UNIT-EXTRA-DAYS-CD                PIC  X(01).            00002800
002900     05  UNIT-1ST-IRS-DUE                 PIC S9(07)    COMP-3.   00002900
003000     05  UNIT-1ST-IRS-AMT                 PIC S9(05)V99 COMP-3.   00003000
003100     05  UNIT-2ND-IRS-DUE                 PIC S9(07)    COMP-3.   00003100
003200     05  UNIT-2ND-IRS-AMT                 PIC S9(05)V99 COMP-3.   00003200
003300     05  UNIT-FINAL-DUE-DT                 PIC S9(07)    COMP-3.  00003300
003400     05  UNIT-SLS-DAYS-SHORT               PIC S9(03)    COMP-3.  00003400
003500     05  UNIT-SLS-DAYS-LONG                PIC S9(03)    COMP-3.  00003500
003600     05  UNIT-LAST-TRAN-CD                 PIC S9(03)    COMP-3.  00003600
003700     05  UNIT-LAST-TRAN-DT                 PIC S9(07)    COMP-3.  00003700
003800     05  UNIT-STATUS-CD                    PIC  X(01).            00003800
003900     05  UNIT-REFERENCE-NO                 PIC S9(05)    COMP-3.  00003900
004000     05  UNIT-FILM-NUMBER                  PIC  9(11)    COMP-3.  00004000
004100     05  UNIT-EXPNSE-PROC                 PIC  X(03).             00004100
004200     05  UNIT-THIRD-IRS                   PIC S9(03)V99 COMP-3.   00004200
004300     05  UNIT-THIRD-DAYS                   PIC S9(03)    COMP-3.  00004300
004400     05  UNIT-FOURTH-IRS                  PIC S9(03)V99 COMP-3.   00004400
004500     05  UNIT-FOURTH-DAYS                  PIC S9(03)    COMP-3.  00004500
004600     05  UNIT-TERMS-CODE                   PIC  9(03)    COMP-3.  00004600
004700     05  UNIT-3RD-IRS-DUE                 PIC S9(07)    COMP-3.   00004700
004800     05  UNIT-3RD-IRS-AMT                 PIC S9(05)V99 COMP-3.   00004800
004900     05  UNIT-4TH-IRS-DUE                 PIC S9(07)    COMP-3.   00004900
005000     05  UNIT-4TH-IRS-AMT                 PIC S9(05)V99 COMP-3.   00005000
005100     05  UNIT-ORIGINAL-PRICE-DATE          PIC  9(07)    COMP-3.  00005100
005200     05  UNIT-ORIGINAL-PRICE-NO            PIC  9(03)    COMP-3.  00005200
005300     05  UNIT-DAYS-PAST-DUE                PIC S9(03)    COMP-3.  00005300
005300     05  UNIT-DISPUTE-CODE                 PIC  X.                00005310
005300     05  UNIT-RECOURSE-TO-OTHER            PIC  X.                00005320
005500                                                                  00005500
005600**** UNADJUSTED DEDUCTION & RECEIVED ON ACCT                      00005600
005700**** RECORD CODE = 2                                              00005700
005800                                                                  00005800
005900 01  WREHOUSE-DETAIL-REC2                   REDEFINES             00005900
006000     WREHOUSE-DETAIL-REC1.                                        00006000
006100                                                                  00006100
006200     05      FILLER                         PIC  X(38).           00006200
006300     05  UNIT-IRS-AMT                     PIC S9(05)V99 COMP-3.   00006300
006400     05  UNIT-IRS-RATE                    PIC S9(2)V999 COMP-3.   00006400
006500     05  UNIT-PRINT-CTL1                   PIC  X(01).            00006500
006600     05  UNIT-REASON-CODE                  PIC  X(02).            00006600
006700     05  UNIT-REFERENCE-NO2                PIC S9(05)    COMP-3.  00006700
006800     05  UNIT-DEPOSIT-DATE                 PIC  9(07)    COMP-3.  00006800
006900     05  UNIT-ARP-PRICE                    PIC  9(03)    COMP-3.  00006900
007000     05  UNIT-ARP-SEQUENCE                 PIC S9(03)    COMP-3.  00007000
007100     05  UNIT-COLR-PROCESSOR               PIC  X(03).            00007100
007200     05      FILLER                         PIC  X(78).           00007200
007300                                                                  00007300
007400**** MISCELLANEOUS RECORDS                                        00007400
007500**** RECORD CODE = 3                                              00007500
007600                                                                  00007600
007700 01  WREHOUSE-DETAIL-REC3                   REDEFINES             00007700
007800     WREHOUSE-DETAIL-REC1.                                        00007800
007900                                                                  00007900
008000     05      FILLER                         PIC  X(28).           00008000
008100     05  UNIT-MISC-AMT                     PIC S9(07)V99 COMP-3.  00008100
008200     05      FILLER                         PIC  X(107).          00008200
008300                                                                  00008300
008400 01  UNIT-REC-LENGTH                       PIC S9(04)    COMP     00008400
008500                                            VALUE +140.           00008500
004970 01  SUPPLR-CUS-TOT-REC.                                          00004970
004990     05  SUPPLR-CUS-TOT-KEY.                                      00004990
005000         10  SUPPLR-CUS-TOT-SUPPLR-NO       PIC  X(03).           00005000
005010         10  SUPPLR-CUS-TOT-WREHOUSE-NO     PIC  9(07).           00005010
005020     05  SUPPLR-CUS-TOT-WREHOUSE-NAME       PIC  X(30).           00005020
005030     05  SUPPLR-CUS-TOT-AMT0-RATING         PIC  X(03).           00005030
005040     05  SUPPLR-CUS-TOT-ATB-RATING          PIC  X(03).           00005040
005050     05  SUPPLR-CUS-TOT-GROSS-REC           PIC S9(11)V99 COMP-3. 00005050
005060     05  SUPPLR-CUS-TOT-MATURED-GROSS       PIC S9(09)V99 COMP-3. 00005060
005070     05  SUPPLR-CUS-TOT-BILLING-11-30      PIC S9(09)V99 COMP-3.  00005070
005080     05  SUPPLR-CUS-TOT-BILLING-31-60      PIC S9(09)V99 COMP-3.  00005080
005090     05  SUPPLR-CUS-TOT-BILLING-61-90      PIC S9(09)V99 COMP-3.  00005090
005100     05  SUPPLR-CUS-TOT-BILLING-91-180     PIC S9(09)V99 COMP-3.  00005100
005110     05  SUPPLR-CUS-TOT-BILLING-181-UP     PIC S9(09)V99 COMP-3.  00005110
005120     05  SUPPLR-CUS-TOT-DISPUTE             PIC S9(09)V99 COMP-3. 00005120
005130     05  SUPPLR-CUS-TOT-DEDUCTIONS          PIC S9(09)V99 COMP-3. 00005130
005140     05  SUPPLR-CUS-TOT-LOC1                PIC S9(09)V99 COMP-3. 00005140
005150     05  SUPPLR-CUS-TOT-COLR                PIC S9(09)V99 COMP-3. 00005150
005160     05  SUPPLR-CUS-TOT-MAX-DAYS            PIC  9(03).           00005160
005170     05  SUPPLR-CUS-TOT-PRCE1-UNIT         PIC  9(03) COMP-3.     00005170
005180     05  SUPPLR-CUS-TOT-PRCE1-LINE         PIC  9(07) COMP-3.     00005180
005190     05  SUPPLR-CUS-TOT-PRCE1-LINE-CD      PIC  9(01).            00005190
005200     05  SUPPLR-CUS-TOT-PRCE1-LINE-DT      PIC  9(07) COMP-3.     00005200
005210     05  SUPPLR-CUS-TOT-PERM-UNIT           PIC  9(03) COMP-3.    00005210
005220     05  SUPPLR-CUS-TOT-TERRITORY           PIC  9(02).           00005220
005230     05  SUPPLR-CUS-TOT-PRCE1-FILE-CD      PIC  X(01).            00005230
005240     05  SUPPLR-CUS-TOT-PERM-COLL           PIC  9(03) COMP-3.    00005240
005250     05  SUPPLR-CUS-TOT-INDUSTRY            PIC  9(03) COMP-3.    00005250
005260     05      FILLER                         PIC  X(08).           00005260
005270                                                                  00005270
005280 01  SUPPLR-CUS-999-REC-LENGTH              PIC S9(04)    COMP    00005280
005290                                            VALUE +140.           00005290
005300*                                                                 00005300
005310 01  FILLER                                  PIC X(20) VALUE      00005310
005320      '***** FC12DEL   ****'.                                     00005320
005330 01    DELCL-BILL-LADING-REC.                                     00005330
005340           05  DELCL-TRAN-CODE               PIC 9(3) VALUE 030.  00005340
005350           05  DELCL-TRANS-CUST-NO           PIC 9(7).            00005350
005360           05  DELCL-TRANS-SUPPLR            PIC X(5).            00005360
005370           05  FILLER                      PIC X(75) VALUE SPACE. 00005370
005380*                                                                 00005380
005390 01  FILLER                                  PIC X(20) VALUE      00005390
005400      '***** FC12LIST  ****'.                                     00005400
005410 01    LIST-BILL-LADING-REC.                                      00005410
005420           05  LIST-TRANS-CODE               PIC 9(3) VALUE 982.  00005420
005430           05  LIST-TRANS-PRICE-NO           PIC 9(3).            00005430
005440           05  LIST-TRANS-CUST-NO            PIC 9(7).            00005440
005450           05  LIST-TRANS-SUPPLR             PIC X(3).            00005450
005460           05  LIST-TRANS-TS                 PIC 9(2) VALUE 00.   00005460
005470           05  LIST-TRANS-INITIALS           PIC X(3) VALUE 'XFR'.00005470
005480           05  FILLER                        PIC X(8) VALUE SPACE.00005480
005490           05  LIST-TRANS-DATE               PIC 9(6).            00005490
005500           05  FILLER                        PIC X(6) VALUE SPACE.00005500
005510           05  LIST-TRANS-AMOUNT             PIC S9(7)V99 COMP-3. 00005510
005520           05  FILLER                        PIC X(3) VALUE SPACE.00005520
005530           05  LIST-TRANS-CHG-DATE           PIC 9(6).            00005530
005540           05  FILLER                        PIC X(8) VALUE SPACE.00005540
005550           05  LIST-TRANS-ORDER              PIC 9(7).            00005550
005560           05  LIST-TRANS-BRANCH             PIC 9(7).            00005560
005570           05  FILLER                       PIC X(13) VALUE SPACE.00005570
005580 01    TIME-OF-DAY PIC X(100).                                    00005580
005580 01    LIST-891-TRANS-REC.                                        00005580
005590     05  LIST-891-TRAN-CODE                PIC  9(03) VALUE 891.  00005590
005600     05  LIST-891-PRICE-NUMBER             PIC  9(03).            00005600
005610     05  LIST-891-WREHOUSE-NUMBER          PIC  9(07).            00005610
005620     05  LIST-891-SUPPLR-NUMBER            PIC  X(05).            00005620
005630     05  LIST-891-STORE-NUMBER             PIC  9(04).            00005630
005640     05      FILLER                         REDEFINES             00005640
005650         LIST-891-STORE-NUMBER.                                   00005650
005660                                                                  00005660
005670         10  RDZ456-STORE-PACKED           PIC  9(05)    COMP-3.  00005670
005680         10      FILLER                    PIC  X(01).            00005680
005690                                                                  00005690
005700     05  LIST-891-EXPNSE-NUMBER           PIC  9(07).             00005700
005710     05  LIST-891-OLD-TRAN-CODE            PIC  9(03).            00005710
005720     05  LIST-891-NEW-TRAN-CODE            PIC  9(03).            00005720
005730     05  LIST-891-COMMENT                  PIC  X(25).            00005730
005740     05  LIST-891-DISPUTE-CODE             PIC  X.                00005740
005750     05      FILLER                        PIC  X(16).            00005750
005760     05  LIST-891-PROCESSOR                PIC  X(03).            00005760
005770     05      FILLER                        PIC  X(10).            00005770
005780                                                                  00005780
      *COPY DETAIL.                                                             
                                                                                
      * Copybook Location:                                                      
      * C:\education_workspace\IDz Tech Portal\IDz Resources\Education T        
      * raining Resource\IDzClass\copy\DETAIL.cpy                               
                                                                                
005790 01  FILLER                                  PIC X(20) VALUE      00005790
005800      '***** NEW DETAIL****'.                                     00005800
005810*******                       L                                   00005810
005820*******           COPYBOOK - DETAIL                               00005820
005830*******           WAREHOUSE RECORD  -                             00005830
005840*******                                                           00005840
005850 01        NEW-WREHOUSE-DETAIL-REC1.                              00005850
005860           05  NEW-UNIT-CUST-NO       PIC  9(7)      COMP-3.      00005860
005870           05  NEW-UNIT-SUPPLR-NO     PIC  X(5).                  00005870
005880           05  NEW-UNIT-STORE-NO      PIC  9(5)      COMP-3.      00005880
005890           05  NEW-UNIT-EXPNSE-NO    PIC  9(9)      COMP-3.       00005890
005900           05  NEW-UNIT-REC-CD        PIC  X(1).                  00005900
005910           05  NEW-UNIT-TRAN-CD       PIC  9(3)      COMP-3.      00005910
005920           05  NEW-UNIT-DATE-POSTED   PIC  9(7)      COMP-3.      00005920
005930           05  NEW-UNIT-EXPNSE-DATE  PIC  9(7)      COMP-3.       00005930
005940           05  NEW-UNIT-GROSS-AMT     PIC S9(7)V99   COMP-3.      00005940
005950           05  NEW-UNIT-NET-AMT       PIC S9(7)V99   COMP-3.      00005950
005960           05  NEW-UNIT-FREIGHT-AMT   PIC S9(7)V99   COMP-3.      00005960
005970           05  NEW-UNIT-FIRST-IRS    PIC S9(3)V99   COMP-3.       00005970
005980           05  NEW-UNIT-FIRST-DAYS    PIC S9(3)      COMP-3.      00005980
005990           05  NEW-UNIT-SECOND-IRS   PIC S9(3)V99   COMP-3.       00005990
006000           05  NEW-UNIT-SECOND-DAYS   PIC S9(3)      COMP-3.      00006000
006010           05  NEW-UNIT-TERMS-TYPE    PIC X(1).                   00006010
006020           05  NEW-UNIT-NET-DAYS      PIC S9(3)      COMP-3.      00006020
006030           05  NEW-UNIT-EXTRA-DAYS    PIC S9(3)      COMP-3.      00006030
006040           05  NEW-UNIT-EXTRA-DAYS-CD PIC X(1).                   00006040
006050           05  NEW-UNIT-1ST-IRS-DUE  PIC S9(7)      COMP-3.       00006050
006060           05  NEW-UNIT-1ST-IRS-AMT  PIC S9(5)V99   COMP-3.       00006060
006070           05  NEW-UNIT-2ND-IRS-DUE  PIC S9(7)      COMP-3.       00006070
006080           05  NEW-UNIT-2ND-IRS-AMT  PIC S9(5)V99   COMP-3.       00006080
006090           05  NEW-UNIT-FINAL-DUE-DT  PIC S9(7)      COMP-3.      00006090
006100           05  NEW-UNIT-SLS-DAYS-SHORT PIC S9(3)     COMP-3.      00006100
006110           05  NEW-UNIT-SLS-DAYS-LONG PIC S9(3)      COMP-3.      00006110
006120           05  NEW-UNIT-LAST-TRAN-CD  PIC S9(3)      COMP-3.      00006120
006130           05  NEW-UNIT-LAST-TRAN-DT  PIC S9(7)      COMP-3.      00006130
006140           05  NEW-UNIT-STATUS-CD     PIC X(1).                   00006140
006150           05  NEW-UNIT-REFERENCE-NO  PIC S9(5)      COMP-3.      00006150
006160           05  NEW-UNIT-FILM-NUMBER   PIC 9(11)      COMP-3.      00006160
006170           05  NEW-UNIT-EXPNSE-PROC  PIC X(3).                    00006170
006180           05  NEW-UNIT-THIRD-IRS    PIC S9(3)V99   COMP-3.       00006180
006190           05  NEW-UNIT-THIRD-DAYS    PIC S9(3)      COMP-3.      00006190
006200           05  NEW-UNIT-FOURTH-IRS   PIC S9(3)V99   COMP-3.       00006200
006210           05  NEW-UNIT-FOURTH-DAYS   PIC S9(3)      COMP-3.      00006210
006220           05  NEW-UNIT-TERMS-CODE    PIC 9(3)       COMP-3.      00006220
006230           05  NEW-UNIT-3RD-IRS-DUE  PIC S9(7)      COMP-3.       00006230
006240           05  NEW-UNIT-3RD-IRS-AMT  PIC S9(5)V99   COMP-3.       00006240
006250           05  NEW-UNIT-4TH-IRS-DUE  PIC S9(7)      COMP-3.       00006250
006260           05  NEW-UNIT-4TH-IRS-AMT  PIC S9(5)V99   COMP-3.       00006260
006270           05  NEW-UNIT-ORIGINAL-PRICE-DATE  PIC 9(7)  COMP-3.    00006270
006280           05  NEW-UNIT-ORIGINAL-PRICE-NO    PIC 9(3)  COMP-3.    00006280
006290           05  NEW-UNIT-DAYS-PAST-DUE        PIC 9(3)  COMP-3.    00006290
006300           05  NEW-UNIT-DISPUTE-CODE         PIC X.               00006300
006310           05  NEW-UNIT-RECOURSE-CODE        PIC X.               00006310
006320           05  FILLER                  PIC X(2).                      0000
006330*******                                                           00006330
006340*******                   UNADJUSTED DEDUCTION & RECEIVED ON ACCT 00006340
006350*******                            RECORD CODE = 2                00006350
006360*******                                                           00006360
006370 01  NEW-WREHOUSE-DETAIL-REC2 REDEFINES                           00006370
006380                    NEW-WREHOUSE-DETAIL-REC1.                     00006380
006390           05  FILLER                  PIC X(38).                 00006390
006400           05  NEW-UNIT-IRS-AMT      PIC S9(5)V99   COMP-3.       00006400
006410           05  NEW-UNIT-IRS-RATE     PIC S9(2)V999  COMP-3.       00006410
006420           05  NEW-UNIT-PRINT-CTL1    PIC X(1).                   00006420
006430           05  NEW-UNIT-REASON-CODE   PIC X(2).                   00006430
006440           05  NEW-UNIT-REFERENCE-NO2 PIC S9(5)      COMP-3.      00006440
006450           05  NEW-UNIT-DEPOSIT-DATE  PIC 9(7)       COMP-3.      00006450
006460           05  NEW-UNIT-ARP-PRICE     PIC 9(3)     COMP-3.        00006460
006470           05  NEW-UNIT-ARP-SEQUENCE  PIC S9(3)     COMP-3.       00006470
006480           05  NEW-UNIT-COLR-PROCESSOR PIC X(3).                  00006480
006490           05  FILLER                  PIC X(78).                 00006490
006500*******                                                           00006500
006510*******                   MISCELLANEOUS RECORDS -  RECORD CODE = 300006510
006520*******                                                           00006520
006530 01  NEW-WREHOUSE-DETAIL-REC3 REDEFINES                           00006530
006540                    NEW-WREHOUSE-DETAIL-REC1.                     00006540
006550           05  FILLER                  PIC X(28).                 00006550
006560           05  NEW-UNIT-MISC-AMT      PIC S9(7)V99   COMP-3.      00006560
006570           05  FILLER                  PIC X(107).                00006570
006580*******                                                           00006580
006590*******                                                           00006590
006600 01        NEW-UNIT-REC-LENGTH        PIC S9(4)      COMP         00006600
006610                                           VALUE +140.            00006610
006620*******                                                           00006620
006630     EJECT                                                        00006630
006640 01  NEW-WREHOUSE-MASTER-REC.                                     00006640
006650*******                  LOOKUP WREHOUSE MASTER FILE              00006650
006660*******                                                           00006660
006670*******           WREHOUSE HEADER RECORD                          00006670
006680*******               RECORD CODE =1 ; SUPPLR NO. = 000000        00006680
006690*******      WORK AREA FOR OUTPUT RECORD AND SAVED HEADER         00006690
006700     05  NEW-WRHSE-WREHOUSE-NO        PIC 9(7)      COMP-3.       00006700
006710     05  NEW-WRHSE-RECORD-CODE        PIC 9(1).                   00006710
006720     05  NEW-WRHSE-SUPPLR-NO          PIC X(5).                   00006720
006730     05  NEW-WRHSE-WREHOUSE-NAME      PIC X(30).                  00006730
006731     05  NEW-WRHSE-WREHOUSE-NAME2     PIC X(30).                  00006731
006740     05  NEW-WRHSE-ADDRESS1           PIC X(30).                  00006740
006750     05  NEW-WRHSE-ADDRESS2           PIC X(30).                  00006750
006760     05  NEW-WRHSE-CITY               PIC X(20).                  00006760
006770     05  NEW-WRHSE-STATE              PIC X(5).                   00006770
      ***                                                                       
006771     05  CMST-RECORD-CODE           PIC X(09).                    00006771
006771     05  CMST3-ACCOUNT-BALANCE           PIC X(09).               00006771
006771     05  CMST2-COMMON-ACCT           PIC X(09).                   00006771
      ***                                                                       
006771     05  PRICE2-YEAR-BUS-STARTED           PIC X(09).             00006771
006771     05  CONTACT-PHONE-CITY           PIC X(09).                  00006771
006771     05  PRICE2-REC           PIC X(09).                          00006771
006771     05  PRICE2-RATING           PIC X(09).                       00006771
006771     05  PRICE2-PRIOR-RATING           PIC X(09).                 00006771
006771     05  PRICE2-PRIMARY-SIC           PIC X(09).                  00006771
006771     05  PRICE2-NET-WORTH           PIC 9(09).                    00006771
006771     05  PRICE2-AMT0-NO           PIC X(09).                      00006771
006771     05  PRICE2-DATE-OF-DATA           PIC X(08).                 00006771
006771     05  PRICE2-WREHOUSE-TRADE-STYLE           PIC X(09).         00006771
006771     05  PRICE2-WREHOUSE-NO           PIC X(09).                  00006771
006771     05  PRICE2-WREHOUSE-PHONE           PIC X(09).               00006771
006771     05  PRICE2-WREHOUSE-NAME           PIC X(09).                00006771
006771     05  PRICE2-WREHOUSE-CITY           PIC X(09).                00006771
006771     05  PRICE2-WREHOUSE-ADDRESS           PIC X(09).             00006771
006771     05  PRICE2-WREHOUSE-ST           PIC X(09).                  00006771
006771     05  PRICE2-ZIP-PRIMARY           PIC X(09).                  00006771
006771     05  PRICE2-ZIP-EXTENSION          PIC X(09).                 00006771
006771     05  NEW-WRHSE-ZIP-EXTENSION           PIC X(09).             00006771
006771     05  NEW-WRHSE-ZIP-CODE           PIC X(09).                  00006771
006772     05  FILLER  REDEFINES  NEW-WRHSE-ZIP-CODE.                   00006772
006780         10  NEW-WRHSE-ZIP            PIC 9(5).                   00006780
006781         10  NEW-WRHSE-EXPANDED-ZIP   PIC X(4).                   00006781
006790     05  NEW-WRHSE-STATE-CODE         PIC 9(2).                   00006790
006791     05  NEW-WRHSE-COUNTRY-CODE       PIC X(02).                  00006791
006800     05  NEW-WRHSE-TERRITORY          PIC 9(1).                   00006800
006810     05  NEW-WRHSE-INDUSTRY           PIC S9(3)     COMP-3.       00006810
006811     05  NEW-WRHSE-SIC                PIC 9(04)     COMP.         00006811
006820     05  NEW-WRHSE-CROSS-REF          PIC S9(7)     COMP-3.       00006820
006830     05  NEW-WRHSE-DATE-OPEN          PIC S9(7)     COMP-3.       00006830
006831     05  NEW-WRHSE-AV-LAST-VERIFY-DATE  PIC 9(08)   COMP.         00006831
006832     05  NEW-WRHSE-AV-EXCLUDED-STATUS  PIC X(01).                 00006832
006840     05  NEW-WRHSE-AMT0-NO            PIC 9(09)     COMP-3.       00006840
006850     05  NEW-WRHSE-AMT0-RATING        PIC X(3).                   00006850
006860     05  NEW-WRHSE-AMT0-DATE          PIC S9(7)     COMP-3.       00006860
006861     05  NEW-WRHSE-DO-NOT-POST        PIC X(01).                  00006861
006862     05  NEW-WRHSE-FACTOR-CODE        PIC X(07).                  00006862
006870     05  NEW-WRHSE-HIGH-VOL-PAST-DUE-CD  PIC 9(1).                00006870
006880     05  NEW-WRHSE-LOC1-CODE          PIC 9(1).                   00006880
006890     05  NEW-WRHSE-ACCT-TYPE-CODE     PIC X(1).                   00006890
006900     05  NEW-WRHSE-PRCE1-FILE-CODE   PIC X(1).                    00006900
006910     05  NEW-WRHSE-ATB-RATING         PIC X(3).                   00006910
006920     05  NEW-WRHSE-ATB-DATE           PIC S9(7)     COMP-3.       00006920
006930     05  NEW-WRHSE-PREV-DB-RATING     PIC X(3).                   00006930
006940     05  NEW-WRHSE-PREV-ATB-RATING    PIC X(3).                   00006940
006950     05  NEW-WRHSE-YEAR-BUS-STARTED   PIC S9(5)     COMP-3.       00006950
006951     05  NEW-WRHSE-CONTACT-PHONE-COUR PIC X(03).                  00006951
006952     05  NEW-WRHSE-CONTACT-PHONE-CITY    PIC X(03).               00006952
006960     05  NEW-WRHSE-CONTACT-PHONE      PIC X(09).                  00006960
006971     05  NEW-WRHSE-FAX-PHONE-COUNTRY  PIC X(03).                  00006971
006972     05  NEW-WRHSE-FAX-PHONE-CITY     PIC X(03).                  00006972
006973     05  NEW-WRHSE-FAX-PHONE          PIC X(09).                  00006973
006974     05  NEW-WRHSE-CONTACT-NAME       PIC X(20).                  00006974
006975     05  NEW-WRHSE-TAX-ID             PIC 9(10)     COMP.         00006975
006980     05  NEW-WRHSE-CATALOG001-WORK-CD PIC X(1).                   00006980
006990     05  NEW-WRHSE-CUST-PRCE1-LIMIT  PIC S9(9)     COMP-3.        00006990
007000     05  NEW-WRHSE-OVRD-PRCE1-UNIT   PIC S9(3)     COMP-3.        00007000
007010     05  NEW-WRHSE-DATE-DB-LAST-ORDERED PIC S9(7)   COMP-3.       00007010
007020     05  NEW-WRHSE-COLLECTOR-CODE     PIC S9(3)     COMP-3.       00007020
007040     05  NEW-WRHSE-DB-RATING-METHOD   PIC 9(1).                   00007040
007050     05  NEW-WRHSE-ATB-RATING-METHOD  PIC 9(1).                   00007050
007060     05  FILLER                      PIC X(1).                    00007060
007070     05  NEW-WRHSE-NET-WORTH-CODE     PIC X(5).                   00007070
007080     05  NEW-WRHSE-AQR-RATING         PIC 9(2).                   00007080
007090     05  FILLER                      PIC X(2).                    00007090
007100     05  NEW-WRHSE-AQR-DATE           PIC S9(7)     COMP-3.       00007100
007110     05  FILLER                      PIC X(2).                    00007110
007120     05  NEW-WRHSE-LAST-RDZ-DATE      PIC S9(7)     COMP-3.       00007120
007130     05  FILLER                      PIC X(4).                    00007130
007140     05  NEW-WRHSE-REVIEW-DATE        PIC S9(7)     COMP-3.       00007140
007150     05  NEW-WRHSE-MAX-DAYS-PAST-DUE  PIC S9(3)     COMP-3.       00007150
007160     05  NEW-WRHSE-MAX-PCT-PAST-DUE   PIC S9(3)     COMP-3.       00007160
007170     05  NEW-WRHSE-COLL-CNTCT-PHON-TRY PIC X(03).                 00007170
007171     05  NEW-WRHSE-COLL-CNTCT-PHON-CITY  PIC X(03).               00007171
007172     05  NEW-WRHSE-COLL-CONTACT-PHONE PIC X(09).                  00007172
007180     05  NEW-WRHSE-PRIVATE-LABEL-CODE PIC  9(03)    COMP-3.       00007180
007190     05  NEW-WRHSE-CRED-CORP-GROUP    PIC  9(05)    COMP-3.       00007190
007200     05  NEW-WRHSE-AV-EXCL-STAT-CHG-DT PIC X(08).                 00007200
007201     05  NEW-WRHSE-AV-EXCL-STAT-CHG-BY PIC X(08).                 00007201
007203     05  FILLER                      PIC  X(85).                  00007203
007210*******                                                           00007210
007220*******             SUPPLR CONTROL BLOCK RECORD                   00007220
007230*******                  RECORD CODE = 2                          00007230
007240*******      WORK AREA FOR OUTPUT RECORD                          00007240
007250 01  NEW-WREHOUSE-MASTER-REC2.                                    00007250
007260     05  NEW-WRHSE2-WREHOUSE-NO       PIC 9(7)   COMP-3.          00007260
007270     05  NEW-WRHSE2-RECORD-CODE       PIC X(1).                   00007270
007280     05  NEW-WRHSE2-SUPPLR-NO         PIC X(5).                   00007280
007290     05  NEW-WRHSE2-DATE-LAST-ACTIVE  PIC S9(7)     COMP-3.       00007290
007300     05  FILLER                      PIC X(5).                    00007300
007310     05  NEW-WRHSE2-SUPPLR-TERRITORY  PIC 9(2).                   00007310
007320     05  FILLER                      PIC X(3).                    00007320
007321     05  NEW-WRHSE2-CURRENCY-CODE     PIC X(03).                  00007321
007322     05  NEW-WRHSE2-DATE-RELAT-OPENED PIC 9(08)    COMP  VALUE 0. 00007322
007323     05  NEW-WRHSE2-PRCE1-LINE-EXP-D PIC 9(08)  COMP  VALUE 0.    00007323
007330     05  NEW-WRHSE2-PRCE1-LINE       PIC S9(7)     COMP-3.        00007330
007340     05  NEW-WRHSE2-PRCE1-LINE-DATE  PIC S9(7)     COMP-3.        00007340
007350     05  NEW-WRHSE2-PRCE1-LINE-CODE  PIC 9(1).                    00007350
007360     05  NEW-WRHSE2-MAX-ORDER-LIMIT   PIC S9(7)     COMP-3.       00007360
007370     05  FILLER                      PIC X(4).                    00007370
007380     05  NEW-WRHSE2-MAX-TERMS-DAYS    PIC S9(3)     COMP-3.       00007380
007390     05  NEW-WRHSE2-OUTSTAND-PRCHS-ORD PIC S9(9)V99 COMP-3.       00007390
007400     05  NEW-WRHSE2-EXECUTED-PRCHS-ORD PIC S9(9)V99 COMP-3.       00007400
007410     05  NEW-WRHSE2-UNSHIPPED-BALANCE PIC S9(7)V99 COMP-3.        00007410
007420     05  NEW-WRHSE2-AUTO-APPROVED-AMT PIC S9(9)V99  COMP-3.       00007420
007430     05  NEW-WRHSE2-AUTO-APPROVED-CT PIC S9(5)      COMP-3.       00007430
007440     05  NEW-WRHSE2-MANUAL-APPROVED-AMT PIC S9(9)V99 COMP-3.      00007440
007450     05  NEW-WRHSE2-MANUAL-APPROVED-CT PIC S9(5)    COMP-3.       00007450
007460     05  NEW-WRHSE2-DECLINED-AMT      PIC S9(7)V99  COMP-3.       00007460
007470     05  NEW-WRHSE2-DECLINED-COUNT    PIC S9(5)     COMP-3.       00007470
007480     05  NEW-WRHSE2-BILLING-15-TO-10 PIC S9(9)     COMP-3.        00007480
007490     05  NEW-WRHSE2-BILLING-11-TO-30 PIC S9(9)     COMP-3.        00007490
007500     05  NEW-WRHSE2-BILLING-31-TO-60 PIC S9(9)     COMP-3.        00007500
007510     05  NEW-WRHSE2-BILLING-61-TO-90 PIC S9(9)     COMP-3.        00007510
007511     05  NEW-WRHSE2-BILLING-91-TO-120 PIC S9(9)    COMP-3.        00007511
007512     05  NEW-WRHSE2-BILLING-121-TO-180 PIC S9(9)   COMP-3.        00007512
007513     05  NEW-WRHSE2-BILLING-151-TO-180 PIC S9(9)   COMP-3.        00007513
007514     05  NEW-WRHSE2-BILLING-181-UP   PIC S9(9)     COMP-3.        00007514
007515     05  NEW-WRHSE2-RETURNS-PREV-MONTH PIC S9(09)V99 COMP-3.      00007515
007516     05  NEW-WRHSE2-RETURNS-CURR-MONTH PIC S9(09)V99 COMP-3.      00007516
007520     05  NEW-WRHSE2-SALES-LAST-YEAR   PIC S9(9)     COMP-3.       00007520
007530     05  NEW-WRHSE2-RETURNS-LAST-YEAR PIC S9(7)     COMP-3.       00007530
007540     05  NEW-WRHSE2-SALES-THIS-YEAR   PIC S9(9)     COMP-3.       00007540
007550     05  NEW-WRHSE2-RETURNS-THIS-YEAR PIC S9(7)     COMP-3.       00007550
007560     05  NEW-WRHSE2-HIGH-BAL-LAST-QTR PIC S9(9)     COMP-3.       00007560
007570     05  NEW-WRHSE2-HIGH-BAL-THIS-QTR PIC S9(9)     COMP-3.       00007570
007580     05  NEW-WRHSE2-MERCHANDISE-DISPUTE PIC S9(7)   COMP-3.       00007580
007590     05  NEW-WRHSE2-OSD-CUST-DEDUCT   PIC S9(7)     COMP-3.       00007590
007600     05  NEW-WRHSE2-SUPPLR-LOC1       PIC S9(7)     COMP-3.       00007600
007610     05  NEW-WRHSE2-COMMON-ACCT       PIC S9(7)     COMP-3.       00007610
007611     05  NEW-WRHSE2-SRCHGE-OVERRIDE-SW PIC X(01).                 00007611
007612     05  NEW-WRHSE2-DISPUTE-COUNT     PIC S9(05)    COMP-3.       00007612
007620*        **HIGH BALANCE PREVIOUS QUARTERS**                       00007620
007630     05  NEW-WRHSE2-HIGH-BAL-2ND-PREV PIC S9(9)     COMP-3.       00007630
007640     05  NEW-WRHSE2-HIGH-BAL-3RD-PREV PIC S9(9)     COMP-3.       00007640
007650     05  NEW-WRHSE2-HIGH-BAL-4TH-PREV PIC S9(9)     COMP-3.       00007650
007660     05  NEW-WRHSE2-HIGH-BAL-5TH-PREV PIC S9(9)     COMP-3.       00007660
007670*        **FUTURE PRCHS-ORD**                                     00007670
007680     05  NEW-WRHSE2-APPROVAL-0-TO-60  PIC S9(9)     COMP-3.       00007680
007690     05  NEW-WRHSE2-APPROVAL-61-TO-120 PIC S9(9)    COMP-3.       00007690
007700     05  NEW-WRHSE2-APPROVAL-121-TO-180 PIC S9(9)   COMP-3.       00007700
007710     05  NEW-WRHSE2-APPROVAL-181-UP   PIC S9(9)     COMP-3.       00007710
007720*        **FUTURE EXPNSES**                                       00007720
007730     05  NEW-WRHSE2-EXPNSE-16-TO-60  PIC S9(9)     COMP-3.        00007730
007740     05  NEW-WRHSE2-EXPNSE-61-TO-120 PIC S9(9)     COMP-3.        00007740
007750     05  NEW-WRHSE2-EXPNSE-121-TO-180 PIC S9(9)    COMP-3.        00007750
007750     05  NEW-WRHSE2-EXPNSE-121-TO-18 PIC S9(9)    COMP-3.         00007750
007760     05  NEW-WRHSE2-EXPNSE-181-UP    PIC S9(9)     COMP-3.        00007760
007770     05  NEW-WRHSE2-LAST-APP-ACTION-TRN PIC S9(3)  COMP-3.        00007770
007770     05  NEW-WRHSE2-LAST-APP-ACTION-TRA PIC S9(3)  COMP-3.        00007770
007780     05  NEW-WRHSE2-LAST-APP-ACTION-DAT PIC S9(7)  COMP-3.        00007780
007780     05  NEW-WRHSE2-LAST-APP-ACTION-DTE PIC S9(7)  COMP-3.        00007780
007790     05  NEW-WRHSE2-LAST-EXPNSE-DATE PIC S9(7)     COMP-3.        00007790
007800     05  NEW-WRHSE2-MAX-DAYS-PAST-DUE PIC S9(5)     COMP-3.       00007800
007810     05  FILLER                      PIC X(33).                   00007810
007820     05  NEW-WRHSE2-LAST-RDZ-DATE     PIC S9(7)     COMP-3.       00007820
007860     05  NEW-WRHSE2-SUPPLIERLOC-BALANCE PIC S9(11)V99 COMP-3.     00007860
007870     05  NEW-WRHSE2-BAL-4XX             PIC S9(9)V99 COMP-3.      00007870
007880     05  NEW-WRHSE2-MATURED-GROSS       PIC S9(9)V99 COMP-3.      00007880
007890     05  NEW-WRHSE2-SPEC-CUST-COMM-RATE PIC X.                    00007890
007891     05  NEW-WRHSE2-SPEC-COMM-RATE      PIC S9V9(06) COMP-3.      00007891
007892     05  NEW-WRHSE2-SPEC-SRCHGE-RATE    PIC S9V9(06) COMP-3.      00007892
007893     05  NEW-WRHSE2-ORDER-RATE          PIC S9V9(06) COMP-3.      00007893
007900     05  NEW-WRHSE2-TOTAL-IRSOUNT-OWED PIC S9(07)V99 COMP-3.      00007900
007901     05  NEW-WRHSE2-TOTAL-INTEREST-OWED PIC S9(07)V99 COMP-3.     00007901
007902     05  NEW-WRHSE2-NUM-ORDERS-HELD-EOM PIC S9(05)   COMP-3.      00007902
007903     05  NEW-WRHSE2-AMT-ORDERS-HELD-EOM PIC S9(09)V99 COMP-3.     00007903
007905     05  FILLER                        PIC X(92).                 00007905
007910*******                                                           00007910
007920*******          WREHOUSE CONTROL BLOCK RECORD                    00007920
007930*******               RECORD CODE = 3; SUPPLR NO = 999999         00007930
007940*******     WORK AREA FOR OUTPUT RECORD                           00007940
007950 01  NEW-WREHOUSE-MASTER-REC3.                                    00007950
007960     05  NEW-WRHSE3-WREHOUSE-NO       PIC 9(7)   COMP-3.          00007960
007970     05  NEW-WRHSE3-RECORD-CODE       PIC X(1).                   00007970
007980     05  NEW-WRHSE3-SUPPLR-NO         PIC X(5).                   00007980
007990     05  NEW-WRHSE3-LAST-SALESPRC-DATE PIC S9(7)    COMP-3.       00007990
008000     05  FILLER                      PIC X(5).                    00008000
008010     05  NEW-WRHSE3-HIGH-BAL-LAST-QTR PIC S9(9)     COMP-3.       00008010
008020     05  NEW-WRHSE3-HIGH-BAL-THIS-QTR PIC S9(9)     COMP-3.       00008020
008040     05  FILLER                      PIC S9(9)     COMP-3.        00008040
008050     05  NEW-WRHSE3-CURRENT-PROMPT-AMT PIC S9(9)    COMP-3.       00008050
008060     05  NEW-WRHSE3-CURRENT-LATE-AMT  PIC S9(9)     COMP-3.       00008060
008070     05  NEW-WRHSE3-CURRENT-DOLLAR-DAYS PIC S9(11)  COMP-3.       00008070
008080     05  NEW-WRHSE3-CURRENT-PRICE-PD PIC S9(9)   COMP-3.          00008080
008100     05  FILLER                      PIC S9(3)     COMP-3.        00008100
008110     05  NEW-WRHSE3-LAST-PROMPT-PCT   PIC S9(3)     COMP-3.       00008110
008120     05  NEW-WRHSE3-LAST-LATE-PCT     PIC S9(3)     COMP-3.       00008120
008130     05  NEW-WRHSE3-LAST-LATE-DAYS    PIC S9(3)     COMP-3.       00008130
008140     05  NEW-WRHSE3-LAST-EXPNSES-PAID PIC S9(9)    COMP-3.        00008140
008160     05  FILLER                      PIC S9(3)     COMP-3.        00008160
008170     05  NEW-WRHSE3-PREV-PROMPT-PCT   PIC S9(3)     COMP-3.       00008170
008180     05  NEW-WRHSE3-PREV-LATE-PCT     PIC S9(3)     COMP-3.       00008180
008190     05  NEW-WRHSE3-PREV-LATE-DAYS    PIC S9(3)     COMP-3.       00008190
008200     05  NEW-WRHSE3-PREV-EXPNSES-PAID PIC S9(9)    COMP-3.        00008200
008220     05  NEW-WRHSE3-LAST-COLR-DATE    PIC S9(7)     COMP-3.       00008220
008230     05  NEW-WRHSE3-LAST-COLR-AMT     PIC S9(7)V99  COMP-3.       00008230
008250     05  NEW-WRHSE3-INT-WAIVED-THIS-YR PIC S9(5)    COMP-3.       00008250
008260     05  NEW-WRHSE3-IRS-WAIVED-THIS-YR PIC S9(5)   COMP-3.        00008260
008270     05  NEW-WRHSE3-DIFF-ABSORBED-YTD PIC S9(5)     COMP-3.       00008270
008280     05  NEW-WRHSE3-RETURNED-CKS-THS-YR PIC S9(3)  COMP-3.        00008280
008280     05  NEW-WRHSE3-RETURNED-CKS-THIS-Y PIC S9(3)  COMP-3.        00008280
008300     05  NEW-WRHSE3-INT-WAIVED-LAST-YR PIC S9(5)    COMP-3.       00008300
008310     05  NEW-WRHSE3-IRS-WAIVED-LAST-YR PIC S9(5)   COMP-3.        00008310
008320     05  NEW-WRHSE3-DIFF-ABSORBED-LYTD PIC S9(5)    COMP-3.       00008320
008330     05  NEW-WRHSE3-RETURNED-CKS-LST-YR PIC S9(3)  COMP-3.        00008330
008340*******         **AGING FIELDS**                                  00008340
008350     05  NEW-WRHSE3-BILLING-15-TO-10 PIC S9(9)     COMP-3.        00008350
008360     05  NEW-WRHSE3-BILLING-11-TO-30 PIC S9(9)     COMP-3.        00008360
008370     05  NEW-WRHSE3-BILLING-31-TO-60 PIC S9(9)     COMP-3.        00008370
008380     05  NEW-WRHSE3-BILLING-61-TO-90 PIC S9(9)     COMP-3.        00008380
008381     05  NEW-WRHSE3-BILLING-91-TO-120 PIC S9(9)     COMP-3.       00008381
008382     05  NEW-WRHSE3-BILLING-121-TO-180 PIC S9(9)     COMP-3.      00008382
008383     05  NEW-WRHSE3-BILLING-151-TO-180 PIC S9(9)     COMP-3.      00008383
008384     05  NEW-WRHSE3-BILLING-181-UP   PIC S9(9)     COMP-3.        00008384
008390     05  NEW-WRHSE3-SALES-LAST-YEAR   PIC S9(9)     COMP-3.       00008390
008391     05  NEW-WRHSE3-RETURNS-LAST-MONTH PIC S9(09)V99 COMP-3.              
008392     05  NEW-WRHSE3-RETURNS-CURR-MONTH PIC S9(09)V99 COMP-3.      00008392
008400     05  NEW-WRHSE3-RETURNS-LAST-YEAR PIC S9(7)     COMP-3.       00008400
008410     05  NEW-WRHSE3-SALES-THIS-YEAR   PIC S9(9)     COMP-3.       00008410
008420     05  NEW-WRHSE3-RETURNS-THIS-YEAR PIC S9(7)     COMP-3.       00008420
008430     05  NEW-WRHSE3-WEIGHTED-AVG-DAYS PIC S9(3)     COMP-3.       00008430
008440     05  NEW-WRHSE3-RETURNED-CHK-INDIC PIC 9(1).                  00008440
008460     05  NEW-WRHSE3-HIGH-BAL-2ND-PREV PIC S9(9)     COMP-3.       00008460
008470     05  NEW-WRHSE3-HIGH-BAL-3RD-PREV PIC S9(9)     COMP-3.       00008470
008480     05  NEW-WRHSE3-HIGH-BAL-4TH-PREV PIC S9(9)     COMP-3.       00008480
008490     05  NEW-WRHSE3-HIGH-BAL-5TH-PREV PIC S9(9)     COMP-3.       00008490
008510     05  NEW-WRHSE3-PRCHS-ORD-0-TO-60 PIC S9(9)     COMP-3.       00008510
008520     05  NEW-WRHSE3-PRCHS-ORD-61-TO-120 PIC S9(9)   COMP-3.       00008520
008530     05  NEW-WRHSE3-PRCHS-ORD-121-TO PIC S9(9)  COMP-3.           00008530
008540     05  NEW-WRHSE3-PRCHS-ORD-181-UP  PIC S9(9)     COMP-3.       00008540
008560     05  NEW-WRHSE3-PRICE-0-TO-60  PIC S9(9)     COMP-3.          00008560
008570     05  NEW-WRHSE3-PRICE-61-TO-120 PIC S9(9)    COMP-3.          00008570
008580     05  NEW-WRHSE3-PRICE-121-TO-180 PIC S9(9)   COMP-3.          00008580
008590     05  NEW-WRHSE3-PRICE-181-UP   PIC S9(9)     COMP-3.          00008590
008610     05  NEW-WRHSE3-3RD-PROMPT-PCT    PIC S9(3)     COMP-3.       00008610
008620     05  NEW-WRHSE3-3RD-LATE-PCT      PIC S9(3)     COMP-3.       00008620
008630     05  NEW-WRHSE3-3RD-LATE-DAYS     PIC S9(3)     COMP-3.       00008630
008640     05  NEW-WRHSE3-3RD-EXPNSES-PAID PIC S9(9)     COMP-3.        00008640
008660     05  NEW-WRHSE3-4TH-PROMPT-PCT    PIC S9(3)     COMP-3.       00008660
008670     05  NEW-WRHSE3-4TH-LATE-PCT      PIC S9(3)     COMP-3.       00008670
008680     05  NEW-WRHSE3-4TH-LATE-DAYS     PIC S9(3)     COMP-3.       00008680
008690     05  NEW-WRHSE3-4TH-EXPNSES-PAID PIC S9(9)     COMP-3.        00008690
008710     05  NEW-WRHSE3-5TH-PROMPT-PCT    PIC S9(3)     COMP-3.       00008710
008720     05  NEW-WRHSE3-5TH-LATE-PCT      PIC S9(3)     COMP-3.       00008720
008730     05  NEW-WRHSE3-5TH-LATE-DAYS     PIC S9(3)     COMP-3.       00008730
008740     05  NEW-WRHSE3-5TH-EXPNSES-PAID PIC S9(9)     COMP-3.        00008740
008750     05  NEW-WRHSE3-LAST-APP-ACTION-TRN PIC S9(3)  COMP-3.        00008750
008760     05  NEW-WRHSE3-LAST-APP-ACTION-DTE PIC S9(7)  COMP-3.        00008760
008770     05  FILLER                      PIC X(5).                    00008770
008780     05  NEW-WRHSE3-WREHOUSE-MAX-PAST PIC S9(5)     COMP-3.       00008780
008820     05  NEW-WRHSE3-ACCOUNT-BALANCE   PIC S9(9)V99  COMP-3.       00008820
008830     05  NEW-WRHSE3-BAL-4XX           PIC S9(9)V99  COMP-3.       00008830
008840     05  NEW-WRHSE3-MATURED-GROSS     PIC S9(9)V99  COMP-3.       00008840
008841     05  FILLER                      PIC X(28).                   00008841
008843     05  NEW-WRHSE3-TOT-IRSOUNT-OWED PIC S9(9)V99 COMP-3.         00008843
008844     05  NEW-WRHSE3-TOT-INTEREST-OWED PIC S9(9)V99 COMP-3.        00008844
008845     05  NEW-WRHSE3-AMT-ORDERS-HELD-EOM PIC S9(9)V99 COMP-3.      00008845
008850     05  FILLER                      PIC X(94).                   00008850
008860*******                                                           00008860
008870     EJECT                                                        00008870
008881*********                                                         00008881
008881 01  ZERO-HEADER.                                                 00008881
008890*******           WREHOUSE HEADER RECORD                          00008890
008900*******               RECORD CODE =1 ; SUPPLR NO. = 000000        00008900
008902     05  FILLER                  PIC 9(7)   COMP-3 VALUE 0.       00008902
008903     05  FILLER                  PIC 9(1)          VALUE 1.       00008903
008904     05  FILLER                  PIC X(5)          VALUE SPACES.  00008904
008905     05  FILLER                  PIC X(30)         VALUE SPACES.  00008905
008906     05  FILLER                  PIC X(30)         VALUE SPACES.  00008906
008907     05  FILLER                  PIC X(30)         VALUE SPACES.  00008907
008908     05  FILLER                  PIC X(30)         VALUE SPACES.  00008908
008909     05  FILLER                  PIC X(20)         VALUE SPACES.  00008909
008910     05  FILLER                  PIC X(5)          VALUE SPACES.  00008910
008911     05  FILLER                  PIC X(09)         VALUE SPACES.  00008911
008915     05  FILLER                  PIC 9(2)          VALUE 0.       00008915
008916     05  FILLER                  PIC X(02)         VALUE SPACES.  00008916
008917     05  FILLER                  PIC 9(1)          VALUE 0.       00008917
008918     05  FILLER                  PIC S9(3)  COMP-3 VALUE +0.      00008918
008919     05  FILLER                  PIC 9(04)  COMP   VALUE 0.       00008919
008920     05  FILLER                  PIC S9(7)  COMP-3 VALUE +0.      00008920
008921     05  FILLER                  PIC S9(7)  COMP-3 VALUE +0.      00008921
008922     05  FILLER                  PIC 9(08)  COMP   VALUE 0.       00008922
008923     05  FILLER                  PIC X(01)         VALUE SPACES.  00008923
008924     05  FILLER                  PIC 9(09)  COMP-3 VALUE 0.       00008924
008925     05  FILLER                  PIC X(3)          VALUE SPACES.  00008925
008926     05  FILLER                  PIC S9(7)  COMP-3 VALUE +0.      00008926
008927     05  FILLER                  PIC X(01)         VALUE SPACES.  00008927
008928     05  FILLER                  PIC X(07)         VALUE SPACES.  00008928
008929     05  FILLER                  PIC 9(1)          VALUE 0.       00008929
008930     05  FILLER                  PIC 9(1)          VALUE 0.       00008930
008931     05  FILLER                  PIC X(1)          VALUE SPACES.  00008931
008932     05  FILLER                  PIC X(1)          VALUE SPACES.  00008932
008933     05  FILLER                  PIC X(3)          VALUE SPACES.  00008933
008934     05  FILLER                  PIC S9(7)  COMP-3 VALUE +0.      00008934
008935     05  FILLER                  PIC X(3)          VALUE SPACES.  00008935
008936     05  FILLER                  PIC X(3)          VALUE SPACES.  00008936
008937     05  FILLER                  PIC S9(5)  COMP-3 VALUE +0.      00008937
008938     05  FILLER                  PIC X(03)         VALUE '000'.   00008938
008939     05  FILLER                  PIC X(03)         VALUE '000'.   00008939
008940     05  FILLER                  PIC X(09)  VALUE '000000000'.    00008940
008942     05  FILLER                  PIC X(03)         VALUE '000'.   00008942
008943     05  FILLER                  PIC X(03)         VALUE '000'.   00008943
008944     05  FILLER                  PIC X(09)  VALUE '000000000'.    00008944
008945     05  FILLER                  PIC X(20)         VALUE SPACES.  00008945
008946     05  FILLER                  PIC 9(10)  COMP   VALUE 0.       00008946
008947     05  FILLER                  PIC X(1)          VALUE SPACES.  00008947
008948     05  FILLER                  PIC S9(9)  COMP-3 VALUE +0.      00008948
008949     05  FILLER                  PIC S9(3)  COMP-3 VALUE +0.      00008949
008950     05  FILLER                  PIC S9(7)  COMP-3 VALUE +0.      00008950
008951     05  FILLER                  PIC S9(3)  COMP-3 VALUE +0.      00008951
008952     05  FILLER                  PIC 9(1)          VALUE 0.       00008952
008953     05  FILLER                  PIC 9(1)          VALUE 0.       00008953
008954     05  FILLER                  PIC X(1)          VALUE SPACES.  00008954
008955     05  FILLER                  PIC X(5)          VALUE ZEROS.   00008955
008956     05  FILLER                  PIC 9(2)          VALUE 0.       00008956
008957     05  FILLER                  PIC X(2)          VALUE SPACES.  00008957
008958     05  FILLER                  PIC S9(7)  COMP-3 VALUE +0.      00008958
008959     05  FILLER                  PIC X(2)          VALUE SPACES.  00008959
008960     05  FILLER                  PIC S9(7)  COMP-3 VALUE +0.      00008960
008961     05  FILLER                  PIC X(4)          VALUE SPACES.  00008961
008962     05  FILLER                  PIC S9(7)  COMP-3 VALUE +0.      00008962
008963     05  FILLER                  PIC S9(3)  COMP-3 VALUE +30.     00008963
008964     05  FILLER                  PIC S9(3)  COMP-3 VALUE +50.     00008964
008965     05  FILLER                  PIC X(03)  VALUE '000'.          00008965
008966     05  FILLER                  PIC X(03)  VALUE '000'.          00008966
008967     05  FILLER                  PIC X(09)  VALUE '000000000'.    00008967
008968     05  FILLER                  PIC 9(03)  COMP-3 VALUE 0.       00008968
008969     05  FILLER                  PIC 9(05)  COMP-3 VALUE 0.       00008969
008970     05  FILLER                  PIC X(101)        VALUE SPACES.  00008970
009390*******                                                           00009390
009400*******             SUPPLR CONTROL BLOCK RECORD                   00009400
009410*******                  RECORD CODE = 2                          00009410
009420*******                                                           00009420
009430 01  OEM-STNDARD-PRICE.                                           00009430
009440     05  FILLER                    PIC 9(7) COMP-3   VALUE 0.     00009440
009450     05  FILLER                    PIC 9(1)           VALUE 2.    00009450
009460     05  FILLER                    PIC X(5)           VALUE SPACE.00009460
009470     05  FILLER                    PIC S9(7)     COMP-3  VALUE +0.00009470
009480     05  FILLER                    PIC X(5)           VALUE SPACE.00009480
009490     05  FILLER                    PIC 9(2)             VALUE 0.  00009490
009500     05  FILLER                    PIC X(3)           VALUE SPACE.00009500
009501     05  FILLER                    PIC X(3)           VALUE SPACE.00009501
009502     05  FILLER                    PIC 9(08)     COMP   VALUE 0.  00009502
009503     05  FILLER                    PIC 9(08)     COMP   VALUE 0.  00009503
009510     05  FILLER                    PIC S9(7)     COMP-3 VALUE +0. 00009510
009520     05  FILLER                    PIC S9(7)     COMP-3 VALUE +0. 00009520
009530     05  FILLER                    PIC 9(1)      VALUE ZERO.      00009530
009540     05  FILLER                    PIC S9(7)     COMP-3 VALUE +0. 00009540
009550     05  FILLER                    PIC X(4)      VALUE SPACES.    00009550
009560     05  FILLER                    PIC S9(3)     COMP-3 VALUE +0. 00009560
009570     05  FILLER                    PIC S9(9)V99  COMP-3 VALUE +0. 00009570
009580     05  FILLER                    PIC S9(9)V99  COMP-3 VALUE +0. 00009580
009590     05  FILLER                    PIC S9(7)V99  COMP-3 VALUE +0. 00009590
009600     05  FILLER                    PIC S9(9)V99  COMP-3 VALUE +0. 00009600
009610     05  FILLER                    PIC S9(5)     COMP-3 VALUE +0. 00009610
009620     05  FILLER                    PIC S9(9)V99  COMP-3 VALUE +0. 00009620
009630     05  FILLER                    PIC S9(5)     COMP-3 VALUE +0. 00009630
009640     05  FILLER                    PIC S9(7)V99  COMP-3 VALUE +0. 00009640
009650     05  FILLER                    PIC S9(5)     COMP-3 VALUE +0. 00009650
009660     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009660
009670     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009670
009680     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009680
009690     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009690
009691     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009691
009692     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009692
009693     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009693
009694     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009694
009700     05  FILLER                    PIC S9(9)V99  COMP-3 VALUE +0. 00009700
009701     05  FILLER                    PIC S9(9)V99  COMP-3 VALUE +0. 00009701
009702     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009702
009710     05  FILLER                    PIC S9(7)     COMP-3 VALUE +0. 00009710
009720     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009720
009730     05  FILLER                    PIC S9(7)     COMP-3 VALUE +0. 00009730
009740     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009740
009750     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009750
009760     05  FILLER                    PIC S9(7)     COMP-3 VALUE +0. 00009760
009770     05  FILLER                    PIC S9(7)     COMP-3 VALUE +0. 00009770
009780     05  FILLER                    PIC S9(7)     COMP-3 VALUE +0. 00009780
009790     05  FILLER                    PIC S9(7)     COMP-3 VALUE +0. 00009790
009791     05  FILLER                    PIC X(01)     VALUE SPACES.    00009791
009792     05  FILLER                    PIC S9(05)    COMP-3 VALUE +0. 00009792
009800     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009800
009810     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009810
009820     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009820
009830     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009830
009840     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009840
009850     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009850
009860     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009860
009870     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009870
009880     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009880
009890     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009890
009900     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009900
009910     05  FILLER                    PIC S9(9)     COMP-3 VALUE +0. 00009910
009920     05  FILLER                    PIC S9(3)     COMP-3 VALUE +0. 00009920
009930     05  FILLER                    PIC S9(7)     COMP-3 VALUE +0. 00009930
009940     05  FILLER                    PIC S9(7)     COMP-3 VALUE +0. 00009940
009950     05  FILLER                    PIC S9(5)     COMP-3 VALUE +0. 00009950
009960     05  FILLER                    PIC X(33)     VALUE SPACES.    00009960
009970     05  FILLER                    PIC S9(7)     COMP-3 VALUE +0. 00009970
009971     05  FILLER                    PIC S9(11)V99 COMP-3 VALUE +0. 00009971
010020     05  FILLER                    PIC S9(9)V99  COMP-3 VALUE +0. 00010020
010030     05  FILLER                    PIC S9(9)V99  COMP-3 VALUE +0. 00010030
010040     05  FILLER                    PIC X(01)     VALUE SPACE.     00010040
010041     05  FILLER                    PIC S9V9(06)  COMP-3 VALUE +0. 00010041
010042     05  FILLER                    PIC S9V9(06)  COMP-3 VALUE +0. 00010042
010043     05  FILLER                    PIC S9V9(06)  COMP-3 VALUE +0. 00010043
010044     05  FILLER                    PIC S9(07)V99 COMP-3 VALUE +0. 00010044
010045     05  FILLER                    PIC S9(07)V99 COMP-3 VALUE +0. 00010045
010046     05  FILLER                    PIC S9(5)     COMP-3 VALUE +0. 00010046
010047     05  FILLER                    PIC S9(9)V99  COMP-3 VALUE +0. 00010047
010049     05  FILLER                    PIC X(92)     VALUE SPACES.    00010049
010050*******                                                           00010050
010060*******          WREHOUSE CONTROL BLOCK RECORD                    00010060
010070*******               RECORD CODE = 3; SUPPLR NO = 999999         00010070
010080*******                                                           00010080
010090 01  ZERO-WREHOUSE-BLOCK.                                         00010090
010100     05  FILLER                    PIC 9(7)   COMP-3 VALUE 0.     00010100
010110     05  FILLER                    PIC 9(1)          VALUE 3.     00010110
010120     05  FILLER                    PIC 9(5)          VALUE 99999. 00010120
010130     05  ZERO-DATE-ACTIVE          PIC S9(7)  COMP-3 VALUE +0.    00010130
010140     05  FILLER                    PIC X(5)          VALUE SPACE. 00010140
010150     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010150
010160     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010160
010170     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010170
010180     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010180
010190     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010190
010200     05  FILLER                    PIC S9(11) COMP-3 VALUE +0.    00010200
010210     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010210
010220     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010220
010230     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010230
010240     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010240
010250     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010250
010260     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010260
010270     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010270
010280     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010280
010290     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010290
010300     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010300
010310     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010310
010320     05  FILLER                    PIC S9(7)  COMP-3 VALUE +0.    00010320
010330     05  FILLER                    PIC S9(7)V99  COMP-3 VALUE +0. 00010330
010340     05  FILLER                    PIC S9(5)  COMP-3 VALUE +0.    00010340
010350     05  FILLER                    PIC S9(5)  COMP-3 VALUE +0.    00010350
010360     05  FILLER                    PIC S9(5)  COMP-3 VALUE +0.    00010360
010370     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010370
010380     05  FILLER                    PIC S9(5)  COMP-3 VALUE +0.    00010380
010390     05  FILLER                    PIC S9(5)  COMP-3 VALUE +0.    00010390
010400     05  FILLER                    PIC S9(5)  COMP-3 VALUE +0.    00010400
010410     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010410
010420     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010420
010430     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010430
010440     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010440
010450     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010450
010451     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010451
010452     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010452
010453     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010453
010454     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010454
010460     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010460
010461     05  FILLER                    PIC S9(09)V99 COMP-3  VALUE +0.00010461
010462     05  FILLER                    PIC S9(09)V99 COMP-3  VALUE +0.00010462
010470     05  FILLER                    PIC S9(7)  COMP-3 VALUE +0.    00010470
010480     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010480
010490     05  FILLER                    PIC S9(7)  COMP-3 VALUE +0.    00010490
010500     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010500
010510     05  FILLER                    PIC 9(1)          VALUE 0.     00010510
010520     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010520
010530     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010530
010540     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010540
010550     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010550
010560     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010560
010570     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010570
010580     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010580
010590     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010590
010600     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010600
010610     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010610
010620     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010620
010630     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010630
010640     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010640
010650     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010650
010660     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010660
010670     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010670
010680     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010680
010690     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010690
010700     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010700
010710     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010710
010720     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010720
010730     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010730
010740     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010740
010750     05  FILLER                    PIC S9(9)  COMP-3 VALUE +0.    00010750
010760     05  FILLER                    PIC S9(3)  COMP-3 VALUE +0.    00010760
010770     05  FILLER                    PIC S9(7)  COMP-3 VALUE +0.    00010770
010780     05  FILLER                    PIC X(5)          VALUE SPACES.00010780
010790     05  FILLER                    PIC S9(5)  COMP-3 VALUE +0.    00010790
010830     05  FILLER                    PIC S9(9)V99  COMP-3 VALUE +0. 00010830
010840     05  FILLER                    PIC S9(9)V99  COMP-3 VALUE +0. 00010840
010850     05  FILLER                    PIC S9(9)V99  COMP-3 VALUE +0. 00010850
010851     05  FILLER                    PIC X(28)     VALUE SPACES.    00010851
010852     05  FILLER                    PIC S9(9)V99  COMP-3 VALUE +0. 00010852
010853     05  FILLER                    PIC S9(9)V99  COMP-3 VALUE +0. 00010853
010854     05  FILLER                    PIC S9(9)V99  COMP-3 VALUE +0. 00010854
010860     05  FILLER                    PIC X(94) VALUE SPACES.        00010860
010870     EJECT                                                        00010870
010880 01  PRINT-HEADER-0.                                              00010880
010890     05  FILLER                      PIC X.                       00010890
010900     05  FILLER                      PIC X(25) VALUE              00010900
010910         'LOOKUP WREHOUSE LADING'.                                00010910
010920     05  FILLER                      PIC X(82) VALUE SPACE.       00010920
010930     05  FILLER                      PIC X(5) VALUE               00010930
010940         'DATE '.                                                 00010940
010950     05  HEADER-DATE                 PIC X(8).                    00010950
010960     05  FILLER                      PIC X(2) VALUE SPACE.        00010960
010970     05  FILLER                      PIC X(5) VALUE               00010970
010980         'PAGE '.                                                 00010980
010990     05  HEADER-PAGE-NO              PIC ZZZZ9.                   00010990
011000 01  PRINT-HEADER-1.                                              00011000
011010     05  FILLER                      PIC X.                       00011010
011020     05  FILLER                      PIC X(42) VALUE              00011020
011030         'WREHOUSE SUPPLR STORE   EXPNSE       T/C '.             00011030
011040     05  FILLER                      PIC X(7) VALUE SPACE.        00011040
011050     05  FILLER                      PIC X(40) VALUE              00011050
011060         'EXPNSE AMOUNT      REF  TRM XTRA DUE D '.               00011060
011070     05  FILLER                      PIC X(40) VALUE              00011070
011080         'R DAYS   T/A    LAST CHANGE  INV  FILM #'.              00011080
011090     05  FILLER                      PIC X(3) VALUE SPACE.        00011090
011100 01  PRINT-HEADER-2.                                              00011100
011110     05  FILLER                      PIC X.                       00011110
011120     05  FILLER                      PIC X(36) VALUE              00011120
011130         'NUMBER   NUMBER  NO   NUMBER    DATE'.                  00011130
011140     05  FILLER                      PIC X(10) VALUE SPACE.       00011140
011150     05  FILLER                      PIC X(41) VALUE              00011150
011160      'GROSS      FREIGHT     NO   CDE DAYS DTE '.                00011160
011170     05  FILLER                      PIC X(34) VALUE              00011170
011180         'CDE P.D.   DATE   DATE   CDE   PRC'.                    00011180
011190     05  FILLER                      PIC X(6) VALUE SPACE.        00011190
011200 01  PRINT-CUST-NAME.                                             00011200
011210     05  FILLER                      PIC X.                       00011210
011220     05  PRINT-NA-NO                 PIC X(8).                    00011220
011230     05  FILLER                      PIC X.                       00011230
011240     05  PRINT-NAME                  PIC X(30).                   00011240
011250     05  FILLER                      PIC X.                       00011250
011260     05  PRINT-ADDR1                 PIC X(30).                   00011260
011270     05  FILLER                      PIC X.                       00011270
011280     05  PRINT-ADDR2                 PIC X(30).                   00011280
011290     05  PRINT-CITY                  PIC X(16).                   00011290
011300     05  PRINT-STATE                 PIC X(5).                    00011300
011310     05  PRINT-LIST                  PIC ZZZZZZZ.                 00011310
011320     05  FILLER                      PIC X(3).                    00011320
011330 01  PRINT-CUST-SUPPLR-TOTAL.                                     00011330
011340     05  FILLER                      PIC X.                       00011340
011350     05  PRINT-IDENT                 PIC X(15).                   00011350
011360     05  FILLER                      PIC X(24) VALUE SPACE.       00011360
011370     05  PRINT-GROSS                 PIC ZZZ,ZZZ,ZZ9.99-.         00011370
011380     05  FILLER                      PIC X.                       00011380
011390     05  PRINT-NET                   PIC ZZZ,ZZZ,ZZ9.99-.         00011390
011400     05  FILLER                      PIC X(62) VALUE SPACE.       00011400
011410 01  PRINT-DETAIL-LINE.                                           00011410
011420     05  FILLER                      PIC X.                       00011420
011430     05  PRINT-DETAIL-CUST           PIC X(8).                    00011430
011440     05  FILLER                      PIC X.                       00011440
011450     05  PRINT-DETAIL-SUPPLR         PIC X(5).                    00011450
011460     05  FILLER                      PIC X.                       00011460
011470     05  PRINT-DETAIL-STORE          PIC 9(5).                    00011470
011480     05  FILLER                      PIC X.                       00011480
011490     05  PRINT-DETAIL-EXPNSE        PIC 9999999.                  00011490
011500     05  FILLER                      PIC X.                       00011500
011510     05  PRINT-DETAIL-EXPNSE-DATE   PIC X(8).                     00011510
011520     05  FILLER                      PIC X.                       00011520
011530     05  PRINT-DETAIL-TRAN-CD        PIC 999.                     00011530
011540     05  FILLER                      PIC X.                       00011540
011550     05  PRINT-DETAIL-GROSS          PIC ZZZZ,ZZ9.99.             00011550
011560     05  PRINT-GROSS-MINUS           PIC X.                       00011560
011570     05  FILLER                      PIC X.                       00011570
011580     05  PRINT-DETAIL-FREIGHT        PIC ZZZZ,ZZ9.99.             00011580
011590     05  PRINT-FREIGHT-MINUS         PIC X.                       00011590
011600     05  FILLER                      PIC X.                       00011600
011610     05  PRINT-DETAIL-REF            PIC ZZZZZ.                   00011610
011620     05  FILLER                      PIC X.                       00011620
011630     05  PRINT-DETAIL-TERMS-DAYS.                                 00011630
011640        10  PRINT-DETAIL-TERMS       PIC 999.                     00011640
011650        10  FILLER                   PIC X.                       00011650
011660        10  PRINT-DETAIL-EXTRA-DAYS  PIC ZZZ.                     00011660
011670     05  FILLER REDEFINES PRINT-DETAIL-TERMS-DAYS.                00011670
011680        10  FILLER                   PIC X.                       00011680
011690        10  PRINT-DETAIL-RATE        PIC ZZ.ZZZ.                  00011690
011700     05  FILLER                      PIC X.                       00011700
011710     05  PRINT-DETAIL-DUE-DATE       PIC X(4).                    00011710
011720     05  FILLER                      PIC X.                       00011720
011730     05  PRINT-DETAIL-DISPUTE-CODE   PIC X.                       00011730
011740     05  FILLER                      PIC X.                       00011740
011750     05  PRINT-DETAIL-RECOURSE-CODE  PIC X.                       00011750
011760     05  FILLER                      PIC XX.                      00011760
011770     05  PRINT-DETAIL-PAST-DUE       PIC ZZZ.                     00011770
011780     05  FILLER                      PIC X.                       00011780
011790     05  PRINT-DETAIL-DATE-POSTED    PIC X(8).                    00011790
011800     05  FILLER                      PIC X.                       00011800
011810     05  PRINT-DETAIL-LAST-CHANGE    PIC X(8).                    00011810
011820     05  FILLER                      PIC X.                       00011820
011830     05  PRINT-DETAIL-LAST-TC        PIC ZZZ.                     00011830
011840     05  FILLER                      PIC X.                       00011840
011850     05  PRINT-DETAIL-PROCESSOR      PIC XXX.                     00011850
011860     05  FILLER                      PIC X.                       00011860
011870     05  PRINT-DETAIL-FILM-NO        PIC 9(11).                   00011870
011880     05  FILLER REDEFINES PRINT-DETAIL-FILM-NO.                   00011880
011890        10  PRINT-DETAIL-PRICE       PIC 999.                     00011890
011900        10  FILLER                   PIC X.                       00011900
011910        10  PRINT-DETAIL-SEQ         PIC 999.                     00011910
011920        10  FILLER                   PIC X.                       00011920
011930        10  PRINT-DETAIL-REASON      PIC 99.                      00011930
011940        10  FILLER                   PIC X.                       00011940
011950 01  PRINT-TOTAL-0.                                               00011950
011960     05  FILLER                      PIC X.                       00011960
011970     05  FILLER                      PIC X(34) VALUE              00011970
011980         'LOOKUP PROOF AND CONTROL TOTALS'.                       00011980
011990     05  FILLER                      PIC X(82) VALUE SPACE.       00011990
012000     05  FILLER                      PIC X(5) VALUE               00012000
012010         'DATE '.                                                 00012010
012020     05  TOTAL-DATE                  PIC X(8).                    00012020
012030     05  FILLER                      PIC X(3) VALUE SPACE.        00012030
012040 01  PRINT-TOTAL-D.                                               00012040
012050     05  FILLER                      PIC X.                       00012050
012060     05  FILLER                      PIC X(46) VALUE  SPACE.      00012060
012070     05  FILLER                      PIC X(60) VALUE              00012070
012080         'RCPARTS - CONTROL TOTALS BY SALES STORUNIT'.            00012080
012090     05  FILLER                      PIC X(46) VALUE SPACE.       00012090
012100 01  PRINT-TOTAL-1.                                               00012100
012110     05  FILLER                      PIC X.                       00012110
012120     05  FILLER                      PIC X(28) VALUE SPACE.       00012120
012130     05  FILLER                      PIC X(40) VALUE              00012130
012140         'NO. OF CUST    NO. OF CUST   '.                         00012140
012150     05  FILLER                      PIC X(5) VALUE               00012150
012160         'GROSS'.                                                 00012160
012170     05  FILLER                      PIC X(12) VALUE SPACE.       00012170
012180     05  FILLER                      PIC X(8) VALUE               00012180
012190         'X876OUNT'.                                              00012190
012200     05  FILLER                      PIC X(13) VALUE SPACE.       00012200
012210     05  FILLER                      PIC X(3) VALUE               00012210
012220         'NET'.                                                   00012220
012230     05  FILLER                      PIC X(6) VALUE SPACE.        00012230
012240 01  PRINT-TOTAL-1D.                                              00012240
012250     05  FILLER                      PIC X.                       00012250
012260     05  FILLER                      PIC X(28) VALUE SPACE.       00012260
012270     05  FILLER                      PIC X(40) VALUE SPACE.       00012270
012280     05  FILLER                      PIC X(5) VALUE               00012280
012290         'GROSS'.                                                 00012290
012300     05  FILLER                      PIC X(12) VALUE SPACE.       00012300
012310     05  FILLER                      PIC X(8) VALUE               00012310
012320         'X876OUNT'.                                              00012320
012330     05  FILLER                      PIC X(13) VALUE SPACE.       00012330
012340     05  FILLER                      PIC X(3) VALUE               00012340
012350         'NET'.                                                   00012350
012360     05  FILLER                      PIC X(6) VALUE SPACE.        00012360
012370 01  PRINT-TOTAL-2D.                                              00012370
012380     05  FILLER                      PIC X.                       00012380
012390     05  FILLER                      PIC X(11) VALUE SPACE.       00012390
012400     05  FILLER                      PIC X(15) VALUE              00012400
012410         'SALES STORUNIT '.                                       00012410
012420     05  PRINT-STORUNIT              PIC 9(3).                    00012420
012430     05  FILLER                      PIC X(24) VALUE SPACE.       00012430
012440     05  FILLER                      PIC X(6) VALUE               00012440
012450         'SALESS'.                                                00012450
012460     05  FILLER                      PIC X(14) VALUE SPACE.       00012460
012470     05  FILLER                      PIC X(7) VALUE               00012470
012480         'RETURNS'.                                               00012480
012490     05  FILLER                      PIC X(7) VALUE SPACE.        00012490
012500     05  FILLER                      PIC X(22) VALUE              00012500
012510         'GENERAL LADING ACCOUNT'.                                00012510
012520     05  FILLER                      PIC X(18) VALUE SPACE.       00012520
012530 01  PRINT-TOTAL-2.                                               00012530
012540     05  FILLER                      PIC X.                       00012540
012550     05  FILLER                      PIC X(28) VALUE SPACE.       00012550
012560     05  FILLER                      PIC X(40) VALUE              00012560
012570         'ACCTS ON FILE  ACCTS WITH BAL'.                         00012570
012580     05  FILLER                      PIC X(7) VALUE               00012580
012590         'AMOUNTS'.                                               00012590
012600     05  FILLER                      PIC X(10) VALUE SPACE.       00012600
012610     05  FILLER                      PIC X(7) VALUE               00012610
012620         'AMOUNTS'.                                               00012620
012630     05  FILLER                      PIC X(14) VALUE SPACE.       00012630
012640     05  FILLER                      PIC X(7) VALUE               00012640
012650         'AMOUNTS'.                                               00012650
012660     05  FILLER                      PIC X(2) VALUE SPACE.        00012660
012670 01  PRINT-TOTAL-3.                                               00012670
012680     05  FILLER                      PIC X.                       00012680
012690     05  TOTAL-IDENT                 PIC X(15).                   00012690
012700     05  FILLER                      PIC X(117) VALUE SPACE.      00012700
012710 01  PRINT-TOTAL-3D.                                              00012710
012720     05  FILLER                      PIC X.                       00012720
012730     05  TOTAL-IDENT-D               PIC X(40).                   00012730
012740     05  FILLER                      PIC X(92) VALUE SPACE.       00012740
012750                                                                  00012750
012760 01  PRINT-TOTAL-4D.                                              00012760
012770     05  FILLER                      PIC X.                       00012770
012780     05  FILLER                      PIC X(40) VALUE              00012780
012790         'BIAS RECORDS WRITTEN'.                                  00012790
012800     05  TOTAL-BIAS-RECS             PIC ZZZ,ZZ9.                 00012800
012810     05  FILLER                      PIC X(84) VALUE SPACE.       00012810
012820                                                                  00012820
012830 01  PRINT-TOTAL-LINE.                                            00012830
012840     05  FILLER                      PIC X.                       00012840
012850     05  TOTAL-LABEL                 PIC X(23).                   00012850
012860     05  FILLER                      PIC X(6) VALUE SPACE.        00012860
012870     05  TOTAL-PREV-IN               PIC ZZZZ,ZZ9.                00012870
012880     05  FILLER                      PIC X(6) VALUE SPACE.        00012880
012890     05  TOTAL-PREV-BAL              PIC ZZZZ,ZZ9.                00012890
012900     05  FILLER                      PIC X(06) VALUE SPACE.       00012900
012910     05  TOTAL-GROSS                 PIC ZZZ,ZZZ,ZZZ,ZZZ.99-.     00012910
012920     05  FILLER                      PIC X(1) VALUE SPACE.        00012920
012930     05  TOTAL-IRSOUNT              PIC ZZZ,ZZZ,ZZZ,ZZZ.99-.      00012930
012940     05  FILLER                      PIC X(1) VALUE SPACE.        00012940
012950     05  TOTAL-NET                   PIC ZZZ,ZZZ,ZZZ,ZZZ.99-.     00012950
012960     05  FILLER                      PIC X(16) VALUE SPACE.       00012960
012830 01  BILL-LADING-REC1.                                            00012830
012840     05  FILLER                      PIC X.                       00012840
012850     05  TOTAL-LABEL1                 PIC X(23).                  00012850
012860     05  FILLER                      PIC X(6) VALUE SPACE.        00012860
012870     05  TOTAL-PREV-IN1              PIC ZZZZ,ZZ9.                00012870
012880     05  FILLER                      PIC X(6) VALUE SPACE.        00012880
012890     05  TOTAL-PREV-BAL1              PIC ZZZZ,ZZ9.               00012890
012900     05  FILLER                      PIC X(06) VALUE SPACE.       00012900
012910     05  TOTAL-GROSS1                 PIC ZZZ,ZZZ,ZZZ,ZZZ.99-.    00012910
012920     05  FILLER                      PIC X(1) VALUE SPACE.        00012920
012930     05  TOTAL-IRSOUNT1              PIC ZZZ,ZZZ,ZZZ,ZZZ.99-.     00012930
012940     05  FILLER                      PIC X(1) VALUE SPACE.        00012940
012950     05  TOTAL-NET1                   PIC ZZZ,ZZZ,ZZZ,ZZZ.99-.    00012950
012960     05  FILLER                      PIC X(16) VALUE SPACE.       00012960
      ****                                                                      
012830 01  GL-SUS-SOURCE           PIC X(01).                           00012830
012830 01  GL-SUS-SEQUENCE-NUMBER                PIC X(01).             00012830
012830 01  GL-SUS-REC           PIC X(01).                              00012830
012830 01  GL-SUS-PROGRAM               PIC X(01).                      00012830
012830 01  GL-SUS-PROCESSOR-CODE           PIC X(01).                   00012830
012830 01  GL-SUS-LOOKUP-DATE                PIC X(01).                 00012830
012830 01  GL-SUS-OFFICER-CODE           PIC X(01).                     00012830
012830 01  GL-SUS-ENTRY-TYPE                PIC X(01).                  00012830
012830 01  GL-SUS-ENTRY-SOURCE           PIC X(01).                     00012830
012830 01  GL-SUS-ENTRY-DATE                PIC X(01).                  00012830
012830 01  GL-SUS-DESCRIPTION          PIC X(01).                       00012830
012830 01  GL-SUS-DB-CR-CODE                PIC X(01).                  00012830
012830 01  GL-SUS-CENTER           PIC X(01).                           00012830
012830 01  GL-SUS-PRICE-DATE                PIC X(01).                  00012830
012830 01  GL-SUS-BANK                PIC X(01).                        00012830
012830 01  GL-SUS-AMOUNT           PIC X(01).                           00012830
012830 01  GL-SUS-ACCOUNT                PIC X(01).                     00012830
012830 01  GL-SUS-ACCT-TYPE                PIC X(01).                   00012830
012830 01  GL-SUS-ACCT-REC           PIC X(01).                         00012830
012830 01  GL-SUS-ACCT-KEYWORD                PIC X(01).                00012830
012830 01  GL-SUS-ACCT-STORUNIT           PIC X(01).                    00012830
012830 01  GL-SUS-ACCT-CENTER                PIC X(01).                 00012830
012830 01  GL-SUS-ACCT-BANK               PIC X(01).                    00012830
012830 01  GL-SUS-ACCT-AMOUNT           PIC X(01).                      00012830
                                                                                
      *** HERE                                                                  
012830 01  CURRENT-DATE                PIC X(01).                       00012830
012830 01  CUST-PRCE1-LIMIT           PIC 9(01).                        00012830
      *                                                                         
012830 01  LIST-ACCT-REC2.                                              00012830
012840     05  FILLER                      PIC X.                       00012840
012850     05  TOTAL-LABEL2                 PIC X(23).                  00012850
012860     05  FILLER                      PIC X(6) VALUE SPACE.        00012860
012870     05  TOTAL-PREV-IN2               PIC ZZZZ,ZZ9.               00012870
012880     05  FILLER                      PIC X(6) VALUE SPACE.        00012880
012890     05  TOTAL-PREV-BAL2              PIC ZZZZ,ZZ9.               00012890
012900     05  FILLER                      PIC X(06) VALUE SPACE.       00012900
012910     05  TOTAL-GROSS2                 PIC ZZZ,ZZZ,ZZZ,ZZZ.99-.    00012910
012920     05  FILLER                      PIC X(1) VALUE SPACE.        00012920
012930     05  TOTAL-IRSOUNT2              PIC ZZZ,ZZZ,ZZZ,ZZZ.99-.     00012930
012940     05  FILLER                      PIC X(1) VALUE SPACE.        00012940
012950     05  TOTAL-NET2                   PIC ZZZ,ZZZ,ZZZ,ZZZ.99-.    00012950
012960     05  FILLER                      PIC X(16) VALUE SPACE.       00012960
012970 01  PRINT-STORUNIT-LINE.                                         00012970
012980     05  FILLER                      PIC X.                       00012980
012990     05  FILLER                      PIC XX VALUE SPACE.          00012990
013000     05  DIV-CONTROL-LABEL           PIC X(38).                   00013000
013010     05  FILLER                      PIC X(7) VALUE SPACE.        00013010
013020     05  DIV-TOTAL-SALES             PIC ZZZ,ZZZ,ZZZ.99-.         00013020
013030     05  FILLER                      PIC X(5) VALUE SPACE.        00013030
013040     05  DIV-TOTAL-PRCE1            PIC ZZZ,ZZZ,ZZZ.99-.          00013040
013050     05  FILLER                      PIC X(5) VALUE SPACE.        00013050
013060     05  DIV-PT-LABEL                PIC X(40).                   00013060
013070 01  PRINT-STAT-LINE.                                             00013070
013080     05  FILLER                      PIC X.                       00013080
013090     05  FILLER                      PIC XX VALUE SPACE.          00013090
013100     05  FILLER                      PIC X(30)                    00013100
013110             VALUE 'REACTIVATED WREHOUSES AFTER:'.                00013110
013120     05  FILLER                      PIC X(5) VALUE SPACE.        00013120
013130     05  FILLER                      PIC X(15)                    00013130
013140             VALUE '12-14 MONTHS'.                                00013140
013150     05  TOTAL-12                    PIC ZZ,ZZZ,ZZ9.              00013150
013160     05  FILLER                      PIC X(5) VALUE SPACE.        00013160
013170     05  FILLER                      PIC X(15)                    00013170
013180             VALUE '15-17 MONTHS'.                                00013180
013190     05  TOTAL-15                    PIC ZZ,ZZZ,ZZ9.              00013190
013200     05  FILLER                      PIC X(5) VALUE SPACE.        00013200
013210     05  FILLER                      PIC X(15)                    00013210
013220             VALUE '18+ MONTHS'.                                  00013220
013230     05  TOTAL-18                    PIC ZZ,ZZZ,ZZ9.              00013230
013240     05  FILLER                      PIC X(10) VALUE SPACE.       00013240
013250     EJECT                                                        00013250
013260 01  SAVE-HEADER-RECORD              PIC X(300).                  00013260
013270 01  SAVE-TRAN-REC1.                                              00013270
013280     05  FILLER                      PIC X(3).                    00013280
013290     05  SAVE-REC1-CUST-NO           PIC 9(7).                    00013290
013300     05  FILLER                      PIC X(80).                   00013300
013310 01  SAVE-TRAN-REC2.                                              00013310
013320     05  FILLER                      PIC X(3).                    00013320
013330     05  SAVE-REC2-CUST-NO           PIC 9(7).                    00013330
013340     05  FILLER                      PIC X(80).                   00013340
013350 01  SAVE-TRAN-REC3.                                              00013350
013360     05  FILLER                      PIC X(3).                    00013360
013370     05  SAVE-REC3-CUST-NO           PIC 9(7).                    00013370
013380     05  FILLER                      PIC X(80).                   00013380
014110*                                                                 00014110
013400******************************************************************00013400
013410*                                                                *00013410
013420*      INSERT ADDITIONAL RECORD AREAS HERE                       *00013420
013430*                                                                *00013430
013440******************************************************************00013440
013450*                                                                 00013450
013460*                                                                 00013460
013470 01  WORKING-STORAGE-AREAS.                                       00013470
013480     05  WA00-SUB-TOTALS-AMOUNTS            PIC X(12) VALUE       00013480
013490             'SUB-TOTALS'.                                        00013490
013500*                                                                 00013500
013510     05  WS-1-SUB-TOTALS-AMOUNT.                                  00013510
013520*    SBTOTS FOR WREHOUSE CONTROL BLOCK                            00013520
013530         10  WS-1-REC3-DISPUTE     PIC S9(9)V99 COMP-3 VALUE ZERO.00013530
013540         10  WS-1-REC3-DEDUCTIONS  PIC S9(9)V99 COMP-3 VALUE ZERO.00013540
013550         10  WS-1-REC3-LOC1        PIC S9(9)V99 COMP-3 VALUE ZERO.00013550
013560         10  WS-1-REC3-INT-WAIVED  PIC S9(9)V99 COMP-3 VALUE ZERO.00013560
013570         10  WS-1-REC3-IRS-WAIVED PIC S9(9)V99 COMP-3 VALUE ZERO. 00013570
013580         10  WS-1-REC3-DIFF-ABS    PIC S9(9)V99 COMP-3 VALUE ZERO.00013580
013590         10  WS-1-REC3-RET-CK      PIC 9(3) COMP-3 VALUE ZERO.    00013590
013600         10  WS-1-REC3-WEIGHTED-DAYS PIC 9(3) COMP-3 VALUE ZERO.  00013600
013610         10  WS-1-REC3-CK-INDIC    PIC 9(1) VALUE ZERO.           00013610
013620         10  WS-1-REC3-PROMPT      PIC S9(9)V99 COMP-3 VALUE ZERO.00013620
013630         10  WS-1-REC3-LATE        PIC S9(9)V99 COMP-3 VALUE ZERO.00013630
013640         10  WS-1-REC3-DOLLAR      PIC S9(9)V99 COMP-3 VALUE ZERO.00013640
013650         10  WS-1-REC3-INV-PD      PIC S9(9)V99 COMP-3 VALUE ZERO.00013650
013660         10  WS-1-REC3-ACCOUNT-BAL PIC S9(9)V99 COMP-3 VALUE ZERO.00013660
013670         10  WS-1-REC3-SALES-YTD   PIC S9(9)V99 COMP-3 VALUE ZERO.00013670
013680         10  WS-1-REC3-CRED-YTD    PIC S9(9)V99 COMP-3 VALUE ZERO.00013680
013690         10  WS-1-REC3-MAT-10      PIC S9(9)V99 COMP-3 VALUE ZERO.00013690
013700         10  WS-1-REC3-MAT-30      PIC S9(9)V99 COMP-3 VALUE ZERO.00013700
013710         10  WS-1-REC3-MAT-60      PIC S9(9)V99 COMP-3 VALUE ZERO.00013710
013720         10  WS-1-REC3-MAT-90      PIC S9(9)V99 COMP-3 VALUE ZERO.00013720
013730         10  WS-1-REC3-MAT-120     PIC S9(9)V99 COMP-3 VALUE ZERO.00013730
013740         10  WS-1-REC3-MAT-180     PIC S9(9)V99 COMP-3 VALUE ZERO.00013740
013750         10  WS-1-REC3-MAT-181     PIC S9(9)V99 COMP-3 VALUE ZERO.00013750
013760         10  WS-1-REC3-APPR-60     PIC S9(9)V99 COMP-3 VALUE ZERO.00013760
013770         10  WS-1-REC3-APPR-120    PIC S9(9)V99 COMP-3 VALUE ZERO.00013770
013780         10  WS-1-REC3-APPR-180    PIC S9(9)V99 COMP-3 VALUE ZERO.00013780
013790         10  WS-1-REC3-APPR-181    PIC S9(9)V99 COMP-3 VALUE ZERO.00013790
013800         10  WS-1-REC3-INV-60      PIC S9(9)V99 COMP-3 VALUE ZERO.00013800
013810         10  WS-1-REC3-INV-120     PIC S9(9)V99 COMP-3 VALUE ZERO.00013810
013820         10  WS-1-REC3-INV-180     PIC S9(9)V99 COMP-3 VALUE ZERO.00013820
013830         10  WS-1-REC3-INV-181     PIC S9(9)V99 COMP-3 VALUE ZERO.00013830
013840         10  WS-1-REC3-ACTIVE-DATE PIC 9(7) COMP-3 VALUE ZERO.    00013840
013850         10  WS-1-REC3-APPR-DATE   PIC 9(7) COMP-3 VALUE ZERO.    00013850
013860         10  WS-1-REC3-APPR-TC     PIC 9(3) COMP-3 VALUE ZERO.    00013860
013870         10  WS-1-REC3-COLR-DATE   PIC 9(7) COMP-3 VALUE ZERO.    00013870
013880         10  WS-1-REC3-COLR-AMT    PIC S9(9)V99 COMP-3 VALUE ZERO.00013880
013890         10  WS-1-REC3-MAT-GROSS   PIC S9(9)V99 COMP-3 VALUE ZERO.00013890
013900         10  WS-1-REC3-BAL-4XX     PIC S9(9)V99 COMP-3 VALUE ZERO.00013900
013910         10  WS-1-OUTSTAND-PRCHS-ORD                              00013910
013920                                   PIC S9(9)V99 COMP-3 VALUE ZERO.00013920
013930         10  WS-1-NOT-ON-TABLE-COUNT                              00013930
013940                                   PIC S9(9)V99 COMP-3 VALUE ZERO.00013940
013950*    TOTALS FOR TOTAL PAGE                                        00013950
013960         10  WS-1-TOTAL-BIAS-RECS  PIC  9(9)    COMP-3 VALUE ZERO.00013960
013970         10  WS-1-12-MONTHS        PIC  9(9)    COMP-3 VALUE ZERO.00013970
013980         10  WS-1-15-MONTHS        PIC  9(9)    COMP-3 VALUE ZERO.00013980
013990         10  WS-1-18-MONTHS        PIC  9(9)    COMP-3 VALUE ZERO.00013990
014000         10  WS-1-PREV-GROSS      PIC S9(11)V99 COMP-3 VALUE ZERO.00014000
014010         10  WS-1-PREV-FIRST      PIC S9(11)V99 COMP-3 VALUE ZERO.00014010
014020         10  WS-1-PREV-NET        PIC S9(11)V99 COMP-3 VALUE ZERO.00014020
014030         10  WS-1-PREV-MEMO       PIC S9(11)V99 COMP-3 VALUE ZERO.00014030
014040         10  WS-1-WORK-BALANCE    PIC S9(11)V99 COMP-3 VALUE ZERO.00014040
014050         10  WS-1-SALES-POSTED-GROSS                              00014050
014060                                  PIC S9(11)V99 COMP-3 VALUE ZERO.00014060
014070         10  WS-1-SALES-POSTED-FIRST                              00014070
014080                                  PIC S9(11)V99 COMP-3 VALUE ZERO.00014080
014090         10  WS-1-SALES-POSTED-NET                                00014090
014100                                  PIC S9(11)V99 COMP-3 VALUE ZERO.00014100
014110         10  WS-1-SALES-DELETED-GROSS                             00014110
014120                                  PIC S9(11)V99 COMP-3 VALUE ZERO.00014120
014130         10  WS-1-SALES-DELETED-FIRST                             00014130
014140                                  PIC S9(11)V99 COMP-3 VALUE ZERO.00014140
014150         10  WS-1-SALES-DELETED-NET                               00014150
014160                                  PIC S9(11)V99 COMP-3 VALUE ZERO.00014160
014170         10  WS-1-PAY-POSTED-GROSS                                00014170
014180                                  PIC S9(11)V99 COMP-3 VALUE ZERO.00014180
014190         10  WS-1-PAY-POSTED-FIRST                                00014190
014200                                  PIC S9(11)V99 COMP-3 VALUE ZERO.00014200
014210         10  WS-1-PAY-POSTED-NET                                  00014210
014220                                  PIC S9(11)V99 COMP-3 VALUE ZERO.00014220
014230         10  WS-1-PAY-DELETED-GROSS                               00014230
014240                                  PIC S9(11)V99 COMP-3 VALUE ZERO.00014240
014250         10  WS-1-PAY-DELETED-FIRST                               00014250
014260                                  PIC S9(11)V99 COMP-3 VALUE ZERO.00014260
014270         10  WS-1-PAY-DELETED-NET                                 00014270
014280                                  PIC S9(11)V99 COMP-3 VALUE ZERO.00014280
014290         10  WS-1-JE-POSTED-GROSS PIC S9(11)V99 COMP-3 VALUE ZERO.00014290
014300         10  WS-1-JE-POSTED-FIRST PIC S9(11)V99 COMP-3 VALUE ZERO.00014300
014310         10  WS-1-JE-POSTED-NET   PIC S9(11)V99 COMP-3 VALUE ZERO.00014310
014320         10  WS-1-JE-DELETED-GROSS                                00014320
014330                                  PIC S9(11)V99 COMP-3 VALUE ZERO.00014330
014340         10  WS-1-JE-DELETED-FIRST                                00014340
014350                                  PIC S9(11)V99 COMP-3 VALUE ZERO.00014350
014360         10  WS-1-JE-DELETED-NET                                  00014360
014370                                  PIC S9(11)V99 COMP-3 VALUE ZERO.00014370
014380         10  WS-1-NEW-GROSS       PIC S9(11)V99 COMP-3 VALUE ZERO.00014380
014390         10  WS-1-NEW-FIRST       PIC S9(11)V99 COMP-3 VALUE ZERO.00014390
014400         10  WS-1-NEW-NET         PIC S9(11)V99 COMP-3 VALUE ZERO.00014400
014410         10  WS-1-IRSOUNT-POSTED PIC S9(11)V99 COMP-3 VALUE ZERO. 00014410
014420         10  WS-1-IRSOUNT-PAID   PIC S9(11)V99 COMP-3 VALUE ZERO. 00014420
014430         10  WS-1-IRSOUNT-WAIVED PIC S9(11)V99 COMP-3 VALUE ZERO. 00014430
014440         10  WS-1-INTEREST-POSTED PIC S9(11)V99 COMP-3 VALUE ZERO.00014440
014450         10  WS-1-INTEREST-PAID   PIC S9(11)V99 COMP-3 VALUE ZERO.00014450
014460         10  WS-1-INTEREST-WAIVED PIC S9(11)V99 COMP-3 VALUE ZERO.00014460
014470         10  WS-1-NEW-MEMO        PIC S9(11)V99 COMP-3 VALUE ZERO.00014470
014480         10  WS-1-WREHOUSE-BAL    PIC S9(11)V99 COMP-3 VALUE 0.   00014480
014490*                                                                 00014490
014500     05  WS-1-SUB-TOTALS-NUMBER.                                  00014500
014510         10  WS-1-NEW-CUST         PIC S9(7) COMP-3 VALUE ZERO.   00014510
014520         10  WS-1-NEW-BAL          PIC S9(7) COMP-3 VALUE ZERO.   00014520
014530         10  WS-1-NEW-OPEN-DETAILS PIC S9(7) COMP-3 VALUE ZERO.   00014530
014540         10  WS-1-PREV-CUST        PIC S9(7) COMP-3 VALUE ZERO.   00014540
014550         10  WS-1-PREV-BAL         PIC S9(7) COMP-3 VALUE ZERO.   00014550
014560*                                                                 00014560
014570     05  WS-1-SUPPLR-PRICE-PD   PIC S9(9)V99 COMP-3 VALUE ZERO.   00014570
014580     05  WS-1-SUPPLR-NET           PIC S9(9)V99 COMP-3 VALUE ZERO.00014580
014590     05  WS-1-SUPPLR-GROSS         PIC S9(9)V99 COMP-3 VALUE ZERO.00014590
014600     05  WS-1-WREHOUSE-NET         PIC S9(9)V99 COMP-3 VALUE ZERO.00014600
014610     05  WS-1-WREHOUSE-GROSS       PIC S9(9)V99 COMP-3 VALUE ZERO.00014610
014620* JE SUMMARY REPORT RECORD                                        00014620
014630     05  WS-1-HEIGH-BAD-COLR       PIC S9(9) COMP-3 VALUE ZERO.   00014630
014640     05  WS-1-HEIGH-CHARGE-BACKS   PIC S9(9) COMP-3 VALUE ZERO.   00014640
014650     05  WS-1-HEIGH-NO-TERMS       PIC 9(5) COMP-3 VALUE ZERO.    00014650
014660     05  WS-1-HEIGH-NO-CBS         PIC 9(5) COMP-3 VALUE ZERO.    00014660
014670*                                                                 00014670
014680     05  WS-1-PAGE                 PIC 9(4) COMP-3 VALUE 0.       00014680
014690     05  WS-1-LINE                 PIC 9(4) COMP-3 VALUE 56.      00014690
014700     05  WS-1-OOB-COUNT            PIC 9(2) COMP-3 VALUE 0.       00014700
014710     05  WS-1-REJECT-COUNT         PIC 9(5) COMP-3 VALUE 0.       00014710
014720*                                                                 00014720
014730 01  SIZE-STORUNIT-WORK-TABLE.                                    00014730
014740        05  SIZE-STORUNIT-LABEL     PIC X(16) VALUE 'DIV TABLE'.  00014740
014750        05  SIZE-STORUNIT-SBTOTULATORS.                           00014750
014760         10  FILLER.                                              00014760
014770            15  FILLER                 PIC 9(3) COMP-3 VALUE 000. 00014770
014780            15  FILLER                 PIC X(150).                00014780
014790         10  FILLER.                                              00014790
014800            15  FILLER                 PIC 9(3) COMP-3 VALUE 100. 00014800
014810            15  FILLER                 PIC X(150).                00014810
014820         10  FILLER.                                              00014820
014830            15  FILLER                 PIC 9(3) COMP-3 VALUE 200. 00014830
014840            15  FILLER                 PIC X(150).                00014840
014850         10  FILLER.                                              00014850
014860            15  FILLER                 PIC 9(3) COMP-3 VALUE 225. 00014860
014870            15  FILLER                 PIC X(150).                00014870
014880         10  FILLER.                                              00014880
014890            15  FILLER                 PIC 9(3) COMP-3 VALUE 250. 00014890
014900            15  FILLER                 PIC X(150).                00014900
014910         10  FILLER.                                              00014910
014920            15  FILLER                 PIC 9(3) COMP-3 VALUE 275. 00014920
014930            15  FILLER                 PIC X(150).                00014930
014940         10  FILLER.                                              00014940
014950            15  FILLER                 PIC 9(3) COMP-3 VALUE 300. 00014950
014960            15  FILLER                 PIC X(150).                00014960
014970         10  FILLER.                                              00014970
014980            15  FILLER                 PIC 9(3) COMP-3 VALUE 325. 00014980
014990            15  FILLER                 PIC X(150).                00014990
015000         10  FILLER.                                              00015000
015010            15  FILLER                 PIC 9(3) COMP-3 VALUE 350. 00015010
015020            15  FILLER                 PIC X(150).                00015020
015030         10  FILLER.                                              00015030
015040            15  FILLER                 PIC 9(3) COMP-3 VALUE 375. 00015040
015050            15  FILLER                 PIC X(150).                00015050
015060         10  FILLER.                                              00015060
015070            15  FILLER                 PIC 9(3) COMP-3 VALUE 500. 00015070
015080            15  FILLER                 PIC X(150).                00015080
015090         10  FILLER.                                              00015090
015100            15  FILLER                 PIC 9(3) COMP-3 VALUE 510. 00015100
015110            15  FILLER                 PIC X(150).                00015110
015120         10  FILLER.                                              00015120
015130            15  FILLER                 PIC 9(3) COMP-3 VALUE 520. 00015130
015140            15  FILLER                 PIC X(150).                00015140
015150         10  FILLER.                                              00015150
015160            15  FILLER                 PIC 9(3) COMP-3 VALUE 530. 00015160
015170            15  FILLER                 PIC X(150).                00015170
015180         10  FILLER.                                              00015180
015190            15  FILLER                 PIC 9(3) COMP-3 VALUE 540. 00015190
015200            15  FILLER                 PIC X(150).                00015200
015210         10  FILLER.                                              00015210
015220            15  FILLER                 PIC 9(3) COMP-3 VALUE 550. 00015220
015230            15  FILLER                 PIC X(150).                00015230
015240         10  FILLER.                                              00015240
015250            15  FILLER                 PIC 9(3) COMP-3 VALUE 560. 00015250
015260            15  FILLER                 PIC X(150).                00015260
015270         10  FILLER.                                              00015270
015280            15  FILLER                 PIC 9(3) COMP-3 VALUE 570. 00015280
015290            15  FILLER                 PIC X(150).                00015290
015300         10  FILLER.                                              00015300
015310            15  FILLER                 PIC 9(3) COMP-3 VALUE 580. 00015310
015320            15  FILLER                 PIC X(150).                00015320
015330         10  FILLER.                                              00015330
015340            15  FILLER                 PIC 9(3) COMP-3 VALUE 590. 00015340
015350            15  FILLER                 PIC X(150).                00015350
015360         10  FILLER.                                              00015360
015370            15  FILLER                 PIC 9(3) COMP-3 VALUE 600. 00015370
015380            15  FILLER                 PIC X(150).                00015380
015390         10  FILLER.                                              00015390
015400            15  FILLER                 PIC 9(3) COMP-3 VALUE 610. 00015400
015410            15  FILLER                 PIC X(150).                00015410
015420     05  SIZE-STORUNIT-SBTOT-TABLE                                00015420
015430                          REDEFINES SIZE-STORUNIT-SBTOTULATORS.   00015430
015440         10 SIZE-DIV-TABLE-ENTRY OCCURS 22 TIMES                  00015440
015450                 INDEXED BY SBTOT-SUBSC.                          00015450
015460            15  SIZE-SALES-STORUNIT         PIC 9(3) COMP-3.      00015460
015470            15  SIZE-TOTAL-PRICE0-PROCESSED PIC S9(9)V99 COMP-3.  00015470
015480            15  SIZE-KNOWN-LOCATION-ADJ     PIC S9(9)V99 COMP-3.  00015480
015490            15  SIZE-NET-SUPPLIERLOC-CHG-COLR PIC S9(9)V99 COMP-3.00015490
015500            15  SIZE-WREHOUSE-INCOME        PIC S9(9)V99 COMP-3.  00015500
015510            15  SIZE-DORMANT-RETURNS-USED   PIC S9(9)V99 COMP-3.  00015510
015520            15  SIZE-BAD-COLR-RECOVERIES    PIC S9(9)V99 COMP-3.  00015520
015530            15  SIZE-CATALOG001-COST        PIC S9(9)V99 COMP-3.  00015530
015540            15  SIZE-SUPPLR-RETURNS-CC      PIC S9(9)V99 COMP-3.  00015540
015550            15  SIZE-SUPPLR-SALESS          PIC S9(9)V99 COMP-3.  00015550
015560            15  SIZE-RETURN-PRICE0-POSTED   PIC S9(9)V99 COMP-3.  00015560
015570            15  SIZE-VOUCHERS               PIC S9(9)V99 COMP-3.  00015570
015580            15  SIZE-SUPPLR-EXPNSE         PIC S9(9)V99 COMP-3.   00015580
015590            15  SIZE-SUPPLR-RETURNS-JE      PIC S9(9)V99 COMP-3.  00015590
015600            15  SIZE-BAD-COLR-CHARGE-OFFS   PIC S9(9)V99 COMP-3.  00015600
015610            15  SIZE-MISC-OFFSETS           PIC S9(9)V99 COMP-3.  00015610
015620            15  SIZE-NET-SUPPLIERLOC-CHG-JE PIC S9(9)V99 COMP-3.  00015620
015630            15  SIZE-REFUNDED-TO-SUPPLR     PIC S9(9)V99 COMP-3.  00015630
015640            15  SIZE-FE-CONTRACT-CODES      PIC S9(9)V99 COMP-3.  00015640
015650            15  SIZE-OTHER-CONTRACT-CODES   PIC S9(9)V99 COMP-3.  00015650
015660            15  SIZE-PREV-GROSS             PIC S9(9)V99 COMP-3.  00015660
015670            15  SIZE-PREV-IRSOUNT          PIC S9(9)V99 COMP-3.   00015670
015680            15  SIZE-PREV-NET               PIC S9(9)V99 COMP-3.  00015680
015690            15  SIZE-CURRENT-GROSS          PIC S9(9)V99 COMP-3.  00015690
015700            15  SIZE-CURRENT-IRSOUNT       PIC S9(9)V99 COMP-3.   00015700
015710            15  SIZE-CURRENT-NET            PIC S9(9)V99 COMP-3.  00015710
015720     05  COST-DIV-TOTAL-SALES               PIC S9(9)V99 COMP-3.  00015720
015730     05  COST-DIV-TOTAL-PRCE1              PIC S9(9)V99 COMP-3.   00015730
015740     05  COST-UNRECONCILED-SUSP-ADJ         PIC S9(9)V99 COMP-3.  00015740
015750*                                                                 00015750
015760     05  PRCE-HOLD-AREAS                    PIC X(10) VALUE       00015760
015770             'HOLD AREAS'.                                        00015770
015780*                                                                 00015780
015790     05  COLR-HOLD-AREAS.                                         00015790
015800         10  COLR-KEYWORD                   PIC X(10)             00015800
015810                                    VALUE SPACES.                 00015810
015831         10  COLR-DESCRIPTION               PIC X(38)             00015831
015832                                    VALUE SPACES.                 00015832
015840         10  COLR-ENTRY-TYPE                PIC X(10)             00015840
015850                                    VALUE SPACES.                 00015850
015860         10  COLR-ENTRY-SOURCE              PIC X(10)             00015860
015870                                    VALUE SPACES.                 00015870
015880         10  COLR-DR-CR                     PIC X                 00015880
015890                                    VALUE SPACES.                 00015890
015892         10  COLR-HOLD-CURRENT-RETURNS      PIC S9(9)V99 COMP-3   00015892
015893                                    VALUE +0.                     00015893
015900         10  COLR-PT-DATE                   PIC 9(7) COMP-3       00015900
015910                                    VALUE ZERO.                   00015910
015920         10  COLR-ORIGINAL-DATE             PIC 9(7) COMP-3       00015920
015930                                    VALUE ZERO.                   00015930
015940         10  BUYR-CR-DAYS                   PIC 9(3)              00015940
015950                                    VALUE ZERO.                   00015950
015960         10  COLR-SPECIAL-DAYS              PIC X                 00015960
015970                                    VALUE 'N'.                    00015970
015980         10  COLR-DAYS                      PIC S9(3)             00015980
015990                                    VALUE +0.                     00015990
016000         10  BUYR-XPENS-HLD                PIC 9(3)               00016000
016010                                    VALUE ZERO.                   00016010
016020         10  COLR-MATURED-FE-DAYS           PIC 9(3)              00016020
016030                                    VALUE ZERO.                   00016030
016040         10  COLR-DBS.                                            00016040
016050            15  COLR-DBS-DATE              PIC 9(6).              00016050
016060            15  FILLER                     PIC X(2) VALUE SPACE.  00016060
016070            15  COLR-DBS-PRICE             PIC 9(3).              00016070
016080            15  FILLER                     PIC X(2) VALUE SPACE.  00016080
016090            15  COLR-DBS-SEQ               PIC 9(3).              00016090
016100            15  FILLER                     PIC X(2) VALUE SPACE.  00016100
016110            15  COLR-DBS-LABEL             PIC X(15) VALUE SPACE. 00016110
016120         10  COLR-WORK-AMOUNT               PIC S9(11)V99 COMP-3. 00016120
016130         10  COLR-WORK-DOLLAR               PIC S9(11)    COMP-3. 00016130
016140         10  COLR-REC-BAL                   PIC S9(11)V99 COMP-3. 00016140
016150         10  COLR-DETAIL-AMT                PIC S9(9)V99  COMP-3. 00016150
016160         10  COLR-LINE-AMT                  PIC S9(9)V99  COMP-3. 00016160
016170         10  COLR-PRCE1-WORK               PIC S9(11)V99 COMP-3.  00016170
016180         10  COLR-CLI-TR.                                         00016180
016190             15  COLR-CLI                   PIC X(3).             00016190
016200             15  COLR-TR                    PIC X(2).             00016200
016210         10  COLR-4XX-BAL                   PIC S9(9)V99 COMP-3   00016210
016220                                                      VALUE +0.   00016220
016230         10  COLR-TIME                      PIC 9(6).             00016230
016240         10  COLR-DAY-OF-WEEK               PIC 9.                00016240
016241         10  COLR-CURR-DAY-OF-WEEK          PIC 9.                00016241
016250         10  COLR-TODAY                     PIC 9.                00016250
016260         10  COLR-DAYS-CALC-REMAINDER       PIC 9(3) VALUE 0.     00016260
016270         10  COLR-DAYS-CALC-1               PIC 9(3) VALUE 0.     00016270
016280         10  COLR-DAYS-CALC-2               PIC 9(3) VALUE 0.     00016280
016290         10  COLR-DAYS-CALC-TOTAL           PIC 9(3) VALUE 0.     00016290
016300         10  COLR-FILM                      PIC 9(11).            00016300
016310         10  COLR-PREVIOUS-WREHOUSE         PIC 9(7).             00016310
016320         10  COLR-STATUS                    PIC X.                00016320
016330         10  COLR-MAX-DAYS                  PIC S9(5)             00016330
016340                                    VALUE +0.                     00016340
016350         10  COLR-CUST-MAX                  PIC S9(5)             00016350
016360                                    VALUE +0.                     00016360
016370         10  COLR-TOTAL-PAST-DUE-AMT        PIC S9(7)V99          00016370
016380                                    VALUE +0.                     00016380
016390         10  COLR-HIGH-VOLUME               PIC 9(7).             00016390
016400         10  FILLER REDEFINES COLR-HIGH-VOLUME.                   00016400
016410             15  HIGH-VOLUME                PIC 99.               00016410
016420             15  FILLER                     PIC 9(5).             00016420
016430         10  COLR-TC-CUST                   PIC 9(7).             00016430
016440         10  COLR-TC-SUPPLR                 PIC X(5).             00016440
016450         10  COLR-REAL-SUPPLR               PIC X(5).             00016450
016460         10  COLR-TC-STORE                  PIC 9(5).             00016460
016470         10  COLR-TC-EXPNSE                PIC 9(9).              00016470
016480*                                                                 00016480
016490     05  RDZ-COLL-REC-WRITTEN           PIC X(1)  VALUE 'N'.      00016490
016500*                                                                 00016500
016510     05  BUYR-CR-DEPOSIT                     PIC 9(7) VALUE 0.    00016510
016520     05  WS-BUYER-CREDIT                   PIC X(20).             00016520
016530     05  FILLER REDEFINES WS-BUYER-CREDIT.                        00016530
016540         10  ONE                             PIC X(1).            00016540
016550         10  FILLER                          PIC X(19).           00016550
016560     05  FILLER REDEFINES WS-BUYER-CREDIT.                        00016560
016570         10  TWO                             PIC X(2).            00016570
016580         10  FILLER                          PIC X(18).           00016580
016590     05  FILLER REDEFINES WS-BUYER-CREDIT.                        00016590
016600         10  THREE                           PIC X(3).            00016600
016610         10  FILLER                          PIC X(17).           00016610
016620     05  FILLER REDEFINES WS-BUYER-CREDIT.                        00016620
016630         10  FOUR                            PIC X(4).            00016630
016640         10  FILLER                          PIC X(16).           00016640
016650     05  FILLER REDEFINES WS-BUYER-CREDIT.                        00016650
016660         10  FIVE                            PIC X(5).            00016660
016670         10  FILLER                          PIC X(15).           00016670
016680     05  FILLER REDEFINES WS-BUYER-CREDIT.                        00016680
016690         10  SIX                             PIC X(6).            00016690
016700         10  FILLER                          PIC X(14).           00016700
016710     05  FILLER REDEFINES WS-BUYER-CREDIT.                        00016710
016720         10  SEVEN                           PIC X(7).            00016720
016730         10  FILLER                          PIC X(13).           00016730
016740     05  FILLER REDEFINES WS-BUYER-CREDIT.                        00016740
016750         10  EIGHT                           PIC X(8).            00016750
016760         10  FILLER                          PIC X(12).           00016760
016770     05  FILLER REDEFINES WS-BUYER-CREDIT.                        00016770
016780         10  NINE                            PIC X(9).            00016780
016790         10  FILLER                          PIC X(11).           00016790
016800     05  FILLER REDEFINES WS-BUYER-CREDIT.                        00016800
016810         10  TEN                             PIC X(10).           00016810
016820         10  FILLER                          PIC X(10).           00016820
016821     05  FILLER REDEFINES WS-BUYER-CREDIT.                        00016821
016822         10  FILLER                          PIC X(01).           00016822
016823         10  TWO-NINE                        PIC X(08).           00016823
016824         10  TEN-SEVENTEEN                   PIC X(08).           00016824
016825         10  FILLER                          PIC X(03).           00016825
016830*                                                                 00016830
016840                                                                  00016840
016850     05  COLR-PRICE2-WREHOUSE-NO.                                 00016850
016860         10  FILLER                          PIC  X(1) VALUE 'F'. 00016860
016870         10  COLR-WREHOUSE-NO                PIC  9(7).           00016870
016880         10  FILLER                          PIC  X(22) VALUE     00016880
016890                                                  SPACES.         00016890
016900*                                                                 00016900
016910     05 BUYR-NET-WORTH-AMOUNT                PIC 9(17) COMP-3.    00016910
016920     05 BUYR-NET-WORTH-CODE.                                      00016920
016930         10  BUYR-NET-WORTH-1                PIC  9(001).         00016930
016940         10  BUYR-NET-WORTH-3                PIC  9(003).         00016940
016950         10  BUYR-NET-WORTH-ALPHA            PIC  X.              00016950
016960*                                                                 00016960
016970     05  COLR-HOLD-AMT0-DATE                 PIC  9(007) VALUE    00016970
016980                                                  ZEROES.         00016980
016990     05  FILLER REDEFINES COLR-HOLD-AMT0-DATE.                    00016990
017000         10  COLR-HOLD-AMT0-C                PIC  9(001).         00017000
017010         10  COLR-HOLD-AMT0-MM               PIC  9(002).         00017010
017020         10  COLR-HOLD-AMT0-DD               PIC  9(002).         00017020
017030         10  COLR-HOLD-AMT0-YY               PIC  9(002).         00017030
017040     05  COLR-DATE-OF-DATA.                                       00017040
017050         10  COLR-DATE-OF-DATA-YY            PIC  9(002).         00017050
017060         10  COLR-DATE-OF-DATA-MM            PIC  9(002).         00017060
017070         10  COLR-DATE-OF-DATA-DD            PIC  9(002).         00017070
017080                                                                  00017080
017090     05  COLR-STATE.                                              00017090
017100         10  COLR-ALPHA-STATE                PIC  X(02).          00017100
017110         10  FILLER                          PIC  X(03).          00017110
017120                                                                  00017120
017130     05  NEW-WRHSE                     PIC  X(10).                00017130
017130     05  COLR-HOLD-PHONE                     PIC  X(10).          00017130
017140     05  FILLER REDEFINES COLR-HOLD-PHONE.                        00017140
017150         10  COLR-HOLD-PHONE-CITY            PIC  X(03).          00017150
017160         10  COLR-HOLD-PHONE-NO              PIC  X(07).          00017160
017171                                                                  00017171
017172     05  COLR-CHECK-PHONE                    PIC  X(07).          00017172
017173     05  FILLER REDEFINES COLR-CHECK-PHONE.                       00017173
017175         10  WS-CHK-PHONE-NUMBER             PIC  9(07).          00017175
017180*                                                                 00017180
017181     05  COLR-TXN-PHONE                      PIC  X(10).          00017181
017182     05  FILLER REDEFINES COLR-TXN-PHONE.                         00017182
017183         10  COLR-TXN-PHONE-CITY             PIC  9(03).          00017183
017184         10  COLR-TXN-PHONE-NUMBER           PIC  9(07).          00017184
017185*                                                                 00017185
017190     05  COLR-PRICE2-PHONE-NO.                                    00017190
017200         10  COLR-PHONE-CITY                 PIC  X(03).          00017200
017201         10  COLR-PHONE-NO                   PIC  X(07).          00017201
017210         10  FILLER                          PIC  X(02) VALUE     00017210
017220                                                  SPACES.         00017220
017230*                                                                 00017230
017240     05  COLR-DIV-TABLE-ARG.                                      00017240
017250         10  COLR-DIV-CLI-3                  PIC X(3).            00017250
017260         10  COLR-DIV-TRADE-STYLE            PIC X(2).            00017260
017270*                                                                 00017270
017280     05  COLR-SALES-STORUNIT                 PIC 9(3).            00017280
017290*                                                                 00017290
017300     05  COLR-SUPPLR.                                             00017300
017310         10  COLR-SUPPLR-3                   PIC X(3).            00017310
017320         10  COLR-TRADE-STYLE                PIC 9(2).            00017320
017330*                                                                 00017330
017340     05  COLR-CUST-SUPPLR.                                        00017340
017350         10  COLR-CUST-SUPPLR-3              PIC X(3).            00017350
017360         10  FILLER                          PIC X(2).            00017360
017370*                                                                 00017370
017380     05  COLR-SAVE-TRAN-CD                  PIC 9(3) VALUE ZEROS. 00017380
017390     05  SAVE-T-SUBSC                   PIC 9(4) COMP VALUE ZEROS.00017390
017400     05  SAVE-F-SUBSC                   PIC 9(4) COMP VALUE ZEROS.00017400
017410     05  SAVE-AVG-SUBSC                 PIC 9(4) COMP VALUE ZEROS.00017410
017420     05  COLR-SAVE-SUBSC                PIC 9(4) COMP VALUE 0.    00017420
017430*                                                                 00017430
017440     05  COLR-WORK-DATE                     PIC 9(7) VALUE ZEROS. 00017440
017450     05  FILLER REDEFINES COLR-WORK-DATE.                         00017450
017460         10  COLR-DATE-CENTURY              PIC 9.                00017460
017470         10  COLR-DATE-YEAR                 PIC 99.               00017470
017480         10  COLR-DATE-MONTH                PIC 99.               00017480
017490         10  COLR-DATE-DAY                  PIC 99.               00017490
017500*                                                                 00017500
017510     05  COLR-WORK-DOCUM1                   PIC 9(7) VALUE ZEROS. 00017510
017520     05  FILLER REDEFINES COLR-WORK-DOCUM1.                       00017520
017530         10  COLR-DOCUM1-CENTURY            PIC 9.                00017530
017540         10  COLR-DOCUM1-YEAR               PIC 99.               00017540
017550         10  COLR-DOCUM1-MONTH              PIC 99.               00017550
017560         10  COLR-DOCUM1-DAY                PIC 99.               00017560
017570     05  COLR-TERM-MMYY                     PIC 9(4).             00017570
017580     05  FILLER REDEFINES COLR-TERM-MMYY.                         00017580
017590         10  COLR-TERM-MONTH                PIC 99.               00017590
017600         10  COLR-TERM-YEAR                 PIC 99.               00017600
017610     05  COLR-TERM-YYMM                     PIC 9(4).             00017610
017620     05  FILLER REDEFINES COLR-TERM-YYMM.                         00017620
017630         10  COLR-TERM-YY                   PIC 99.               00017630
017640         10  COLR-TERM-MM                   PIC 99.               00017640
017650     05  COLR-TWO-AGO                       PIC 9(4) VALUE ZEROS. 00017650
017660     05  FILLER REDEFINES COLR-TWO-AGO.                           00017660
017670         10  COLR-TWO-YEAR                  PIC 99.               00017670
017680         10  COLR-TWO-MONTH                 PIC 99.               00017680
017690     05  COLR-SIX-AGO                       PIC 9(4) VALUE ZEROS. 00017690
017700     05  FILLER REDEFINES COLR-SIX-AGO.                           00017700
017710         10  COLR-SIX-YEAR                  PIC 99.               00017710
017720         10  COLR-SIX-MONTH                 PIC 99.               00017720
017730     05  COLR-CYYMMDD-AGO                   PIC 9(7) VALUE ZEROS. 00017730
017740     05  FILLER REDEFINES COLR-CYYMMDD-AGO.                       00017740
017750         10  FILLER                         PIC 9.                00017750
017760         10  COLR-AGO-YEAR                  PIC 99.               00017760
017770         10  COLR-AGO-MONTH                 PIC 99.               00017770
017780         10  COLR-AGO-DAY                   PIC 99.               00017780
017790*                                                                 00017790
017800     05  COLR-DAYS-DIFFERENCE               PIC S9(5) COMP-3.     00017800
017810     05  COLR-CYYMMDD-CURRENT               PIC 9(7) COMP-3.      00017810
017820     05  COLR-CURRENT-YYMMDD.                                     00017820
017830         10  COLR-CURRENT-YY                PIC 99.               00017830
017840         10  COLR-CURRENT-MM                PIC 99.               00017840
017850         10  COLR-CURRENT-DD                PIC 99.               00017850
017860*                                                                 00017860
017870     05  COLR-CYYJJJ-CURRENT               PIC 9(7) COMP-3.       00017870
017880*                                                                 00017880
017890     05  COLR-CYYMMDD-PLUS30                PIC 9(7) COMP-3.      00017890
017900*                                                                 00017900
017910     05  COLR-REJECT-CODE                   PIC X(2) VALUE '  '.  00017910
017920*                                                                 00017920
017930     05  COLR-ABEND-CODE                    PIC 99 VALUE 0.       00017930
017940*                                                                 00017940
017950     05  BUYR-CR-PERCENT                    PIC 9V9(2) COMP-3.    00017950
017960*                                                                 00017960
017970     05  IBM3-HOLD-AREAS.                                         00017970
017980         10  IBM3-LAST-MMDDYY               PIC 9(6).             00017980
017990         10  FILLER REDEFINES IBM3-LAST-MMDDYY.                   00017990
018000             15  IBM3-LAST-MONTH            PIC 99.               00018000
018010             15  IBM3-LAST-DAY              PIC 99.               00018010
018020             15  IBM3-LAST-YEAR             PIC 99.               00018020
018030         10  IBM3-FINAL-DT                  PIC 9(6).             00018030
018040         10  FILLER REDEFINES IBM3-FINAL-DT.                      00018040
018050             15  IBM3-DATE-MONTH            PIC 99.               00018050
018060             15  IBM3-DATE-DAY              PIC 99.               00018060
018070             15  IBM3-DATE-YEAR             PIC 99.               00018070
018080         10  IBM3-WORK-MMDDYY               PIC 9(6).             00018080
018090         10  FILLER REDEFINES IBM3-WORK-MMDDYY.                   00018090
018100             15  IBM3-W-MONTH            PIC 99.                  00018100
018110             15  IBM3-W-DAY              PIC 99.                  00018110
018120             15  IBM3-W-YEAR             PIC 99.                  00018120
018130         10  IBM3-MMDDYY                    PIC 9(6).             00018130
018140         10  FILLER REDEFINES IBM3-MMDDYY.                        00018140
018150             15  IBM3-C-MONTH            PIC 99.                  00018150
018160             15  IBM3-C-DAY              PIC 99.                  00018160
018170             15  IBM3-C-YEAR             PIC 99.                  00018170
018180         10  IBM3-WORK-YYMMDD               PIC 9(6).             00018180
018190         10  FILLER REDEFINES IBM3-WORK-YYMMDD.                   00018190
018200             15  IBM3-WORK-YEAR          PIC 99.                  00018200
018210             15  IBM3-WORK-MONTH         PIC 99.                  00018210
018220             15  IBM3-WORK-DAY           PIC 99.                  00018220
018230         10  IBM3-STATEMENT-YYMMDD          PIC 9(6).             00018230
018240         10  FILLER REDEFINES IBM3-STATEMENT-YYMMDD.              00018240
018250             15  IBM3-STMT-YEAR          PIC 99.                  00018250
018260             15  IBM3-STMT-MONTH         PIC 99.                  00018260
018270             15  IBM3-STMT-DAY           PIC 99.                  00018270
018280         10  IBM3-DAYS                      PIC 9(3) COMP-3.      00018280
018290         10  COLR-HDGS-DAY                 PIC 99.                00018290
018300         10  COLR-HDGS-YEAR                PIC 9999.              00018300
018310         10  COLR-RETURN-CODE              PIC 9.                 00018310
018320*                                                                 00018320
018330        10  IBM3-CUST-LIMIT                 PIC 9(9) VALUE 0.     00018330
018340        10  IBM3-CUSTNUM                    PIC 9(7).             00018340
018350        10  FILLER REDEFINES IBM3-CUSTNUM.                        00018350
018360            15  CUST-THREE                  PIC 9(3).             00018360
018370            15  CUST-FOUR                   PIC 9(4).             00018370
018380        10  IBM3-CUST-NO.                                         00018380
018390            15  FIRST-THREE                 PIC 9(3).             00018390
018400            15  FILLER                      PIC X VALUE '-'.      00018400
018410            15  LAST-FOUR                   PIC 9(4).             00018410
018420        10  IBM3-PRINT-DATE.                                      00018420
018430            15  PRINT-MO                    PIC 9(2).             00018430
018440            15  FILLER                      PIC X VALUE '-'.      00018440
018450            15  PRINT-DAY                   PIC 9(2).             00018450
018460            15  FILLER                      PIC X VALUE '-'.      00018460
018470            15  PRINT-YR                    PIC 9(2).             00018470
018480            15  FILLER                      PIC X VALUE '-'.      00018480
018490        10  IBM3-PRINT-DATE2.                                     00018490
018500            15  PRINT-MO2                   PIC 9(2).             00018500
018510            15  PRINT-DAY2                  PIC 9(2).             00018510
018520                                                                  00018520
018530     05  IBM3-TRAN-CODE-RDZ                PIC 9(03) VALUE 0.     00018530
018540     05  IBM3-WREHOUSE-NUMBER-RDZ          PIC 9(07) VALUE 0.     00018540
018550                                                                  00018550
018560     05  RDZ1-WREHOUSE-STATE-CODE           PIC 9(02) VALUE 0.    00018560
018570     05  RDZ1-UNIT-CUST-NO                 PIC 9(7)  COMP-3       00018570
018580                                              VALUE 0.            00018580
018590     05  RDZ1-WORK-CCYYMMDD                 PIC 9(8).             00018590
018600     05  FILLER REDEFINES   RDZ1-WORK-CCYYMMDD.                   00018600
018610          10 RDZ1-W-CENTURY       PIC 99.                         00018610
018620          10 RDZ1-W-YEAR          PIC 99.                         00018620
018630          10 RDZ1-W-MONTH         PIC 99.                         00018630
018640          10 RDZ1-W-DAY           PIC 99.                         00018640
018641     05  WH05-DATE-CURR                     PIC 9(7).             00018641
018642     05  FILLER REDEFINES   WH05-DATE-CURR.                       00018642
018643          10 WH05-CENTURY         PIC 9.                          00018643
018644          10 WH05-YEAR            PIC 99.                         00018644
018645          10 WH05-MONTH           PIC 99.                         00018645
018646          10 WH05-DAY             PIC 99.                         00018646
018650     05  WH05-DATE                          PIC 9(7).             00018650
018660     05  FILLER REDEFINES   WH05-DATE.                            00018660
018670          10 WH05-C               PIC 9.                          00018670
018680          10 WH05-MM              PIC 99.                         00018680
018690          10 WH05-DD              PIC 99.                         00018690
018700          10 WH05-YY              PIC 99.                         00018700
018710     05  WH20-SUPPLR-NUMBER                 PIC X(05).            00018710
018720     05  FILLER                             REDEFINES             00018720
018730         WH20-SUPPLR-NUMBER.                                      00018730
018740         10  WH20-SUPPLR3                   PIC X(03).            00018740
018750         10  FILLER                         PIC X(02).            00018750
018760*                                                                 00018760
018770     05  WI00-SUBSCES.                                            00018770
018780         10  FILLER                          PIC X(08) VALUE      00018780
018790             'INDEXES '.                                          00018790
018800         10  WI01-XCOUNT                     PIC S9(3) COMP.      00018800
018810         10  WI01-IND                        PIC S9(3) COMP.      00018810
018820*                                                                 00018820
018830*                                                                 00018830
018840     05  WK00-MESSAGES.                                           00018840
018850         10  FILLER                          PIC X(08) VALUE      00018850
018860             'MESSAGES'.                                          00018860
018870         10  WK80-COST-ERROR.                                     00018870
018880             15  FILLER                      PIC X(35) VALUE      00018880
018890              'INVALID KEY ON COST-CTLFILE REWRITE'.              00018890
018900*                                                                 00018900
018910*                                                                 00018910
018920     05  COST-88-LEVELS.                                          00018920
018930         10  FILLER                          PIC X(09) VALUE      00018930
018940             '88 LEVELS'.                                         00018940
018950         10  COST-FIELD                      PIC 999.             00018950
018960* FIELD                                                           00018960
018970             88  CUST-FIELD            VALUE  100 THRU 120, 122,  00018970
018980                         123, 125, 126, 127, 129, 130, 131, 132,  00018980
018981                         133, 134.                                00018981
018990* FIELD                                                           00018990
019000             88  CUST-SUPPLR-FIELD     VALUE  200, 204 THRU 207,  00019000
019010                                209, 210, 001, 002.               00019010
019020*                                                                 00019020
019030         10  COST-KEYWORD                    PIC X(10).           00019030
019040             88  KEYWORD   VALUE 'LOCATION', 'X876 INC',          00019040
019050                      'COLR SUSP', 'DORMANT CR', 'COLL EXP',      00019050
019060                      'LIABILITY', 'ADVANCES', 'ACCTS REC',       00019060
019070                      'RECOVY TIP', 'LOSS TIP'.                   00019070
019080*                                                                 00019080
019090         10  COST-RECORD-CODE                PIC 9.               00019090
019100             88  HEADER    VALUE 1.                               00019100
019110             88  SUPPLR    VALUE 2.                               00019110
019120             88  WREHOUSE  VALUE 3.                               00019120
019130         10  COST-ORIG-TRAN-CODE             PIC 9(3).            00019130
019140             88  ORIG-MATERIAL-ENTRY-PLUS VALUE 100 THRU 199,     00019140
019150                                 333, 411, 811.                   00019150
019160             88  ORIG-MATERIAL-ENTRY-MINUS VALUE 200 THRU 299,    00019160
019170                                 400 THRU 499.                    00019170
019180             88  ORIG-WREHOUSE-INCOME     VALUE 551, 561.         00019180
019190                                                                  00019190
019200         10  COST-TRAN-CODE                  PIC 9(3).            00019200
019210* RDZ25                                                           00019210
019220             88  ADVANCE            VALUE  11, 13, 14, 15,        00019220
019230                                           21, 31, 03, 04,        00019230
019240                                           32, 33, 34,            00019240
019250                                           41, 51, 55,            00019250
019260                                           081, 05.               00019260
019270* RDZ26                                                           00019270
019280             88  COLR               VALUE  343, 361, 441, 571,    00019280
019290     122, 222, 711, 715, 721, 725, 741, 745, 751, 755, 761, 765.  00019290
019300* MFC                                                             00019300
019310             88  WREHOUSE-DELETE    VALUE  010.                   00019310
019320* MFC                                                             00019320
019330             88  NEW-WREHOUSE       VALUE  011 THRU 013.          00019330
019340* MFC                                                             00019340
019350             88  WREHOUSE-NA        VALUE  021 THRU 022.          00019350
019360* MFC                                                             00019360
019370             88  WREHOUSE-RDZ       VALUE  023.                   00019370
019380* MFC                                                             00019380
019390             88  WREHOUSE-SUPPLR-DELETE  VALUE 030.               00019390
019400* MFC                                                             00019400
019410             88  WREHOUSE-SUPPLR-RDZ   VALUE  035, 036, 038.      00019410
019420* OPEN                                                            00019420
019430             88  LOOKUP    VALUE  120, 121, 130, 131, 160, 161,   00019430
019440             170, 171, 180, 220, 221, 281, 333, 411, 411, 419,    00019440
019450             421, 429, 431, 439, 551, 561, 811.                   00019450
019460* OPEN                                                            00019460
019470             88  EXPNSE-REVERSAL       VALUE 191, 291.            00019470
019480* OPEN                                                            00019480
019490             88  EXPNSE    VALUE  120, 121, 130, 131, 160, 161,   00019490
019500             170, 171, 180, 220, 221, 281.                        00019500
019510* OPEN                                                            00019510
019520             88  INSRN-EMPLOYED   VALUE 122, 222.                 00019520
019530*                                                                 00019530
019540             88  RETURN-CHECK     VALUE 811.                      00019540
019550*                                                                 00019550
019560             88  CHANGE-TERMS-CLOSED    VALUE 885.                00019560
019570*                                                                 00019570
019580             88  INTEREST-IRSOUNT-BILLS VALUE 551, 561.           00019580
019590*                                                                 00019590
019600             88  DEDUCTIONS       VALUE 333, 411.                 00019600
019610*                                                                 00019610
019620             88  SHIPING-ON-ACCT  VALUE 411, 419, 421, 429,       00019620
019630                                  431, 439.                       00019630
019640*                                                                 00019640
019650             88  COLR-DIFFERENCE  VALUE 731, 735.                 00019650
019660* SHIPPING                                                        00019660
019670             88  PAID-CHECK       VALUE 311.                      00019670
019680*                                                                 00019680
019690             88  ECB              VALUE 361.                      00019690
019700*                                                                 00019700
019710             88  DELETE-TRAN-CODES  VALUE 321, 825, 831, 835,     00019710
019720                         841, 845, 851, 861, 871, 875.            00019720
019730*                                                                 00019730
019740             88  PAID-BY-COLR     VALUE 321.                      00019740
019750*                                                                 00019750
019760             88  WAIVE-IRSOUNT         VALUE 851.                 00019760
019770*                                                                 00019770
019780             88  WAIVE-INTEREST         VALUE 861.                00019780
019790*                                                                 00019790
019800             88  CHARGE-BACK            VALUE  831, 835.          00019800
019810*                                                                 00019810
019820             88  VOUCHER                VALUE  825.               00019820
019830*                                                                 00019830
019840             88  CHARGE-OFF             VALUE 841, 845.           00019840
019850*                                                                 00019850
019860             88  OFFSET                 VALUE 871, 875.           00019860
019870*                                                                 00019870
019880             88  MATERIAL-ENTRY  VALUE  825, 831, 835,            00019880
019890                         841, 845, 851, 861, 871, 875.            00019890
019900* MISC                                                            00019900
019910             88  CHANGE-TRAN-CODE       VALUE 891.                00019910
019920* PRCHS-ORD                                                       00019920
019930             88   PRCE1           VALUE 979, 981, 982, 985, 991.  00019930
019940* FE                                                              00019940
019950             88  INSRN-EMPLOYED-NEW     VALUE 121, 131, 161,      00019950
019960                                171, 180, 221, 281.               00019960
019970* FE                                                              00019970
019980             88  INSRN-EMPLOYED-MAT     VALUE 120, 121, 170,      00019980
019990                                171, 180, 220, 221, 281.          00019990
020000* FE                                                              00020000
020010             88  INSRN-EMPLOYED-COA     VALUE 411, 419, 431,      00020010
020020                                439.                              00020020
020030* RELOCATION-1                                                    00020030
020040             88  ADD-SALES        VALUE 120, 130, 160, 170.       00020040
020050*                                                                 00020050
020060             88  WRHOUS-MOVE      VALUE 220.                      00020060
020070*                                                                 00020070
020080             88  SUB-REINSTATED   VALUE 221, 281.                 00020080
020090*                                                                 00020090
020100             88  ADD-REINSTATED   VALUE 121, 131, 161, 171, 180.  00020100
020110*RELOCATION-1 STORUNIT CONTROL TOTALS.                            00020110
020120             88  CONTROL-TOTAL-FIELD VALUE 311, 761, 765, 711,    00020120
020130                 721, 731, 571, 715, 725, 735,                    00020130
020140                 361, 751, 755, 741, 745, 441, 343, 333, 411,     00020140
020150                 411, 419, 421, 429, 431, 439, 811.               00020150
020160             88  CONTROL-TOTAL-DTC VALUE 321, 825, 841, 871, 835, 00020160
020170                 845, 875, 831.                                   00020170
020180             88  WREHOUSE-INCOME-MINUS VALUE 711, 721, 731, 571.  00020180
020190             88  WREHOUSE-INCOME-PLUS VALUE 715, 725, 735, 321.   00020190
020200             88  WREHOUSE-INCOME-DTC  VALUE 321.                  00020200
020210             88  TOTAL-PRICE0-PROCESSED VALUE 311.                00020210
020220             88  KNOWN-LOCATION-ADJUSTMENT VALUE 761, 765.        00020220
020230             88  DORMANT-RETURNS-USED VALUE 361.                  00020230
020240             88  BAD-COLR-RECOVERIES  VALUE 751, 755.             00020240
020250             88  CATALOG001-COST  VALUE 741, 745.                 00020250
020260             88  SUPPLR-RETURNS-CC   VALUE 441.                   00020260
020270             88  SUPPLR-SALESS    VALUE 343.                      00020270
020280             88  RETURN-PRICE0-POSTED VALUE 811.                  00020280
020290             88  VOUCHERS             VALUE 825.                  00020290
020300             88  SUPPLR-EXPNSE       VALUE 831.                   00020300
020310             88  SUPPLR-RETURNS-JE    VALUE 835.                  00020310
020320             88  BAD-COLR-CHARGE-OFFS VALUE 841, 845.             00020320
020330             88  MISC-OFFSETS         VALUE 871, 875.             00020330
020340             88  REFUND-TO-SUPPLR       VALUE 711, 721, 731, 571, 00020340
020350                                              715, 725, 735, 321. 00020350
020360             88  ACCOUNTS-RECEIVABLE-ADJ VALUE 831, 343, 835, 441.00020360
020370             88  ACCOUNTS-RECEIVABLE-ADJ-PLUS VALUE 831, 343.     00020370
020380             88  ACCOUNTS-RECEIVABLE-ADJ-MINUS VALUE 835, 441.    00020380
020390             88  LOOKUP-PLUS VALUE 411, 419, 421, 429, 431, 439.  00020390
020400             88  LOOKUP-MINUS VALUE 333, 411.                     00020400
020410         10  COST-ALPHA-FILE-CODE            PIC X.               00020410
020420             88  VALID-ALPHA-FILE-CODE VALUES ARE 'A' 'B' 'C',    00020420
020430                                                  'D' 'E' 'F',    00020430
020440                                                  'G' 'H' 'I'.    00020440
020450     05  WS00-SWITCHES.                                           00020450
020460         10  FILLER                          PIC X(08) VALUE      00020460
020470             'SWITCHES'.                                          00020470
020480         10  WS01-EOF-SUPPLR                 PIC X(01) VALUE 'N'. 00020480
020490             88  END-OF-SUPPLR-FILE                    VALUE 'Y'. 00020490
020500         10  WS01-EOF-MASTER                 PIC X(01) VALUE 'N'. 00020500
020510             88  END-OF-MASTER-FILE                    VALUE 'Y'. 00020510
020520         10  WS01-EOF-TRANS                  PIC X(01) VALUE 'N'. 00020520
020530             88  END-OF-TRANS-FILE                     VALUE 'Y'. 00020530
020540         10  WS01-EOF-DETAIL                 PIC X(01) VALUE 'N'. 00020540
020550             88  END-OF-DETAIL-FILE                    VALUE 'Y'. 00020550
020560         10  WS01-GOT-TRAN                   PIC X(01) VALUE 'N'. 00020560
020570             88  GOT-TRAN                              VALUE 'Y'. 00020570
020580         10  WS01-GOT-DETAIL                 PIC X(01) VALUE 'N'. 00020580
020590             88  GOT-DETAIL                            VALUE 'Y'. 00020590
020600         10  WS01-GOT-MASTER                 PIC X(01) VALUE 'N'. 00020600
020610             88  GOT-MASTER                            VALUE 'Y'. 00020610
020620         10  WS01-NEW-LIST                   PIC X(01) VALUE 'N'. 00020620
020630             88  LIST-TODAY                            VALUE 'Y'. 00020630
020640         10  WS01-PREVIOUS-RECORD            PIC 9(01) VALUE 0.   00020640
020650             88  LAST-WAS-HEADER                       VALUE 1.   00020650
020660         10  WS01-FE-FLAG                    PIC 9(01) VALUE 0.   00020660
020670             88  FE-NEW-TODAY                          VALUE 1.   00020670
020680             88  FE-MATURED                            VALUE 2.   00020680
020690             88  FE-891                                VALUE 3.   00020690
020700             88  FE-4XX                                VALUE 4.   00020700
020710         10  WS01-NEW-TODAY                  PIC X(01) VALUE 'N'. 00020710
020720         10  WS01-NEXT-WREHOUSE              PIC X(01) VALUE 'N'. 00020720
020730         10  WS01-WREHOUSE-TOTAL             PIC X(01) VALUE 'N'. 00020730
020740         10  WS01-WAREHOUS-SW                PIC X(01) VALUE 'N'. 00020740
020750         10  WS01-SUPPLR-TOTAL               PIC X(01) VALUE 'N'. 00020750
020760         10  WS01-INVALID-SWITCH             PIC X(01) VALUE 'N'. 00020760
020770             88  INVALID-KEY                           VALUE 'Y'. 00020770
020780         10  WS01-FE-FOUND                   PIC X(01) VALUE 'N'. 00020780
020790         10  WS01-SUPPLR-XFR                 PIC X(01) VALUE 'N'. 00020790
020800         10  WS01-ACCORD-MATCH               PIC X(01) VALUE 'N'. 00020800
020810             88  NO-MATCH                              VALUE 'N'. 00020810
020820         10  WS01-NON-NOTIF-MATCH            PIC X(01) VALUE 'N'. 00020820
020830             88  NO-NON-NOTIF-MATCH                    VALUE 'N'. 00020830
020840         10  WS01-ALL-ACCORD-DETAILS-FND     PIC X(01) VALUE 'Y'. 00020840
020850             88  ALL-ACCORD-DETAILS-FND                VALUE 'Y'. 00020850
020860         10  WS01-ALL-NON-NOTIF-DTLS-FND     PIC X(01) VALUE 'Y'. 00020860
020870             88  ALL-NON-NOTIF-DTLS-FND                VALUE 'Y'. 00020870
020880         10  WS01-WRITE-BIAS-RECORD          PIC X(01) VALUE 'N'. 00020880
020890             88  WRITE-BIAS-RECORD                     VALUE 'Y'. 00020890
020900         10  WS01-BIAS-ADDRESS-CHANGE        PIC X(01) VALUE 'N'. 00020900
020910             88  BIAS-ADDRESS-CHANGE                   VALUE 'Y'. 00020910
020920         10  WS01-BIAS-DNB-CHANGE            PIC X(01) VALUE 'N'. 00020920
020930             88  BIAS-DNB-CHANGE                       VALUE 'Y'. 00020930
020940         10  WS01-BIAS-ATB-CHANGE            PIC X(01) VALUE 'N'. 00020940
020950             88  BIAS-ATB-CHANGE                       VALUE 'Y'. 00020950
020960*                                                                 00020960
020970 01  WT-PT-TABLE.                                                 00020970
020980     05  WT01-OVRHEDS-TABLE OCCURS 800 TIMES INDEXED BY           00020980
020990                ACCT-SUBSC.                                       00020990
021000         10   GL-STORUNIT                     PIC 999.            00021000
021010         10   GL-BANK                         PIC 9(04).          00021010
021020         10   GL-ACCOUNT                      PIC 9(07).          00021020
021030         10   GL-CENTER                       PIC 9(07).          00021030
021040         10   GL-KEYWORD                      PIC X(10).          00021040
021050         10   GL-TYPE                         PIC X(04).          00021050
021060*                                                                 00021060
021070     05  WT-TABLES.                                               00021070
021080         10  FILLER                          PIC X(06) VALUE      00021080
021090             'TABLES'.                                            00021090
021100         10  WT01-TERMS-TABLE OCCURS 1000 TIMES INDEXED BY        00021100
021110                TERM-SUBSC.                                       00021110
021120            15  TERMS-CODE                      PIC 999.          00021120
021130            15  TERMS-EXPANDED                  PIC X(36).        00021130
021140*                                                                 00021140
021150         10  WT01-STORUNIT-TABLE.                                 00021150
021160            15  WT01-DIV-TABLE-ENTRY OCCURS 4000 TIMES            00021160
021170                ASCENDING KEY IS DIV-SUPPLR                       00021170
021180                INDEXED BY DIV-SUBSC.                             00021180
021190                20  DIV-SUPPLR                      PIC X(5).     00021190
021200                20  DIV-STORUNIT                    PIC 9(3).     00021200
021210*                                                                 00021210
021220 01  WT-TABLES-WORK-AREAS.                                        00021220
021230         10  FILLER                          PIC X(26) VALUE      00021230
021240             'TABLE AREAS AND WORK AREAS'.                        00021240
021250         10  WT01-INSRN-EMPLOYED OCCURS 3000 TIMES INDEXED BY     00021250
021260                       FE-SUBSC.                                  00021260
021270             15  FE-SUPPLR-NO               PIC X(3).             00021270
021280             15  FE-CONTRACT-CODE           PIC 9(1).             00021280
021290             15  FE-DAYS                    PIC 9(3).             00021290
021300             15  FE-CAL-CODE                PIC X(1).             00021300
021310             15  FE-AGE-DAYS                PIC 9(3).             00021310
021320             15  FE-COA-CODE                PIC X.                00021320
021330             15  FE-PERCENT                 PIC 9(3) COMP-3.      00021330
021340             15  FE-TRANSFER-CODE           PIC X.                00021340
021350             15  FE-TRANSFER-SUPPLR         PIC X(5).             00021350
021360*                                                                 00021360
021370         10  WT01-AVERAGE-DUE OCCURS 2000 TIMES INDEXED BY        00021370
021380                      AVG-SUBSC.                                  00021380
021390             15  AVG-SUPPLR-NO              PIC X(3).             00021390
021400             15  AVG-CONTRACT-CODE          PIC 9(1).             00021400
021410             15  AVG-DAYS                   PIC 9(3).             00021410
021420             15  AVG-CAL-CODE               PIC X(1).             00021420
021430             15  AVG-AGE-DAYS               PIC 9(3).             00021430
021440             15  AVG-COA-CODE               PIC X.                00021440
021450             15  AVG-PERCENT                PIC 9(3) COMP-3.      00021450
021460             15  AVG-TRANSFER-CODE          PIC X.                00021460
021470             15  AVG-TRANSFER-SUPPLR        PIC X(5).             00021470
021480*                                                                 00021480
021490         10  WT01-TERMINATING OCCURS 999 TIMES INDEXED BY         00021490
021500                  TERMINATING-SUBSC.                              00021500
021510             15  TERM-SUPPLR-NO             PIC X(3).             00021510
021520             15  TERM-CODE                  PIC 9(1).             00021520
021530*                                                                 00021530
021540         10  WT01-CANADIAN-ACCORD OCCURS 4000 TIMES               00021540
021550                                  INDEXED BY ACCORD-SUBSC.        00021550
021560             15  WT01-ACCORD-SUPPLR-NO        PIC X(03).          00021560
021570             15  WT01-ACCORD-INDICATOR        PIC X(01).          00021570
021580*                                                                 00021580
021590 01  WT-TABLES-WORK-AREAS2.                                       00021590
021600         10  FILLER                          PIC X(31) VALUE      00021600
021610             'MORE TABLE AREAS AND WORK AREAS'.                   00021610
021620*                                                                 00021620
021630         10  WT01-NON-NOTIF-ELEM  OCCURS 4000 TIMES               00021630
021640                                  INDEXED BY NON-NOTIF-SUBSC.     00021640
021650             15  WT01-NON-NOTIF-SUPPLR        PIC X(03).          00021650
021660             15  WT01-NON-NOTIF-CR            PIC X(01).          00021660
021670             15  WT01-NON-NOTIF-AR            PIC X(01).          00021670
021680*                                                                 00021680
021690         10  WT01-ABEND-TABLE.                                    00021690
021700             15  FILLER                     PIC X(30)             00021700
021710             VALUE 'HOUSEKEEPING DATE ERROR'.                     00021710
021720             15  FILLER                     PIC X(30)             00021720
021730             VALUE 'COST CTLFILE READ ERROR'.                     00021730
021740             15  FILLER                     PIC X(30)             00021740
021750             VALUE 'INPUT FILES IN ERROR'.                        00021750
021760             15  FILLER                     PIC X(30)             00021760
021770             VALUE '180 BILL-LADING DATE INVALID'.                00021770
021780             15  FILLER                     PIC X(30)             00021780
021790             VALUE 'EXPNSE TRAN DATE INVALID'.                    00021790
021800             15  FILLER                     PIC X(30)             00021800
021810             VALUE 'INVALID BILL-LADING CODE'.                    00021810
021820             15  FILLER                     PIC X(30)             00021820
021830             VALUE ' PRCE1 TRAN DATE INVALID'.                    00021830
021840             15  FILLER                     PIC X(30)             00021840
021850             VALUE 'BILL-LADING DATE INVALID'.                    00021850
021860             15  FILLER                     PIC X(30)             00021860
021870             VALUE 'CALCULATION OF TERMS INVALID'.                00021870
021880             15  FILLER                     PIC X(30)             00021880
021890             VALUE 'INVALID WREHOUSE CODE'.                       00021890
021900             15  FILLER                     PIC X(30)             00021900
021910             VALUE 'INVALID DETAIL CODE'.                         00021910
021920             15  FILLER                     PIC X(30)             00021920
021930             VALUE 'SUPPLR RECORD OUT OF BALANCE'.                00021930
021940             15  FILLER                     PIC X(30)             00021940
021950             VALUE 'INVALID FINAL DUE DATE'.                      00021950
021960             15  FILLER                     PIC X(30)             00021960
021970             VALUE 'TERMS CODE INVALID'.                          00021970
021980             15  FILLER                     PIC X(30)             00021980
021990             VALUE 'MASTERS OUT OF SYN AT EOJ'.                   00021990
022000             15  FILLER                     PIC X(30)             00022000
022010             VALUE 'STORUNIT TABLE OVERFLOW  '.                   00022010
022020             15  FILLER                     PIC X(30)             00022020
022030             VALUE 'CALCULATION OF DAYSINMO ERR'.                 00022030
022040             15  FILLER                     PIC X(30)             00022040
022050             VALUE '11,000 REJECTS BAD FCACTRAS'.                 00022050
022060             15  FILLER                     PIC X(30)             00022060
022070             VALUE 'INSRN EMPLD TABLE OVERFLOW '.                 00022070
022080             15  FILLER                     PIC X(30)             00022080
022090             VALUE 'AVG DUE DATE TABLE OVERFLOW'.                 00022090
022100             15  FILLER                     PIC X(30)             00022100
022110             VALUE 'ACCORD TABLE OVERFLOW      '.                 00022110
022120             15  FILLER                     PIC X(30)             00022120
022130             VALUE 'NON NOTIF TABLE OVERFLOW   '.                 00022130
022140         10  WT01-MESSAGE REDEFINES WT01-ABEND-TABLE PIC X(30)    00022140
022150              OCCURS 22 TIMES.                                    00022150
022160*                                                                 00022160
022170         10 WT02-LADING-TABLE OCCURS 3000 TIMES                   00022170
022180                            INDEXED BY LADING-SUBSC.              00022180
022190             15  WT02-SUPPLR-NO.                                  00022190
022200                 20  WT02-SUPPLR            PIC X(3).             00022200
022210                 20  WT02-TRADE             PIC X(2).             00022210
022220             15  WT02-LADING-ACCT       PIC 9(7) COMP-3.          00022220
022230******************************************************************00022230
022240*              PRIVATE LABEL PAST DUE RANGE TABLE                *00022240
022250******************************************************************00022250
022260     05  WT03-RDZ-PD-RANGE-TABLE.                                 00022260
022270         10  WT03-RDZ-PD-RANGE-TBL       OCCURS 100 TIMES.        00022270
022280             15  WT03-RDZ-SUPPLR-CODE         PIC X(05).          00022280
022290             15  FILLER                      PIC X(1).            00022290
022300             15  WT03-RDZ-INDUSTRY-CODE       PIC S9(03).         00022300
022310             15  FILLER                      PIC X(1).            00022310
022320             15  WT03-RDZ-PAST-DUE-AMT-1-6    PIC S9(07)V99.      00022320
022330             15  FILLER                      PIC X(1).            00022330
022340             15  WT03-RDZ-PAST-DUE-AMT-7-14   PIC S9(07)V99.      00022340
022350             15  FILLER                      PIC X(1).            00022350
022360             15  WT03-RDZ-PAST-DUE-AMT-15-19  PIC S9(07)V99.      00022360
022370             15  FILLER                      PIC X(1).            00022370
022380             15  WT03-RDZ-PAST-DUE-AMT-20-24  PIC S9(07)V99.      00022380
022390             15  FILLER                      PIC X(1).            00022390
022400             15  WT03-RDZ-PAST-DUE-AMT-25-29  PIC S9(07)V99.      00022400
022410             15  FILLER                      PIC X(1).            00022410
022420             15  WT03-RDZ-PAST-DUE-AMT-30-36  PIC S9(07)V99.      00022420
022430             15  FILLER                      PIC X(1).            00022430
022440             15  WT03-RDZ-PAST-DUE-AMT-37-45  PIC S9(07)V99.      00022440
022450             15  FILLER                      PIC X(21).           00022450
022460*                                                                 00022460
022470     05  WT03-SUB                       PIC 9(3) COMP VALUE 1.    00022470
022480*                                                                 00022480
022490     05  WW-WORK-AREAS.                                           00022490
022500         10  FILLER                          PIC X(10) VALUE      00022500
022510             'WORK AREAS'.                                        00022510
022520*                                                                 00022520
022530     05  WW01-CURRENT-DATE.                                       00022530
022540         10  WW01-CURRENT-MONTH              PIC 99.              00022540
022550         10  FILLER                          PIC X.               00022550
022560         10  WW01-CURRENT-DAY                PIC 99.              00022560
022570         10  FILLER                          PIC X.               00022570
022580         10  WW01-CURRENT-YEAR               PIC 99.              00022580
022590                                                                  00022590
022600     05  WW01-UNIT-ARP-PRICE                PIC 999.              00022600
022610                                                                  00022610
022620     05  WW01-IRSOUNT-AMOUNT                PIC 9(5)V99 COMP-3.   00022620
022630     05  WW01-NET-AMOUNT                     PIC 9(7)V99 COMP-3.  00022630
022640     05  WW01-PT-AMOUNT                    PIC 9(11)V99 COMP-3.   00022640
022650     05  WW01-FE-PERCENT                     PIC 9(3) COMP-3.     00022650
022660     05  FILLER REDEFINES WW01-FE-PERCENT.                        00022660
022670         10  WW01-FE-PERCENT-1               PIC X.               00022670
022680         10  WW01-FE-PERCENT-2               PIC X.               00022680
022690*                                                                 00022690
022700     05  WW01-SMLSORT-PARMS.                                      00022700
022710         10  WW01-ENTRY-LENGTH       PIC 9999 COMP VALUE 0008.    00022710
022720         10  WW01-KEY-LENGTH         PIC 9999 COMP VALUE 0005.    00022720
022730         10  WW01-KEY-LOCATION       PIC 9999 COMP VALUE 0001.    00022730
022740         10  WW01-NUMBER-ENTRIES     PIC 9999 COMP VALUE 0000.    00022740
022750     05  WW01-CUST-PRCE1-LIMIT      PIC S9(9) COMP-3.             00022750
022760                                                                  00022760
022770     05  WY-RECORD-KEYS.                                          00022770
022780         10  FILLER                          PIC X(11) VALUE      00022780
022790             'RECORD KEYS'.                                       00022790
022800*                                                                 00022800
022810     05  WY01-RELTV-KEY                      PIC S9(8) COMP SYNC  00022810
022820                                                       VALUE +2.  00022820
022830     05  WY01-TERMS-CODE                     PIC 9(3).            00022830
022840     EJECT                                                        00022840
022850*                                                                 00022850
022860 01  LW-LINKAGE-WORKING-STORAGE.                                  00022860
022870     05  FILLER                              PIC X(23) VALUE      00022870
022880         'LINKAGE WORKING STORAGE'.                               00022880
022890     05  LW01-DSNTIAR.                                            00022890
022900         10  LW01-ABEND-CODE                 PIC X(03).           00022900
022910*                                                                 00022910
022920 01  LK20-DATE-PARM-FUNCTIONS.                                    00022920
022930     05  LK20-CVTDT                          PIC 99 VALUE 0  COMP.00022930
022940     05  LK20-DAYNUM                         PIC 99 VALUE 4  COMP.00022940
022950     05  LK20-DAYSINMO                       PIC 99 VALUE 8  COMP.00022950
022960     05  LK20-DIFFDATE                       PIC 99 VALUE 12 COMP.00022960
022970     05  LK20-VARYDT                         PIC 99 VALUE 12 COMP.00022970
022980*                                                                 00022980
022990 01  CAL-DATE-PARMS.                                              00022990
023000     05  CAL-FUNCTION-CODE                  PIC 99        COMP.   00023000
023010     05  CAL-CALENDAR-DT-1                  PIC S9(7)     COMP-3. 00023010
023020     05  CAL-JULIAN-DT-1                    PIC S9(7)     COMP-3. 00023020
023030     05  CAL-CALENDAR-DT-2                  PIC S9(7)     COMP-3. 00023030
023040     05  CAL-JULIAN-DT-2                    PIC S9(7)     COMP-3. 00023040
023050     05  CAL-DAY-OF-WEEK                    PIC S9(5)     COMP-3. 00023050
023060     05  CAL-DAYS-IN-MONTH                      REDEFINES         00023060
023070         CAL-DAY-OF-WEEK                    PIC S9(5)     COMP-3. 00023070
023080     05  CAL-DAYS-DIFFERENCE                    REDEFINES         00023080
023090         CAL-DAY-OF-WEEK                    PIC S9(5)     COMP-3. 00023090
023100     05  CAL-RETURN-CODE                    PIC 99        COMP.   00023100
023110*                                                                 00023110
023120 01  CAL-INIT-DATE-PARMS.                                         00023120
023130     05  CAL-INIT-FUNCTION-CODE      PIC 99    VALUE 0    COMP.   00023130
023140     05  CAL-INIT-CALENDAR-DT-1      PIC S9(7) VALUE +0   COMP-3. 00023140
023150     05  CAL-INIT-JULIAN-DT-1        PIC S9(7) VALUE +0   COMP-3. 00023150
023160     05  CAL-INIT-CALENDAR-DT-2      PIC S9(7) VALUE +0   COMP-3. 00023160
023170     05  CAL-INIT-JULIAN-DT-2        PIC S9(7) VALUE +0   COMP-3. 00023170
023180     05  CAL-INIT-DAY-OF-WEEK        PIC S9(5) VALUE +0   COMP-3. 00023180
023190     05  CAL-INIT-RETURN-CODE        PIC 99    VALUE 0    COMP.   00023190
023200*                                                                 00023200
023210 01  0USER-ABEND.                                                 00023210
023220     05  LW40-ABEND-CODE                    PIC X(3).             00023220
023230     05  FILLER                             PIC X(4) VALUE 'DUMP'.00023230
023240*                                                                 00023240
023250     EJECT                                                        00023250
023260*                                                                 00023260
023270 LINKAGE SECTION.                                                 00023270
023280 01  LINK-PRM0.                                                   00023280
023290     05  PARM-LENGTH                        PIC S9(4) COMP.       00023290
023300     05  PARM                               PIC X(8).             00023300
023310*                                                                 00023310
023320     EJECT                                                        00023320
023330 PROCEDURE DIVISION USING LINK-PRM0.                              00023330
023340*                                                                 00023340
023350*                                                                 00023350
023360 000-PROGRAM-DRIVER SECTION.                                      00023360
023370     PERFORM 100-HOUSEKEEPING.                                    00023370
023380     PERFORM 300-LOOKUP                                           00023380
024110         UNTIL END-OF-TRANS-FILE.                                 00024110
023400     PERFORM 9500-TERMINATION.                                    00023400
023410     EJECT                                                        00023410
023420*                                                                 00023420
023510******************************************************************00023510
023520 100-HOUSEKEEPING SECTION.                                        00023520
023530*TODO Change TRANS-FILE-In to wwwww                                       
023540 110-OPEN-FILES.                                                          
023550*                                                                 00023550
023560     OPEN INPUT  TRANS-FILE-IN                                    00023560
023570                 DETAIL-FILE-IN                                   00023570
023580                 WREHOUSE-FILE-IN                                 00023580
023590                 OVRHED-FILE-IN                                   00023590
023600                 PAST-DUE-CRITERIA                                00023600
023610                 PL-PAST-DUE-CRITERIA                             00023610
023620                 VSAM-SUPPLR-FILE                                 00023620
023630                 TERMS-FILE                                       00023630
023640                 COST-CONTROL-FILE                                00023640
023650                 WAREHOUS-FILE-IN                                 00023650
023660          OUTPUT PAST-DUE-FILE-OUT                                00023660
023670                 DETAIL-FILE-OUT                                  00023670
023680                 WREHOUSE-FILE-OUT                                00023680
023690                 OVRFLE-FILE-OUT                                  00023690
023700                 REPORT-FILE-OUT                                  00023700
023710                 SUPPLR-BAL-OUT                                   00023710
023720                 SUPPLR-WREHOUSE-FILE-OUT                         00023720
023730                 LIST-FILE-OUT                                    00023730
023740                 CST9-FILE-OUT                                    00023740
023750                 COLLECT-FILE-OUT                                 00023750
023760                 TOTAL-PAGE                                       00023760
023770                 WAREHOUS-FILE-OUT                                00023770
023780                 LADING-FILE                                      00023780
023790                 LADING-COLR-FILE                                 00023790
023800                 LOOKUP-CNS-BIAS-FILE.                            00023800
023810 112-READ-WAREHOUS-FILE.                                          00023810
023820     READ WAREHOUS-FILE-IN INTO LIST-ACCT-REC                     00023820
023830        AT END                                                    00023830
023840           MOVE 'Y' TO WS01-WAREHOUS-SW.                          00023840
023850 114-INITIALIZE.                                                  00023850
023860     MOVE SPACES TO SUPPLR-CUST-REC SUPPLR-CUS-TOT-REC.           00023860
023870     MOVE HIGH-VALUES TO WT01-STORUNIT-TABLE.                     00023870
023880     SET SBTOT-SUBSC TO 1.                                        00023880
023890 115-INITIALIZE-STORUNIT-SBTOTS.                                  00023890
023900     IF SBTOT-SUBSC GREATER THAN 22                               00023900
023910         GO TO 116-INIT-RDZ-PD-RANGE-TABLE.                       00023910
023920     MOVE ZEROS TO SIZE-TOTAL-PRICE0-PROCESSED (SBTOT-SUBSC)      00023920
023930                   SIZE-KNOWN-LOCATION-ADJ (SBTOT-SUBSC)          00023930
023940                   SIZE-NET-SUPPLIERLOC-CHG-COLR(SBTOT-SUBSC)     00023940
023950                   SIZE-WREHOUSE-INCOME (SBTOT-SUBSC)             00023950
023960                   SIZE-DORMANT-RETURNS-USED (SBTOT-SUBSC)        00023960
023970                   SIZE-BAD-COLR-RECOVERIES (SBTOT-SUBSC)         00023970
023980                   SIZE-CATALOG001-COST (SBTOT-SUBSC)             00023980
023990                   SIZE-SUPPLR-RETURNS-CC (SBTOT-SUBSC)           00023990
024000                   SIZE-SUPPLR-SALESS (SBTOT-SUBSC)               00024000
024010                   SIZE-RETURN-PRICE0-POSTED (SBTOT-SUBSC)        00024010
024020                   SIZE-VOUCHERS (SBTOT-SUBSC)                    00024020
024030                   SIZE-SUPPLR-EXPNSE (SBTOT-SUBSC)               00024030
024040                   SIZE-SUPPLR-RETURNS-JE (SBTOT-SUBSC)           00024040
024050                   SIZE-BAD-COLR-CHARGE-OFFS (SBTOT-SUBSC)        00024050
024060                   SIZE-MISC-OFFSETS (SBTOT-SUBSC)                00024060
024070                   SIZE-NET-SUPPLIERLOC-CHG-JE (SBTOT-SUBSC)      00024070
024080                   SIZE-REFUNDED-TO-SUPPLR (SBTOT-SUBSC)          00024080
024090                   SIZE-FE-CONTRACT-CODES (SBTOT-SUBSC)           00024090
024100                   SIZE-OTHER-CONTRACT-CODES (SBTOT-SUBSC)        00024100
024110                   SIZE-PREV-GROSS (SBTOT-SUBSC)                  00024110
024120                   SIZE-PREV-IRSOUNT (SBTOT-SUBSC)                00024120
024130                   SIZE-PREV-NET (SBTOT-SUBSC)                    00024130
024140                   SIZE-CURRENT-GROSS (SBTOT-SUBSC)               00024140
024150                   SIZE-CURRENT-IRSOUNT (SBTOT-SUBSC)             00024150
024160                   SIZE-CURRENT-NET (SBTOT-SUBSC).                00024160
024170      SET SBTOT-SUBSC UP BY 1.                                    00024170
024180      GO TO 115-INITIALIZE-STORUNIT-SBTOTS.                       00024180
024190                                                                  00024190
024200 116-INIT-RDZ-PD-RANGE-TABLE.                                     00024200
024210     IF WT03-SUB GREATER THAN 100                                 00024210
024220         MOVE 1 TO WT03-SUB                                       00024220
024230         GO TO 120-DATE-AND-HEADINGS.                             00024230
024240                                                                  00024240
024250     MOVE ZEROS  TO WT03-RDZ-SUPPLR-CODE (WT03-SUB)               00024250
024260                    WT03-RDZ-INDUSTRY-CODE (WT03-SUB)             00024260
024270                    WT03-RDZ-PAST-DUE-AMT-1-6 (WT03-SUB)          00024270
024280                    WT03-RDZ-PAST-DUE-AMT-7-14 (WT03-SUB)         00024280
024290                    WT03-RDZ-PAST-DUE-AMT-15-19 (WT03-SUB)        00024290
024300                    WT03-RDZ-PAST-DUE-AMT-20-24 (WT03-SUB)        00024300
024310                    WT03-RDZ-PAST-DUE-AMT-25-29 (WT03-SUB)        00024310
024320                    WT03-RDZ-PAST-DUE-AMT-30-36 (WT03-SUB)        00024320
024330                    WT03-RDZ-PAST-DUE-AMT-37-45 (WT03-SUB).       00024330
024340                                                                  00024340
024350      ADD 1 TO WT03-SUB.                                          00024350
024360      GO TO 116-INIT-RDZ-PD-RANGE-TABLE.                          00024360
024370******************************************************************00024370
024380*        PROCESS CURRENT DATE ADJUSTED FOR MIDNIGHT               00024380
024390*        PROCESS CURRENT DATE LESS 30 DAYS FOR LIST TRANS         00024390
024400*        PROCESS CURRENT DATE LESS 60 DAYS FOR TERMINATING        00024400
024410*        PROCESS CURRENT DATE LESS 180 DAYS FOR TERMINATING       00024410
024411*        PROCESS DAY OF THE WEEK.                                 00024411
024420******************************************************************00024420
024430*                                                                 00024430
024440 120-DATE-AND-HEADINGS.                                           00024440
024450*                                                                 00024450
024460     MOVE TIME-OF-DAY TO COLR-TIME.                               00024460
024470     MOVE CAL-INIT-DATE-PARMS TO CAL-DATE-PARMS.                  024470  
024480     MOVE '111' TO LW40-ABEND-CODE.                               00024480
024490     MOVE 1 TO COLR-ABEND-CODE.                                   00024490
024500     IF PARM-LENGTH IS GREATER THAN +0                            00024500
024510         MOVE PARM TO WW01-CURRENT-DATE                           00024510
024520     ELSE                                                         00024520
024530         MOVE CURRENT-DATE TO WW01-CURRENT-DATE.                  00024530
024540     MOVE WW01-CURRENT-DAY   TO COLR-DATE-DAY.                    00024540
024550     MOVE WW01-CURRENT-YEAR  TO COLR-DATE-YEAR.                   00024550
024560     MOVE WW01-CURRENT-MONTH TO COLR-DATE-MONTH                   00024560
024571     IF COLR-DATE-YEAR IS LESS THAN 50                            00024571
024580         MOVE 1 TO COLR-DATE-CENTURY                              00024580
024590     ELSE                                                         00024590
024600         MOVE 0 TO COLR-DATE-CENTURY.                             00024600
024610     MOVE COLR-WORK-DATE TO COLR-CYYMMDD-CURRENT.                 00024610
024620     MOVE COLR-CYYMMDD-CURRENT TO CAL-CALENDAR-DT-1.              00024620
024630     IF COLR-TIME IS LESS THAN 120000                             00024630
024640         SUBTRACT 1 FROM CAL-DAYS-DIFFERENCE.                     0024640 
024650     MOVE LK20-DIFFDATE TO CAL-FUNCTION-CODE.                     0024650 
024660     PERFORM 9000-DATE-PROCESSING.                                00024660
024670     PERFORM 9600-DATE-RETURN-CODE.                               00024670
024680     MOVE CAL-CALENDAR-DT-2 TO COLR-CYYMMDD-CURRENT               00024680
024690                                COLR-WORK-DATE.                   00024690
024700     MOVE COLR-DATE-MONTH TO IBM3-C-MONTH WW01-CURRENT-MONTH      00024700
024710                             COLR-CURRENT-MM.                     00024710
024720     MOVE COLR-DATE-DAY   TO IBM3-C-DAY WW01-CURRENT-DAY          00024720
024730                             COLR-CURRENT-DD.                     00024730
024740     MOVE COLR-DATE-YEAR  TO IBM3-C-YEAR WW01-CURRENT-YEAR        00024740
024750                             COLR-CURRENT-YY.                     00024750
024760     MOVE WW01-CURRENT-DATE TO HEADER-DATE.                       00024760
024770     MOVE WW01-CURRENT-DATE TO TOTAL-DATE.                        00024770
024780     MOVE CAL-JULIAN-DT-2 TO COLR-CYYJJJ-CURRENT.                 00024780
024790     MOVE COLR-DATE-YEAR TO IBM3-C-YEAR COLR-AGO-YEAR.            00024790
024800     MOVE COLR-DATE-MONTH TO IBM3-C-MONTH COLR-AGO-MONTH.         00024800
024810     MOVE COLR-DATE-DAY TO IBM3-C-DAY.                            00024810
024820     MOVE '28' TO COLR-AGO-DAY.                                   00024820
024830*                                                                 00024830
024840*                                                                 00024840
024850     MOVE CAL-INIT-DATE-PARMS TO CAL-DATE-PARMS.                  024850  
024860     MOVE COLR-CYYMMDD-AGO TO CAL-CALENDAR-DT-1.                  00024860
024870     SUBTRACT 365 FROM CAL-DAYS-DIFFERENCE.                       0024870 
024880     MOVE LK20-DIFFDATE TO CAL-FUNCTION-CODE.                     0024880 
024890     PERFORM 9000-DATE-PROCESSING.                                00024890
024900     PERFORM 9600-DATE-RETURN-CODE.                               00024900
024910     MOVE CAL-CALENDAR-DT-2 TO COLR-WORK-DATE.                    00024910
024920     MOVE COLR-DATE-YEAR TO IBM3-LAST-YEAR.                       00024920
024930     MOVE COLR-DATE-MONTH TO IBM3-LAST-MONTH.                     00024930
024940     MOVE COLR-DATE-DAY TO IBM3-LAST-DAY.                         00024940
024950*                                                                 00024950
024960     MOVE CAL-INIT-DATE-PARMS TO CAL-DATE-PARMS.                  024960  
024970     MOVE COLR-CYYMMDD-AGO TO CAL-CALENDAR-DT-1.                  00024970
024980     SUBTRACT 60 FROM CAL-DAYS-DIFFERENCE.                        0024980 
024990     MOVE LK20-DIFFDATE TO CAL-FUNCTION-CODE.                     0024990 
025000     PERFORM 9000-DATE-PROCESSING.                                00025000
025010     PERFORM 9600-DATE-RETURN-CODE.                               00025010
025020     MOVE CAL-CALENDAR-DT-2 TO COLR-WORK-DATE.                    00025020
025030     MOVE COLR-DATE-YEAR TO COLR-TWO-YEAR.                        00025030
025040     MOVE COLR-DATE-MONTH TO COLR-TWO-MONTH.                      00025040
025050*                                                                 00025050
025060     MOVE CAL-INIT-DATE-PARMS TO CAL-DATE-PARMS.                  025060  
025070     MOVE COLR-CYYMMDD-AGO TO CAL-CALENDAR-DT-1.                  00025070
025080     SUBTRACT 180 FROM CAL-DAYS-DIFFERENCE.                       0025080 
025090     MOVE LK20-DIFFDATE TO CAL-FUNCTION-CODE.                     0025090 
025100     PERFORM 9000-DATE-PROCESSING.                                00025100
025110     PERFORM 9600-DATE-RETURN-CODE.                               00025110
025120     MOVE CAL-CALENDAR-DT-2 TO COLR-WORK-DATE.                    00025120
025130     MOVE COLR-DATE-YEAR TO COLR-SIX-YEAR.                        00025130
025140     MOVE COLR-DATE-MONTH TO COLR-SIX-MONTH.                      00025140
025141*                                                                 00025141
025142     MOVE CAL-INIT-DATE-PARMS TO CAL-DATE-PARMS.                  025142  
025143     MOVE COLR-CYYMMDD-CURRENT TO CAL-CALENDAR-DT-1.              00025143
025145     MOVE LK20-DAYNUM TO CAL-FUNCTION-CODE.                       0025145 
025146     PERFORM 9000-DATE-PROCESSING.                                00025146
025147     PERFORM 9600-DATE-RETURN-CODE.                               00025147
025148     MOVE CAL-DAY-OF-WEEK TO COLR-CURR-DAY-OF-WEEK.               00025148
025151     EJECT                                                        00025151
025160*                                                                 00025160
025170******************************************************************00025170
025180*     1. READS THE COST CONTROL FILE (REC 3,DISPL 2).            *00025180
025190*     2. PRICE0 FOR INVALID KEY OR NULL RECORD.                  *00025190
025200*     3. ABORTS PROGRAM IF COST FILE READ IS BAD.                *00025200
025210*     4. SAVE THE STATEMENT DATE AS YYMMDD.                      *00025210
025220*     5. ZERO OUT FIELDS IN THE SUPPLR BALANCE RECORD.           *00025220
025230******************************************************************00025230
025240 120-READ-COST-CTLFILE.                                           00025240
025250*                                                                 00025250
025260     READ COST-CONTROL-FILE INTO CTLFILE-REC                      00025260
025270          AT END MOVE 'Y' TO WS01-INVALID-SWITCH.                 00025270
025280     IF CTLFILE-PRIME EQUAL ZEROS OR INVALID-KEY                  00025280
025290          MOVE '222' TO LW40-ABEND-CODE                           00025290
025300          MOVE 2 TO COLR-ABEND-CODE                               00025300
025310          PERFORM 9700-ABEND.                                     00025310
025320     MOVE CTLFILE-STMT-DATE TO IBM3-WORK-MMDDYY.                  00025320
025330     MOVE IBM3-W-MONTH TO IBM3-STMT-MONTH.                        00025330
025340     MOVE IBM3-W-DAY TO IBM3-STMT-DAY.                            00025340
025350     MOVE IBM3-W-YEAR TO IBM3-STMT-YEAR.                          00025350
025360     MOVE SPACES TO SUPPLR-BAL-REC.                               00025360
025370     MOVE SPACES TO SUPPLR-BAL-SUPPLR-NO.                         00025370
025380     MOVE ZEROS TO SUPPLR-BAL-DATE                                00025380
025390                   SUPPLR-BAL-BILLING-11-30                       00025390
025400                   SUPPLR-BAL-BILLING-31-60                       00025400
025410                   SUPPLR-BAL-BILLING-61-90                       00025410
025420                   SUPPLR-BAL-BILLING-91-180                      00025420
025430                   SUPPLR-BAL-BILLING-181-UP                      00025430
