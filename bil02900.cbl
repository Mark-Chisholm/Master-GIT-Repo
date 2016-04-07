000010  CBL SQL('QUALIFIER CICS BINDFILE')
000020 IDENTIFICATION DIVISION.
DLXMIG* DLXMIG Changes start here
DLXMIG*  PROGRAM-ID. BIL02900.
DLXMIG   PROGRAM-ID. "bil02900".
DLXMIG* DLXMIG Changes end here
000070*PROGRAM-NAME. CREATE WEEKLY BILLING REPORT.
000080   AUTHOR. JACK MASON.
000090   INSTALLATION. DELUXE FINANCIAL SERVICES, INC.
000100   DATE-WRITTEN. 12-09-98.
000110   DATE-COMPILED.
000120******************************************************************        
000130*
000140*RUN INSTRUCTIONS         = = =  PASS 1  = = =            12-09-98
000150*
000160*I545 = 545 INPUT FROM BIL470 OR BIL471       VARIABLE LRECL=1005
000170*       BILLING TRANSACTION FILE
000180*
000190*IOWRK = WORK OUTPUT TO PASS 2 VIA SORT                LRECL=101
000200*
000210*SYSOUT = REVISION INFO, FILE COUNTS, ERROR MESSAGES; ANY PAPER
000220*
000230*PARM = 'PASS=1'
000240*
000250******************************************************************        
000260*
000270*RETURN CODES             = = =  PASS 1  = = =            12-09-98
000280*
000290*  16 = RUN ABORTED.  SEE SYSOUT.
000300*         - INVALID PARM DATA
000310*         - EMPTY I545 FILE
000320*         - ALL I545 RECORDS HAD NON-WEEKLY FLAG '1'
000330*         - INCORRECT I545 UPDATE CODE
000340*         - MISMATCHED BILLING DATES
000350*
000360*  04 = INFO ONLY.  RUN CONTINUED.  SEE SYSOUT.
000370*         - EXTRANEOUS PARM DATA
000380*
000390******************************************************************        
000400*
000410*RUN INSTRUCTIONS         = = =  PASS 2  = = =            12-09-98
000420*
000430*I575 = 575 INPUT FROM BIL560 VIA SORT                 LRECL=135
000440*       NACHA PARAMETERS FILE
000450*
000460*IOWRK = WORK INPUT FROM PASS 1 VIA SORT               LRECL=101
000470*
000480*I8929I - INVOICE TRANSACTION RECORD                  LRECL = 300
000490*         IMAGE FROM BIL29100
000500*
000510*I8929P - INVOICE TRANSACTION RECORD                  LRECL = 300
000520*         PRINT FROM BIL29100
000530*
000540*I8929E - INVOICE TRANSACTION RECORD                  LRECL = 300
000550*         EMAIL FROM BIL29100
000560*
000570*PRINTER1 = WEEKLY BILLING REPORT, 132 CHARS/LINE     (LRECL=133)
000580*
000590*SYSOUT = REVISION INFO, FILE COUNTS, ERROR MESSAGES; ANY PAPER
000600*
000610*PARM = 'PASS=2'
000620*
000630******************************************************************        
000640*
000650*RETURN CODES             = = =  PASS 2  = = =            12-09-98
000660*
000670*  16 = RUN ABORTED.  SEE SYSOUT.
000680*         - INVALID PARM DATA
000690*         - EMPTY FILE; I575, IOWRK
000700*         - SEQUENCE ERROR; I575, IOWRK
000710*         - MISMATCHED BILLING DATES
000720*         - TABLE OVERFLOW; NACHA TAPE CODE OR CREDIT BC
000730*
000740*  12 = ERROR.  RUN CONTINUED.  REPORT INCORRECT.  SEE SYSOUT.
000750*         - IOWRK NACHA TAPE CODE INVALID PER I575 (ITEMS FOR
000760*             THIS CODE WILL BE COUNTED AS "PAPER")
000770*
000780*  08 = INFO ONLY.  RUN CONTINUED.  SEE SYSOUT.
000790*         - EMPTY I8929I OR I8929P OR I8929E FILE
000800*
000810*  04 = INFO ONLY.  RUN CONTINUED.  SEE SYSOUT.
000820*         - EXTRANEOUS PARM DATA
000830*         - TABLE OVER 80% FULL; NACHA TAPE CODE OR CREDIT BC
000840*
000850******************************************************************        
000860*
000870*NARRATIVE                                                12-14-09
000880*
000890*PURPOSE:
000900*  TO GATHER AND REPORT INFORMATION ABOUT A WEEK'S BILLING.
000910*
000920*SOURCE OF INFORMATION:
000930*  PASS 1
000940*    THE 545 FILE IN ANY ORDER.
000950*      FILES FOR ALL REGIONS MAY BE INCLUDED IN ONE EXECUTION OF
000960*            PASS 1, OR EACH REGION MAY BE RUN SEPARATELY THRU
000970*            PASS 1 WITH WORK FILES SORTED TOGETHER FOR PASS 2.
000980*  PASS 2
000990*    THE WORK FILE IN PRICING INSTITUTION, NACHA TAPE CODE,
001000*           EXPENSE CODE BYTE 1, CREDIT FLAG ORDER
001010*      SORT FIELDS=(1,25,A).
001020*    THE 575 FILE IN NACHA TAPE CODE ORDER
001030*      SORT FIELDS=(1,3,A)
001040*        PROGRAM USES ONLY 'A' RECORDS, OTHER RECORD TYPES CAN
001050*              (AND SHOULD) BE OMITTED IN THE SORT
001060*          INCLUDE COND=(20,1,CH,EQ,C'A').
001070*
001080*PROGRAM FLOW:
001090*  COMMON
001100*    COMMON INITIALIZATION INVOLVES DISPLAYING REVISION/RUN INFO
001110*      AND PARSING THE PARM TO DETERMINE WHICH PASS IS BEING RUN.
001120*    THE COMMON CLOSE INCLUDES CLOSING THE WORK FILE (OPENED IN
001130*      BOTH PASSES - OUTPUT IN 1, INPUT IN 2), AND DEALING WITH
001140*      THE RETURN-CODE.
001150*  PASS 1
001160*    PASS 1 INITIALIZATION OPENS FILES, CHECKS THE I545 AGAINST
001170*      BEING EMPTY, AND STORES THE I545 BILLING DATE.
001180*    PASS 1'S MAIN PROCESS IS TO EVALUATE EACH I545 RECORD:
001190*      - IF IT HAS THE WRONG UPDATE CODE, ABORT.
001200*      - IF ITS BILLING DATE DOESN'T MATCH THAT STORED IN
001210*          INITIALIZATION, ABORT.
001220*      - IF IT HAS A NON-WEEKLY FLAG OF '1', BYPASS IT.
001230*      - OTHERWISE, BUILD AND WRITE A WORK RECORD.
001240*    BUILDING THE WORK RECORD IS LARGELY MOVING SELECTED FIELDS
001250*      FROM THE 545.  THE THREE AMOUNT FIELDS, HOWEVER, REQUIRE
001260*      A LITTLE PROCESSING.  DUE DELUXE IS CALCULATED BY
001270*      DETERMINING IF WE COLLECT TAX AND/OR UPCHARGE.  THE TWO
001280*      'ABSOLUTE' NUMBERS ARE FORCED TO BE POSITIVE (NEGATIVES ARE
001290*      SUBTRACTED FROM ZERO).
001300*    AT THE END OF THE 545 FILE, PASS 1 CLOSES THE 545 (THE COMMON
001310*      CLOSE WILL TAKE CARE OF THE WORK FILE), AND DISPLAYS FILE
001320*      COUNTS.
001330*  PASS 2
001340*    PASS 2 INITIALIZATION OPENS FILES, CHECKS AGAINST EMPTY
001350*      INPUTS, STORES THE BILLING DATE FROM THE FIRST WORK RECORD,
001360*      AND FORMATS THE BILLING DATE FOR THE REPORT HEADING.
001370*    PASS 2 NEXT LOADS A TABLE OF I575 NACHA TAPE NUMBERS WITH TWO
001380*      OTHER FIELDS:
001390*      - AN INDICATOR OF THE BILLING MEDIUM (E.G., WIRE).
001400*      - AN INDICATOR THAT NO BILLING HAS YET BEEN FOUND FOR THAT
001410*          TAPE CODE (TO BE CHANGED IF AND WHEN BILLING IS FOUND
001420*          THAT WOULD INDICATE THE TAPE WAS CREATED THIS WEEK).
001430*    PASS 2'S IOWRK PROCESSING GATHERS VARIOUS INFORMATION
001440*      FROM THE FILE.  THIS PROCESSING HAS SEVERAL LOOPS
001450*      FOR DIFFERENT LEVELS OF SEQUENCE BREAKS.  THIS IS BECAUSE
001460*      SOME OF THE INFORMATION THAT NEEDS TO BE GATHERED IS IN
001470*      THE FORM OF COUNTS OF BCS OR BRANCHES WITH VARIOUS
001480*      CONDITIONS (E.G., DEBITS OR 50+ BILLING ERROR CREDITS).
001490*    PASS 2'S REPORTING PROCESS DOES NOT BEGIN UNTIL ALL DATA
001500*      HAS BEEN GATHERED.  SOME DATA IS SIMPLY REPORTED AS IT
001510*      WAS ACCUMULATED IN THE IOWRK PROCESS.  SOME FIELDS
001520*      NEED TO BE CALCULATED (PERCENTAGES, AVERAGES).
001530*    PASS 2'S INVOICING RECAP PART OF REPORT DISPALYS INVOICE
001540*      COUNTS OFF I8929I, I8929P, AND I8929E FILES.
001550*    PASS 2'S CLOSE CLOSES ALL ITS FILES EXCEPT IOWRK (WHICH IS
001560*      HANDLED BY THE COMMON CLOSE), AND DISPLAYS FILE COUNTS.
001570*    PASS 2'S GETTING THE PRODUCT DESCRIPTIONS FROM DATA OBJECT 3.
001580*
001590*NOTE:
001600*  TOTAL BILLING ABSOLUTE DOLLARS IS THE SUM OF
001610*    (THE ABSOLUTE VALUE OF (GROSS MINUS UPCHARGE))
001620*    PLUS (THE ABSOLUTE VALUE OF UPCHARGE).
001630*  THUS DOWNCHARGE INCREASES THE TOTAL.
001640*  LIKEWISE A CAPS CREDIT INCREASES THE TOTAL.
001650*  BUT A CAPS DISCOUNT OR GAS DISCOUNT DECREASES THE TOTAL.
001660*
001670******************************************************************        
001680*
001690*REVISION HISTORY
001700*
001710*02-11-02  REMOVED  'DEPS' AS A VALID OPTION IN NACHA TAPE CODE.
001720*          REVMOVE 'DEPS' FROM BILLING RECAP REPORT ALSO.
001730*                         CAPS #27467             ASHOK MENON  08
001740*
001750*03-11-02  REMOVED  I900 FILE AND ALL RELATED LOGIC. INCLUDED
001760*          I8929I, I8929P, AND I8929E FOR INVOICE COUNTS IN
001770*          IN PASS2. THE INVOICE RECAP SECTION OF THE REPORT WILL
001780*          SHOW UP ALL DIFFERENT INVOICE COUNTS
001790*                         CAPS #27468            SYED HUSSAIN  09
001800*
001810*06-10-02  MODIFIED THE PROGRAM TO INCLUDE A COLUMN AND ASSOCIATED
001820*          COUNT FOR THE NUMBER OF RECORDS FROM THE I8929P FILE
001830*          THAT HAVE RECORD TYPE "50". THIS COUNT REFLECTS THE
001840*          NUMBER OF INVOICES THAT HAVE REMITTANCE STUBS
001850*          ASSOCIATED WITH THEM.
001860*                         CAPS #27471            ASHOK MENON   10
001870*
001880*08-14-02  MODIFIED THE PROGRAM FOR TRACING RESTITUTIONS/
001890*          REIMBURSEMENTS.
001900*          CM #BILL791     REMEDY #1819                PRASAD  11
001910*
001920*08-26-02  MODIFIED THE PROGRAM FOR REFORMATTING THE REPORT BASE
001930*          AND REPORT DELIVIERY VALUES
001940*          CM #BILL803     REMEDY #2613                PRASAD  12
001950*
001960*12-16-02  MODIFIED THE PROGRAM FOR ADDING THREE NEW COLUMNS FOR
001970*          MASS CREDITS WHEN GREATER THAN 99
001980*          CM #BILL850     REMEDY #ACM00009197         PRASAD  13
001990*
002000*10-20-08  INSERTED AN ENTRY FOR DSI INVOICES INTO THE INVOICING
002010*            RECAP LINE, AND CORRECTED THE DATA USED FOR THE EMAIL
002020*            INVOICES ENTRY IN THAT SAME LINE.
002030*          PART OF SIMPLIFICATION PROJECT.
002040*          IMS ISSUE = JDBT-6VJSF9.                 JACK MASON  14
002050*
002060*12-15-08  REMOVED REPORT LINES RELATING TO OBSOLETE MAG TAPE AND
002070*            NON-WEEKLY.
002080*          PART OF SIMPLIFICATION PROJECT.
002090*          IMS ISSUE = JDBT-6VJSF9.                 JACK MASON  15
002100*
002110*12-14-09  CHANGED ONE SECTION OF THE REPORT, DEALING WITH BCS AND
002120*            CREDIT REASON CODES, TO LIST SITUATIONS WITH OVER 49
002130*            INSTANCES (HAD BEEN OVER 99).
002140*          PART OF SIMPLIFICATION PROJECT.
002150*          IMS ISSUE = JDBT-6VJSF9.                 JACK MASON  16
002160*
DLXMIG**----------------------------------------------------------------    
DLXMIG**  DATE      DESCRIPTION                        INITIALS             
DLXMIG** -------  ------------------------------------------------------    
DLXMIG** 011310   UPDATED FOR DLX MIGRATION            COGNIZANT            
DLXMIG** -------  ------------------------------------------------------    
002220*
002230*03-14-11  ADDED REPORT SECTION FOR DUE DELUXE TOTALS BY ACH TAPE
002240*            NUMBER
002250*          CORE SIMP PROJECT (IMS = JDBT-6VJSF9)    JACK MASON  17
002260*
002270******************************************************************        
002280*
002290 ENVIRONMENT DIVISION.
002300 CONFIGURATION SECTION.
DLXMIG* DLXMIG Changes start here
DLXMIG*  SOURCE-COMPUTER. IBM-PC.
DLXMIG   SOURCE-COMPUTER. UNIX.
DLXMIG* DLXMIG Changes end here
DLXMIG* DLXMIG Changes start here
DLXMIG*  OBJECT-COMPUTER. IBM-370.
DLXMIG   OBJECT-COMPUTER. UNIX.
DLXMIG* DLXMIG Changes end here
002390   SPECIAL-NAMES.
002400     CSP IS NO-ADVANCE.
002410*
002420 INPUT-OUTPUT SECTION.
002430   FILE-CONTROL.
002440*                * PASS 1 *
DLXMIG     SELECT I545-FILE     ASSIGN RSD-I545.
002460*                * COMMON *
DLXMIG     SELECT IOWRK-FILE    ASSIGN RSD-IOWRK.
002480*                * PASS 2 *
DLXMIG     SELECT I575-FILE     ASSIGN RSD-I575.
002500     SELECT I550-FILE     ASSIGN I550
002510                        ORGANIZATION IS INDEXED
002520                        ACCESS MODE IS DYNAMIC
002530                        RECORD KEY IS I550-RECORD-KEY
002540                        FILE STATUS IS WS-I550-FILE-STATUS.
DLXMIG     SELECT I8929I-FILE     ASSIGN RSD-I8929I.
DLXMIG     SELECT I8929P-FILE     ASSIGN RSD-I8929P.
DLXMIG     SELECT I8929E-FILE     ASSIGN RSD-I8929E.
002580     SELECT PRINTER1-FILE ASSIGN PRINTER1
DLXMIG                         ORGANIZATION IS LINE SEQUENTIAL.
002600*
002610 DATA DIVISION.
002620 FILE SECTION.
002630*
002640 FD  I545-FILE
002650*
002660 COPY T0545I.
002670*
002680 FD  IOWRK-FILE
002690     RECORDING MODE IS F
002700     BLOCK CONTAINS 0 RECORDS
002710     RECORD CONTAINS 101 CHARACTERS
002720     LABEL RECORDS STANDARD.
002730   01  IOWRK-REC.
002740     03  IOWRK-SEQ.
002750       05  IOWRK-INSTITUTION                     PIC X(20).
002760       05  IOWRK-TAPE-CODE                       PIC X(3).
002770       05  IOWRK-EXP-BYTE-1                      PIC X.
002780       05  IOWRK-CREDIT-FLAG                     PIC X.
002790     03  IOWRK-REASON-FOR-CREDIT                 PIC X(2).
002800     03  IOWRK-REGION                            PIC X(2).
002810     03  IOWRK-BILLING-DATE                      PIC X(6).
002820     03  IOWRK-NON-WEEKLY-FLAG                   PIC X.
002830     03  IOWRK-ABSOLUTE-REST        COMP-3       PIC S9(7)V9(2).
002840     03  IOWRK-ABSOLUTE-UPCHARGE    COMP-3       PIC S9(5)V9(2).
002850     03  IOWRK-DUE-DELUXE           COMP-3       PIC S9(7)V9(2).
002860     03  IOWRK-ORDER-COUNTS                      PIC 9(2).
002870     03  IOWRK-PRODUCT-ID                        PIC X(18).
002880     03  IOWRK-REPORT-BASE                       PIC S9(7)V9(2).
002890     03  IOWRK-REPORT-DELIVERY                   PIC S9(5)V9(2).
002900     03  IOWRK-REPORT-BASE-DELIVERY COMP-3       PIC S9(7)V9(2).
002910     03  IOWRK-UPCHRAGE             COMP-3       PIC S9(7)V9(2).
002920     03  IOWRK-GROSS                COMP-3       PIC S9(7)V9(2).
002930*
002940 FD  I575-FILE
002950*
002960 COPY T0575I.
002970*
002980*
002990 FD  I550-FILE
003000*
003010 COPY T0550I.
003020*
003030 FD  I8929I-FILE
003040*
003050 COPY T8929I.
003060*
003070 FD  I8929P-FILE
003080*
003090 COPY T8929O.
003100*
003110 FD  I8929E-FILE
003120     RECORDING MODE IS F
003130     BLOCK CONTAINS 0 RECORDS
003140     RECORD CONTAINS 300 CHARACTERS
003150     LABEL RECORDS ARE STANDARD.
003160*
003170 COPY T8929W.
003180*
003190 FD  PRINTER1-FILE
003200     RECORDING MODE IS F
003210     BLOCK CONTAINS 0 RECORDS
003220     RECORD CONTAINS 132 CHARACTERS
003230     LABEL RECORDS OMITTED.
003240   01  PRINTER1-REC                              PIC X(132).
003250*                * M LINE IS FOR VARIOUS REPORTS BY MEDIUM *
003260   01  PR1-M-LINE.
003270     03  FILLER                                  PIC X(3).
003280     03  PR1-M-CAPT-1                            PIC X(9).
003290     03  PR1-M-DLRS-1                         PIC ----,---,---.99.
003300     03  FILLER REDEFINES PR1-M-DLRS-1.
003310       05  PR1-M-NBR-1                           PIC ----,---,--9.
003320       05  FILLER                                PIC X(3).
003330     03  FILLER                                  PIC X(2).
003340     03  PR1-M-PCT-1                             PIC ----.99.
003350     03  FILLER                                  PIC X(11).
003360     03  PR1-M-CAPT-2                            PIC X(9).
003370     03  PR1-M-DLRS-2                         PIC ----,---,---.99.
003380     03  FILLER REDEFINES PR1-M-DLRS-2.
003390       05  PR1-M-NBR-2                           PIC ----,---,--9.
003400       05  FILLER                                PIC X(3).
003410     03  FILLER                                  PIC X(2).
003420     03  PR1-M-PCT-2                             PIC ----.99.
003430     03  FILLER                                  PIC X(11).
003440     03  PR1-M-CAPT-3                            PIC X(9).
003450     03  PR1-M-DLRS-3                         PIC ----,---,---.99.
003460     03  FILLER REDEFINES PR1-M-DLRS-3.
003470       05  PR1-M-NBR-3                           PIC ----,---,--9.
003480       05  FILLER                                PIC X(3).
003490     03  FILLER                                  PIC X(2).
003500     03  PR1-M-PCT-3                             PIC ----.99.
003510     03  FILLER                                  PIC X(8).
003520*                * I LINE IS FOR INVOICING REPORT *
003530   01  PR1-I-LINE.
003540     03  FILLER                                  PIC X(17).
003550     03  PR1-I-CT-PRINT                          PIC ----,---,--9.
003560     03  FILLER                                  PIC X(3).
003570     03  PR1-I-CT-IMAGE                          PIC ----,---,--9.
003580     03  FILLER                                  PIC X(3).
003590     03  PR1-I-CT-EMAIL                          PIC ----,---,--9.
003600     03  FILLER                                  PIC X(3).
003610     03  PR1-I-CT-DSI                            PIC ----,---,--9.
003620     03  FILLER                                  PIC X(3).
003630     03  PR1-I-CT-DNM                            PIC ----,---,--9.
003640     03  FILLER                                  PIC X(3).
003650     03  PR1-I-CT-STUB                           PIC ----,---,--9.
003660     03  FILLER                                  PIC X(28).
003670*                * E LINE IS ACH DUE DELUXE *
003680   01  PR1-E-LINE.
003690     03  FILLER                                  PIC X(25).
003700     03  PR1-E-TAPE-CODE                         PIC X(3).
003710     03  FILLER                                  PIC X(4).
003720     03  PR1-E-DUE-DLX                        PIC ----,---,--9.99.
003730     03  FILLER                                  PIC X(85).
003740*                * A LINE IS FOR ACCURACY REPORT *
003750   01  PR1-A-LINE.
003760     03  FILLER                                  PIC X(1).
003770     03  PR1-A-CAPT                              PIC X(12).
003780     03  FILLER                                  PIC X.
003790     03  PR1-A-TRN-CT                            PIC ----,---,--9.
003800     03  FILLER                                  PIC X.
003810     03  PR1-A-ERR-01-CT                         PIC --,---,--9.
003820     03  FILLER                                  PIC X.
003830     03  PR1-A-ERR-02-CT                         PIC --,---,--9.
003840     03  FILLER                                  PIC X.
003850     03  PR1-A-ERR-03-CT                         PIC --,---,--9.
003860     03  FILLER                                  PIC X.
003870     03  PR1-A-ERR-06-CT                         PIC --,---,--9.
003880     03  FILLER                                  PIC X.
003890     03  PR1-A-ERR-08-CT                         PIC --,---,--9.
003900     03  FILLER                                  PIC X.
003910     03  PR1-A-ERR-09-CT                         PIC --,---,--9.
003920     03  FILLER                                  PIC X.
003930     03  PR1-A-TOT-ERR-CT                        PIC --,---,--9.
003940     03  FILLER                                  PIC X(4).
003950     03  PR1-A-PCT                               PIC ----.99.
003960     03  FILLER                                  PIC X(3).
003970     03  PR1-A-ERR-BC-CT                         PIC --,---,--9.
003980     03  FILLER                                  PIC X(5).
003990*                * B LINE IS FOR 100+ CREDIT/ERROR BC INFO *
004000   01  PR1-B-LINE.
004010     03  FILLER                              PIC X(1).
004020     03  PR1-B-BC                            PIC X(8).
004030     03  FILLER                              PIC X(1).
004040     03  PR1-B-COUNT                         PIC --,---,--9.
004050     03  FILLER                              PIC X(1).
004060     03  PR1-B-CR-ORD-COUNT                  PIC --,---,--9.
004070     03  FILLER                              PIC X(3).
004080     03  PR1-B-ERR-ENTRY          OCCURS 6 TIMES
004090                                  INDEXED BY IX-PR1-B-ERR.
004100       05  PR1-B-ERROR-CODE                  PIC X(2).
004110       05  FILLER                            PIC X.
004120     03  PR1-B-FI-NAME                       PIC X(35).
004130     03  FILLER                              PIC X(1).
004140     03  PR1-B-REP-BASE-DELEVERY             PIC $$,$$$,$$9.99.
004150     03  FILLER                              PIC X(1).
004160     03  PR1-B-UPCHARGE                      PIC $$,$$$,$$9.99.
004170     03  FILLER                              PIC X(1).
004180     03  PR1-B-GROSS                         PIC $$,$$$,$$9.99.
004190     03  FILLER                              PIC X(3).
004200*
004210*                * C LINE IS FOR 100+ CREDIT/ERROR BC-CODE INFO *
004220   01  PR1-C-LINE.
004230     03  FILLER                              PIC X(1).
004240     03  PR1-C-BC                            PIC X(8).
004250     03  FILLER                              PIC X(1).
004260     03  PR1-C-COUNT                         PIC --,---,--9.
004270     03  FILLER                              PIC X(1).
004280     03  PR1-C-CR-ORD-COUNT                  PIC --,---,--9.
004290     03  FILLER                              PIC X(6).
004300     03  PR1-C-ERROR-CODE                    PIC X(2).
004310     03  FILLER                              PIC X(13).
004320     03  PR1-C-FI-NAME                       PIC X(35).
004330     03  FILLER                              PIC X(1).
004340     03  PR1-C-REP-BASE-DELEVERY             PIC $$,$$$,$$9.99.
004350     03  FILLER                              PIC X(1).
004360     03  PR1-C-UPCHARGE                      PIC $$,$$$,$$9.99.
004370     03  FILLER                              PIC X(1).
004380     03  PR1-C-GROSS                         PIC $$,$$$,$$9.99.
004390     03  FILLER                              PIC X(3).
004400*                * D LINE IS FOR RESTITUTIONS/REIMBURSEMENTS INFO*
004410   01  PR1-D-LINE.
004420     03  FILLER                                  PIC X(3).
004430     03  PR1-D-PROD-CODE                         PIC X(13).
004440     03  FILLER                                  PIC X(4).
004450     03  PR1-D-PROD-INFO                         PIC X(25).
004460     03  FILLER                                  PIC X(5).
004470     03  PR1-D-BC                                PIC X(8).
004480     03  FILLER                                  PIC X(5).
004490     03  PR1-D-FI-NAME                           PIC X(35).
004500     03  FILLER                                  PIC X(03).
004510     03  PR1-D-REP-BASE                          PIC $,$$$,$$9.99.
004520     03  FILLER                                  PIC X(3).
004530     03  PR1-D-REP-DEL                           PIC $,$$$,$$9.99.
004540     03  FILLER                                  PIC X(4).
004550*
004560 WORKING-STORAGE SECTION.
DLXMIG* DLXMIG Changes start here
DLXMIG COPY TDLXMIGE.
DLXMIG     EXEC SQL INCLUDE SQLCA END-EXEC.
DLXMIG* DLXMIG Changes end here
004610*
004620 01  WS-FLAGS-AND-SUCH                           VALUE SPACES.
004630   03  WS-I545-FILE-FLAG          PIC X.
004640     88  END-I545-FILE              VALUE 'E'.
004650   03  WS-I575-FILE-FLAG          PIC X.
004660     88  END-I575-FILE              VALUE 'E'.
004670   03  WS-I8929I-FILE-FLAG          PIC X.
004680     88  END-I8929I-FILE              VALUE 'E'.
004690   03  WS-I8929P-FILE-FLAG          PIC X.
004700     88  END-I8929P-FILE              VALUE 'E'.
004710   03  WS-I8929E-FILE-FLAG          PIC X.
004720     88  END-I8929E-FILE              VALUE 'E'.
004730   03  WS-IOWRK-FILE-FLAG         PIC X.
004740     88  END-IOWRK-FILE             VALUE 'E'.
004750   03  WS-I550-FILE-STATUS        PIC X(2).
004760     88  WS-I550-SUCCESSFUL         VALUE '00'.
004770     88  WS-I550-NO-SUCH-RECORD     VALUES '20', '23'.
004780     88  WS-I550-SUCCESSFUL-OPEN    VALUES '00', '97'.
004790     88  WS-I550-END-OF-FILE        VALUE '10'.
004800   03  WS-PASS-NUMBER             PIC X.
004810     88  PASS-1                     VALUE '1'.
004820   03  WS-RETURN-12-FLAG          PIC X.
004830     88  RETURN-12                  VALUE 'Y'.
004840   03  WS-RETURN-08-FLAG          PIC X.
004850     88  RETURN-08                  VALUE 'Y'.
004860   03  WS-RETURN-04-FLAG          PIC X.
004870     88  RETURN-04                  VALUE 'Y'.
004880   03  WS-BC-ERROR-FLAG           PIC X.
004890     88  WS-BC-ERROR                VALUE 'Y'.
004900   03  WS-BC-ERROR1-FLAG          PIC X.
004910     88  WS-BC-ERROR1               VALUE 'Y'.
004920   03 WS-REASON-SEQUENCE-FLAG    PIC X.
004930     88  WS-REASON-SEQUENCE         VALUE 'Y'.
004940*
004950 01  WS-I550-RECORD-KEY   VALUE SPACES.
004960   03  WS-I550-BC                 PIC X(8).
004970   03  WS-I550-BR-BR-T            PIC X(12).
004980*
004990 01  WS-SEQUENCES                                VALUE LOW-VALUES.
005000   03  WS-BILLING-DATE            PIC X(6).
005010   03  WS-IOWRK-SEQ.
005020     05  WS-IOWRK-TAPE-SEQ.
005030       07  WS-IOWRK-INSTITUTION.
005040         09  WS-IOWRK-BC      PIC X(8).
005050         09  WS-IOWRK-BR-BR-T PIC X(12).
005060       07  WS-IOWRK-TAPE-CODE PIC X(3).
005070     05  WS-IOWRK-SEQ-24-25.
005080       07  WS-IOWRK-EXP-BYTE-1    PIC X.
005090       07  WS-IOWRK-CREDIT-FLAG   PIC X.
005100   03  WS-HOLD-SEQ.
005110     05  WS-HOLD-TAPE-SEQ.
005120       07  WS-HOLD-INSTITUTION.
005130         09  WS-HOLD-BC           PIC X(8).
005140         09  WS-HOLD-BR-BR-T      PIC X(12).
005150       07  WS-HOLD-TAPE-CODE      PIC X(3).
005160     05  WS-HOLD-SEQ-24-25.
005170       07  WS-HOLD-EXP-BYTE-1     PIC X.
005180       07  WS-HOLD-CREDIT-FLAG    PIC X.
005190   03  WS-I575-TAPE-CODE          PIC X(3).
005200   03  WS-PREV-BAD-TAPE-CODE      PIC X(3).
005210*
005220 01  WS-INVOICE-COUNTS.
005230   03  WS-I8929-IMAGE-INV-CNT     PIC S9(7)      COMP-3 VALUE +0.
005240   03  WS-I8929-PRINT-INV-CNT     PIC S9(7)      COMP-3 VALUE +0.
005250   03  WS-I8929-EMAIL-INV-CNT     PIC S9(7)      COMP-3 VALUE +0.
005260   03  WS-I8929-DSI-INV-CNT       PIC S9(7)      COMP-3 VALUE +0.
005270   03  WS-I8929-DNM-INV-CNT       PIC S9(7)      COMP-3 VALUE +0.
005280   03  WS-I8929-STUB-INV-CNT      PIC S9(7)      COMP-3 VALUE +0.
005290*
005300 01  WS-FILE-COUNTS.
005310   03  WS-I545-REC-COUNT          PIC S9(9)      COMP-3 VALUE +0.
005320   03  WS-I545-BYPASS-COUNT       PIC S9(5)      COMP-3 VALUE +0.
005330   03  WS-IOWRK-REC-COUNT         PIC S9(9)      COMP-3 VALUE +0.
005340   03  WS-I575-REC-COUNT          PIC S9(7)      COMP-3 VALUE +0.
005350   03  WS-I575-BYPASS-COUNT       PIC S9(5)      COMP-3 VALUE +0.
005360   03  WS-I8929I-REC-COUNT        PIC S9(5)      COMP-3 VALUE +0.
005370   03  WS-I8929P-REC-COUNT        PIC S9(5)      COMP-3 VALUE +0.
005380   03  WS-I8929E-REC-COUNT        PIC S9(5)      COMP-3 VALUE +0.
005390*
005400 01  WS-PRINT-CONTROLS.
005410   03  WS-LINE-COUNT              PIC S9(3)      COMP VALUE +090.
005420   03  WS-LINE-SPACER             PIC S9         COMP VALUE +2.
005430 01  WS-HEADINGS-ETC.
005440   03  WS-HEADING-1.
005450     05  FILLER                   PIC X          VALUE '-'.
005460     05  WS-HD1-PROGRAM-NUMBER    PIC X(8)       VALUE SPACE.
005470     05  FILLER                   PIC X(9)      VALUE '-'.
005480     05  FILLER                   PIC X(58)      VALUE
005490            'WEEKLY BILLING REPORT - PAPER PAYMENT SYSTEMS (FEB)'.
005500     05  FILLER              PIC X(18) VALUE 'WEEK ENDING DATE: '.
005510     05  WS-HD1-BILL-DATE.
005520       07  WS-HD1-BILL-MONTH      PIC X(2)       VALUE '00'.
005530       07  FILLER                 PIC X          VALUE '-'.
005540       07  WS-HD1-BILL-DAY        PIC X(2)       VALUE '00'.
005550       07  FILLER                 PIC X          VALUE '-'.
005560       07  WS-HD1-BILL-CENTURY    PIC 9(2)       DISPLAY VALUE 0.
005570       07  WS-HD1-BILL-YEAR       PIC X(2)       VALUE '00'.
005580     05  FILLER              PIC X(18) VALUE '        RUN DATE: '.
005590     05  WS-HD1-RUN-MONTH         PIC X(2)       VALUE '00'.
005600     05  FILLER                   PIC X          VALUE '-'.
005610     05  WS-HD1-RUN-DAY           PIC X(2)       VALUE '00'.
005620     05  FILLER                   PIC X          VALUE '-'.
005630     05  WS-HD1-RUN-YEAR          PIC X(4)       VALUE '0000'.
005640   03  WS-M-HD-A1.
005650     05  FILLER                   PIC X(5)       VALUE SPACE.
005660     05  FILLER PIC X(44) VALUE 'TRANSACTIONS  - - - - - - - - -'.
005670     05  FILLER  PIC X(44) VALUE 'PRICED BANK CODES  - - - - - -'.
005680     05  FILLER  PIC X(39) VALUE 'PRICED BRANCHES  - - - - - - -'.
005690   03  WS-M-HD-2.
005700     05  FILLER                   PIC X(18)      VALUE SPACE.
005710     05  FILLER            PIC X(44) VALUE 'NUMBER    % OF TOTAL'.
005720     05  FILLER            PIC X(44) VALUE 'NUMBER    % OF TOTAL'.
005730     05  FILLER            PIC X(26) VALUE 'NUMBER    % OF TOTAL'.
005740   03  WS-M-FT-A.
005750     05  FILLER                   PIC X(78)      VALUE SPACE.
005760     05  FILLER PIC X(31) VALUE 'NUMBER OF WIRE TRANSMISSIONS:  '.
005770     05  WS-M-FT-A-WIRE-CT        PIC --,---,--9.
005780     05  FILLER                   PIC X(13)      VALUE SPACE.
005790   03  WS-M-HD-B1.
005800     05  FILLER                   PIC X(5)       VALUE SPACE.
005810     05  FILLER  PIC X(44) VALUE 'TOTAL BILLING  - - - - - - - -'.
005820     05  FILLER PIC X(44) VALUE 'NET DUE DELUXE  - - - - - - - -'.
005830     05  FILLER  PIC X(39) VALUE 'UPCHARGE / DOWNCHARGE  - - - -'.
005840   03  WS-M-HD-B2.
005850     05  FILLER                   PIC X(10)      VALUE SPACE.
005860     05  FILLER    PIC X(44) VALUE 'ABSOLUTE DOLLARS  % OF TOTAL'.
005870     05  FILLER    PIC X(44) VALUE '         DOLLARS  % OF TOTAL'.
005880     05  FILLER    PIC X(34) VALUE 'ABSOLUTE DOLLARS  % OF TOTAL'.
005890   03  WS-M-HD-C1.
005900     05  FILLER                   PIC X(5)       VALUE SPACE.
005910     05  FILLER  PIC X(44) VALUE 'RETAIL DEBIT TRANSACTIONS  - -'.
005920     05  FILLER  PIC X(44) VALUE 'WAIVED DEBIT TRANSACTIONS  - -'.
005930     05  FILLER PIC X(39) VALUE 'EXPENSE DEBIT TRANSACTIONS  - -'.
005940   03  WS-M-HD-D1.
005950     05  FILLER                   PIC X(5)       VALUE SPACE.
005960     05  FILLER PIC X(44) VALUE 'RETAIL CREDIT TRANSACTIONS  - -'.
005970     05  FILLER PIC X(44) VALUE 'WAIVED CREDIT TRANSACTIONS  - -'.
005980     05  FILLER  PIC X(39) VALUE 'EXPENSE CREDIT TRANSACTIONS  -'.
005990   03  WS-E-HD.
006000     05  FILLER                   PIC X(22)      VALUE SPACE.
006010     05  FILLER      PIC X(25)  VALUE 'TAPE CODE      DUE DELUXE'.
006020     05  FILLER                   PIC X(85)      VALUE SPACE.
006030   03  WS-I-HD-1.
006040     05  FILLER                   PIC X(44)      VALUE
006050                   '                     PRINTED        IMAGE   '.
006060     05  FILLER                   PIC X(44)      VALUE
006070                   '       E-MAIL         DSI            DNM    '.
006080     05  FILLER                   PIC X(44)      VALUE
006090                   '        STUB                                '.
006100     05  FILLER                   PIC X(44)      VALUE SPACES.
006110   03  WS-I-HD-2.
006120     05  FILLER                   PIC X(44)      VALUE
006130                   '                     INVOICES       INVOICES'.
006140     05  FILLER                   PIC X(44)      VALUE
006150                   '       INVOICES       INVOICES       INVOICE'.
006160     05  FILLER                   PIC X(44)      VALUE
006170                   'S       INVOICES                            '.
006180   03  WS-A-HD-1.
006190     05  FILLER                   PIC X(44)      VALUE
006200                   '                                         FI '.
006210     05  FILLER                   PIC X(44)      VALUE
006220                   'EMPL   MASS CR      WRONG                   '.
006230     05  FILLER                   PIC X(44)      VALUE SPACE.
006240   03  WS-A-HD-2.
006250     05  FILLER                   PIC X(44)      VALUE
006260                   '                            DUPLICATE    CHA'.
006270     05  FILLER                   PIC X(44)      VALUE
006280                   'RGED   BILLING      FI OR     TAXED      PER'.
006290     05  FILLER                   PIC X(44)      VALUE
006300                   ' FI                          NUMBER OF      '.
006310   03  WS-A-HD-3.
006320     05  FILLER                   PIC X(44)      VALUE
006330                   '                             BILLING    IN E'.
006340     05  FILLER                   PIC X(44)      VALUE
006350                   'RROR    ERROR      BRANCH  INCORRECTLY  REQU'.
006360     05  FILLER                   PIC X(44)      VALUE
006370                   'EST     TOTAL     ACCURACY  BANK CODES      '.
006380   03  WS-A-HD-4.
006390     05  FILLER                   PIC X(44)      VALUE
006400                   '                  TOTALS       (01)       (0'.
006410     05  FILLER                   PIC X(44)      VALUE
006420                   '2)       (03)       (06)       (08)       (0'.
006430     05  FILLER                   PIC X(44)      VALUE
006440                   '9)      ERRORS     PERCENT  WITH ERRORS     '.
006450   03  WS-B-HD-1.
006460     05  FILLER                   PIC X(32)      VALUE
006470                   ' BANK CODE    #CRS-TRANS-ORDERS '.
006480     05  FILLER        PIC X(54) VALUE
006490         'ERROR CODES(S)      FI NAME                           '.
006500     05  FILLER        PIC X(41) VALUE
006510          'REP-BASE-DELIVERY     UPCHARGE      GROSS'.
006520   03  WS-D-HD-1.
006530     05  FILLER                   PIC X(50)      VALUE
006540          '   PRODUCT CODE     DESCRIPTION                   '.
006550     05  FILLER        PIC X(50) VALUE
006560          'BANK CODE     FI NAME                             '.
006570     05  FILLER        PIC X(32) VALUE
006580          '   REPORT BASE  REPORT DELIVERY'.
006590   03  WS-END-OF-REPORT.
006600     05  FILLER                   PIC X(42)      VALUE SPACE.
006610     05  FILLER                   PIC X(47)      VALUE
006620                '= = = = = = = =  END OF REPORT  = = = = = = = ='.
006630     05  FILLER                   PIC X(46)      VALUE SPACE.
006640*
006650 01  WS-NON-TBL-RPT-DATA.
006660   03  WS-WIRE-COUNT              PIC S9(7)      COMP-3 VALUE +0.
006670*
006680 01  WS-WORK-FIELDS.
006690   03  WS-WORK-PERCENT            PIC S9(3)V9(2) COMP-3.
006700   03  WS-WORK-RATIO REDEFINES WS-WORK-PERCENT
006710                                  PIC S9V9(4)    COMP-3.
006720   03  WS-WORK-S9                 PIC S9(9)      COMP-3.
006730*
006740 01  WS-TAPE-CODE-INFO.
006750   03  WS-T-C-TAPE-CODE           PIC X(3).
006760   03  WS-T-C-HIT-FLAG            PIC X.
006770   03  WS-T-C-MED-SUBSCR          PIC S9         COMP.
006780   03  WS-T-C-DUE-DLX             PIC S9(9)V9(2) COMP-3.
006790*
006800 01  WS-BC-ERR-REC.
006810   03  WS-BC-ERR-REC-BC           PIC X(8).
006820   03  WS-BC-ERR-REC-01-FLAG      PIC X.
006830   03  WS-BC-ERR-REC-02-FLAG      PIC X.
006840   03  WS-BC-ERR-REC-03-FLAG      PIC X.
006850   03  WS-BC-ERR-REC-06-FLAG      PIC X.
006860   03  WS-BC-ERR-REC-08-FLAG      PIC X.
006870   03  WS-BC-ERR-REC-09-FLAG      PIC X.
006880   03  WS-BC-ERR-REC-COUNT        PIC 9(6)      COMP-3.
006890   03  WS-BC-ERR-ORD-REC-COUNT    PIC 9(6)      COMP-3.
006900   03  WS-BC-ERR-REC-BASE-DEL     PIC S9(7)V9(2)
006910                                            COMP-3 VALUE ZEROES.
006920   03  WS-BC-ERR-REC-UPCHARGE     PIC S9(7)V9(2)
006930                                            COMP-3 VALUE ZEROES.
006940   03  WS-BC-ERR-REC-GROSS        PIC S9(7)V9(2)
006950                                            COMP-3 VALUE ZEROES.
006960 01  WS-BC-ERR1-REC.
006970   05  WS-BC-ERR1-COMB.
006980     07  WS-BC-ERR1-REC-BC        PIC X(8).
006990     07  WS-BC-ERR1-CODE          PIC X(2).
007000   05  WS-BC-ERR1-REC-COUNT       PIC 9(6)  COMP-3 VALUE ZEROES.
007010   05  WS-BC-ERR1-ORD-REC-COUNT   PIC 9(6)  COMP-3 VALUE ZEROES.
007020   03  WS-BC-ERR1-REC-BASE-DEL    PIC S9(7)V9(2)
007030                                            COMP-3 VALUE ZEROES.
007040   03  WS-BC-ERR1-REC-UPCHARGE    PIC S9(7)V9(2)
007050                                            COMP-3 VALUE ZEROES.
007060   03  WS-BC-ERR1-REC-GROSS       PIC S9(7)V9(2)
007070                                            COMP-3 VALUE ZEROES.
007080 01  WS-BC-REST-REIM-REC.
007090   05  WS-BC-REST-REIM-BC         PIC X(8).
007100   05  WS-BC-REST-REIM-PROD-ID    PIC X(18)       VALUE SPACES.
007110   05  WS-BC-REST-REIM-REP-BASE   PIC S9(7)V9(2)  VALUE ZEROES.
007120   05  WS-BC-REST-REIM-REP-DEL    PIC S9(5)V9(2)  VALUE ZEROES.
007130 01  WS-BC-FLAGS.
007140   03  WS-BC-NON-WEEKLY-FLAG      PIC X.
007150   03  WS-BC-MEDIUM-FLAGS.
007160     05  WS-BC-MEDIUM-FLAG        OCCURS 4 TIMES
007170                                  PIC X.
007180   03  WS-BC-INVOICE-FLAGS.
007190     05  WS-BC-INVOICE-FLAG       OCCURS 2 TIMES
007200                                  PIC X.
007210   03  WS-BC-DEBIT-FLAGS.
007220     05  WS-BC-DEBIT-FLAG         OCCURS 2 TIMES
007230                                  PIC X.
007240 01  WS-BR-MEDIUM-FLAGS.
007250   03  WS-BR-MEDIUM-FLAG          OCCURS 4 TIMES
007260                                  PIC X.
007270*
007280 01  WS-MEDIUM-TABLE.
007290   03  WS-MED-TBL-MEDIUM          OCCURS 5 TIMES
007300*                * 1:ACH, 2:(UNUSED), 3:WIRE, 4:PAPER, 5:TOT  *
007310                                  INDEXED BY IX-WS-MED-TBL-MED.
007320     05  WS-MED-TBL-BC-COUNT      PIC S9(5)      COMP-3.
007330     05  WS-MED-TBL-BR-COUNT      PIC S9(7)      COMP-3.
007340     05  WS-MED-TBL-ABS-DLRS      PIC S9(9)V9(2) COMP-3.
007350     05  WS-MED-TBL-DUE-DELUXE    PIC S9(9)V9(2) COMP-3.
007360     05  WS-MED-TBL-ABS-UPCH      PIC S9(9)V9(2) COMP-3.
007370     05  WS-MED-TBL-TRAN-COUNT    OCCURS 7 TIMES
007380*                * 1:RSL DR, 2:WVE DR, 3:EXP DR, 4:RSL CR,
007390*                  5:WVE CR, 6:EXP CR, 7:TOTALS *
007400                                  INDEXED BY IX-WS-MED-TBL-TRN
007410                                  PIC S9(9)      COMP-3.
007420*
007430 01  WS-INVOICE-TABLE.
007440   03  WS-INV-TBL-INV-TYPE        OCCURS 1 TIMES.
007450*                * 1:PAYABLE *
007460     05  WS-INV-TBL-INV-BC-COUNT  PIC S9(5)      COMP-3.
007470     05  WS-INV-TBL-DEB-BC-COUNT  PIC S9(5)      COMP-3.
007480*
007490 01  WS-ACCURACY-FIELDS.
007500     05  WS-ACCURACY-TRAN-COUNT   PIC S9(9)      COMP-3.
007510     05  WS-ACCURACY-ERR-01-COUNT PIC S9(6)      COMP-3.
007520     05  WS-ACCURACY-ERR-02-COUNT PIC S9(6)      COMP-3.
007530     05  WS-ACCURACY-ERR-03-COUNT PIC S9(6)      COMP-3.
007540     05  WS-ACCURACY-ERR-06-COUNT PIC S9(6)      COMP-3.
007550     05  WS-ACCURACY-ERR-08-COUNT PIC S9(6)      COMP-3.
007560     05  WS-ACCURACY-ERR-09-COUNT PIC S9(6)      COMP-3.
007570     05  WS-ACCURACY-TOTAL-ERR-CT PIC S9(6)      COMP-3.
007580     05  WS-ACCURACY-ERR-BC-COUNT PIC S9(6)      COMP-3.
007590 01  WS-ACCURACY-ORDER-FIELDS.
007600     05  WS-ACCURACY-ORDER-COUNT    PIC S9(9)      COMP-3.
007610     05  WS-ACCURACY-ORDER-01-COUNT PIC S9(6)      COMP-3.
007620     05  WS-ACCURACY-ORDER-02-COUNT PIC S9(6)      COMP-3.
007630     05  WS-ACCURACY-ORDER-03-COUNT PIC S9(6)      COMP-3.
007640     05  WS-ACCURACY-ORDER-06-COUNT PIC S9(6)      COMP-3.
007650     05  WS-ACCURACY-ORDER-08-COUNT PIC S9(6)      COMP-3.
007660     05  WS-ACCURACY-ORDER-09-COUNT PIC S9(6)      COMP-3.
007670     05  WS-ACCURACY-TOTAL-ORDER-CT PIC S9(6)      COMP-3.
007680     05  WS-ACCURACY-ORDER-BC-COUNT PIC S9(6)      COMP-3.
007690*
007700 01  WS-TAPE-CODE-TABLE.
007710   03  WS-T-C-TBL-ENTRY-COUNT     PIC S9(4)      COMP-3.
007720   03  WS-T-C-TBL-ENTRY           OCCURS 0 TO 1000 TIMES
007730                                  DEPENDING ON
007740                                    WS-T-C-TBL-ENTRY-COUNT
007750                                  ASCENDING KEY IS
007760                                    WS-T-C-TBL-TAPE-CODE
007770                                  INDEXED BY IX-WS-T-C-TBL.
007780     05  WS-T-C-TBL-TAPE-CODE     PIC X(3).
007790     05  WS-T-C-TBL-HIT-FLAG      PIC X.
007800     05  WS-T-C-TBL-MED-SUBSCR    PIC S9         COMP.
007810     05  WS-T-C-TBL-DUE-DLX       PIC S9(9)V9(2) COMP-3.
007820*
007830 01  WS-BC-ERROR-TABLE.
007840   03  WS-BC-ERR-TBL-REC          OCCURS 1000 TIMES
007850                                  INDEXED BY
007860                                    IX-WS-BC-ERR-TBL-IN
007870                                    IX-WS-BC-ERR-TBL-OUT
007880                                  PIC X(42).
007890 01  WS-BC-REST-REIM-TABLE.
007900   03  WS-BC-REST-REIM-TBL-REC OCCURS 10000 TIMES
007910                                  INDEXED BY
007920                                    IX-WS-BC-REST-REIM-TBL-IN
007930                                    IX-WS-BC-REST-REIM-TBL-OUT.
007940     05 WS-BC-REST-REIM-TBL-BC           PIC X(8).
007950     05 WS-BC-REST-REIM-TBL-PROD-ID      PIC X(18).
007960     05 WS-BC-REST-REIM-TBL-REP-BASE     PIC S9(7)V9(2)
007970                                                  VALUE ZEROES.
007980     05 WS-BC-REST-REIM-TBL-REP-DEL      PIC S9(5)V9(2)
007990                                                  VALUE ZEROES.
008000 01  WS-BC-ERROR1-TABLE.
008010   03  WS-BC-ERR1-TBL-ENT-COUNT          PIC S9(5) COMP-3
008020                                                  VALUE ZEROES.
008030   03  WS-BC-ERR1-TBL-REC         OCCURS 10000 TIMES
008040                                  DEPENDING ON
008050                                    WS-BC-ERR1-TBL-ENT-COUNT
008060                                  ASCENDING KEY IS
008070                                    WS-BC-ERR1-TBL-COMB
008080                                  INDEXED BY
008090                                    IX-WS-BC-ERR1-TBL-IN
008100                                    IX-WS-BC-ERR1-TBL-OUT.
008110     05  WS-BC-ERR1-TBL-COMB.
008120       07 WS-BC-ERR1-TBL-REC-BC          PIC X(8).
008130       07 WS-BC-ERR1-TBL-CODE            PIC X(2).
008140     05  WS-BC-ERR1-TBL-REC-COUNT        PIC S9(6)       COMP-3
008150                                                  VALUE ZEROES.
008160     05  WS-BC-ERR1-TBL-ORD-REC-COUNT    PIC S9(6) COMP-3
008170                                                  VALUE ZEROES.
008180     05  WS-BC-ERR1-TBL-REC-BASE-DEL    PIC S9(7)V9(2)   COMP-3
008190                                                  VALUE ZEROES.
008200     05  WS-BC-ERR1-TBL-REC-UPCHARGE    PIC S9(7)V9(2)   COMP-3
008210                                                  VALUE ZEROES.
008220     05  WS-BC-ERR1-TBL-REC-GROSS       PIC S9(7)V9(2)   COMP-3
008230                                                  VALUE ZEROES.
008240*
008250 01  WS-CLOSING-DISPLAYS.
008260   03  WS-RETURN-CODE-LINE.
008270     05  FILLER                  PIC X(14) VALUE 'RETURN CODE = '.
008280     05  WS-RETURN-CODE           PIC 9(2)       DISPLAY VALUE 00.
008290     05  FILLER                   PIC X          VALUE '.'.
008300   03  WS-CD-I545-LINE.
008310     05  FILLER                   PIC X(8)       VALUE 'I545:'.
008320     05  WS-CD-I545-REC-CT        PIC ZZZ,ZZZ,ZZ9.
008330     05  FILLER              PIC X(18) VALUE ' RECORDS READ     '.
008340     05  WS-CD-I545-BYPASS-CT     PIC ZZ,ZZ9.
008350     05  FILLER                   PIC X(33)      VALUE
008360                              ' BYPASSED FOR NON-WEEKLY FLAG "1"'.
008370   03  WS-CD-IOWRK-LINE.
008380     05  FILLER                   PIC X(8)       VALUE 'IOWRK:'.
008390     05  WS-CD-IOWRK-REC-CT       PIC ZZZ,ZZZ,ZZ9.
008400     05  FILLER                   PIC X(9)      VALUE ' RECORDS '.
008410     05  WS-CD-IOWRK-VERB         PIC X(7)       VALUE 'WRITTEN'.
008420   03  WS-CD-I575-LINE.
008430     05  FILLER                   PIC X(8)       VALUE 'I575:'.
008440     05  WS-CD-I575-REC-CT        PIC ZZZ,ZZZ,ZZ9.
008450     05  FILLER              PIC X(18) VALUE ' RECORDS READ     '.
008460     05  WS-CD-I575-BYPASS-CT     PIC ZZ,ZZ9.
008470     05  FILLER                   PIC X(47)      VALUE
008480                ' RECORDS BYPASSED (RECORD TYPES OTHER THAN "A")'.
008490   03  WS-CD-I8929P-LINE.
008500     05  FILLER                   PIC X(8)      VALUE 'I8929P:'.
008510     05  WS-CD-I8929P-REC-CT        PIC ZZZ,ZZZ,ZZ9.
008520     05  FILLER              PIC X(18) VALUE ' RECORDS READ     '.
008530   03  WS-CD-I8929I-LINE.
008540     05  FILLER                   PIC X(8)      VALUE 'I8929I:'.
008550     05  WS-CD-I8929I-REC-CT        PIC ZZZ,ZZZ,ZZ9.
008560     05  FILLER              PIC X(18) VALUE ' RECORDS READ     '.
008570   03  WS-CD-I8929E-LINE.
008580     05  FILLER                   PIC X(8)      VALUE 'I8929E:'.
008590     05  WS-CD-I8929E-REC-CT        PIC ZZZ,ZZZ,ZZ9.
008600     05  FILLER              PIC X(18) VALUE ' RECORDS READ     '.
008610*
008620 COPY CPY108.
008630*
008640 01  WS-DISPLAY-PARM.
008650   03  WS-DP-CAPTION             PIC X(14) VALUE 'PARM OPTIONS: '.
008660   03  FILLER                     PIC X          VALUE '"'.
008670   03  WS-DP-STATEMENT            PIC X(20)      VALUE SPACE.
008680   03  FILLER                     PIC X          VALUE '"'.
008690*
008700 COPY CPY004.
008710*
008720 01  WS-PROGRAM-NUMBER            PIC X(8)       VALUE 'BIL02917'.
008730 01  WS-REVISION-DATE             PIC X(8)       VALUE '03-14-11'.
008740*
DLXMIG* DLXMIG Changes start here
DLXMIG*LINKAGE SECTION.
DLXMIG* DLXMIG Changes end here
008780*
008790 01  LS-PARM-INFO.
008800   03  LS-PARM-LENGTH             PIC S9(4)      COMP.
008810   03  LS-PARM-DATA               PIC X(100).
008820*
DLXMIG* DLXMIG Changes start here
DLXMIG LINKAGE SECTION.
DLXMIG COPY AIXLNCPY.
DLXMIG* DLXMIG Changes end here
DLXMIG* DLXMIG Changes start here
DLXMIG*PROCEDURE DIVISION USING LS-PARM-INFO.
DLXMIG PROCEDURE DIVISION USING BY VALUE PARMCNT
DLXMIG           BY REFERENCE OS-PARM.
DLXMIG COPY AIXPRCPY.
DLXMIG* DLXMIG Changes end here
008930*
DLXMIG 0000-SQL-DB-CONNECT.
DLXMIG     MOVE 'DLXVSAM' TO DBNAME.
DLXMIG COPY CPYMIGUD.
008970   0000-MAIN-LINE.
008980     PERFORM 8000-COMMON-INIT.
008990     IF PASS-1
009000       PERFORM 7000-PASS-1
009010     ELSE
009020       PERFORM 1000-PASS-2.
009030     PERFORM 9000-COMMON-CLOSE.
009040     STOP RUN.
009050*
009060   1000-PASS-2.
009070     PERFORM 8200-PASS-2-INIT.
009080     PERFORM 6000-LOAD-TAPE-CODE-TBL.
009090     PERFORM 4000-GATHER-DATA.
009100     PERFORM 2000-PRINT-REPORT.
009110     PERFORM 9200-PASS-2-CLOSE.
009120*
009130   2000-PRINT-REPORT.
009140     PERFORM 2800-PRINT-HEADING-1.
009150     PERFORM 2100-PRT-MEDIUM-RECAP.
009160     PERFORM 2200-PRT-DOLLAR-RECAP.
009170     PERFORM 2300-PRT-TRANS-RECAP.
009180     PERFORM 2400-PRT-INVOICE-RECAP.
009190     PERFORM 2450-PRT-ACH-RECAP.
009200     PERFORM 2800-PRINT-HEADING-1.
009210     PERFORM 2500-PRT-ACCURACY-RECAP.
009220     PERFORM 3000-PRT-BCS-W-ERRORS.
009230     PERFORM 3500-PRT-BCS-CODE-W-ERRORS.
009240     PERFORM 3800-PRT-BCS-REST-REIM-RECAP.
009250     PERFORM 2900-FOOT-PRINTER1.
009260*
009270   2100-PRT-MEDIUM-RECAP.
009280     MOVE 'BILLING MEDIUM RECAP:' TO PRINTER1-REC (1:21).
009290     MOVE SPACE TO PRINTER1-REC (22:111).
009300     MOVE +3 TO WS-LINE-SPACER.
009310     PERFORM 2600-WRITE-PRINTER1-REC.
009320     MOVE ALL '_' TO PRINTER1-REC (1:21).
009330     MOVE SPACE TO PRINTER1-REC (22:111).
DLXMIG*    WRITE PRINTER1-REC AFTER NO-ADVANCE.
DLXMIG     WRITE PRINTER1-REC. 
009360     MOVE +2 TO WS-LINE-SPACER.
009370     MOVE WS-M-HD-A1 TO PRINTER1-REC.
009380     PERFORM 2600-WRITE-PRINTER1-REC.
009390     MOVE +1 TO WS-LINE-SPACER.
009400     MOVE WS-M-HD-2 TO PRINTER1-REC.
009410     PERFORM 2600-WRITE-PRINTER1-REC.
009420*
009430     MOVE SPACE TO PRINTER1-REC.
009440     MOVE 'ACH      ' TO PR1-M-CAPT-1 PR1-M-CAPT-2 PR1-M-CAPT-3.
009450     MOVE WS-MED-TBL-TRAN-COUNT (1, 7) TO PR1-M-NBR-1.
009460     IF WS-MED-TBL-TRAN-COUNT (5, 7) NOT EQUAL +0
009470       DIVIDE WS-MED-TBL-TRAN-COUNT (1, 7)
009480             BY WS-MED-TBL-TRAN-COUNT (5, 7)
009490             GIVING WS-WORK-RATIO ROUNDED
009500       MOVE WS-WORK-PERCENT TO PR1-M-PCT-1.
009510     MOVE WS-MED-TBL-BC-COUNT (1) TO PR1-M-NBR-2.
009520     IF WS-MED-TBL-BC-COUNT (5) NOT EQUAL +0
009530       DIVIDE WS-MED-TBL-BC-COUNT (1)
009540             BY WS-MED-TBL-BC-COUNT (5)
009550             GIVING WS-WORK-RATIO ROUNDED
009560       MOVE WS-WORK-PERCENT TO PR1-M-PCT-2.
009570     MOVE WS-MED-TBL-BR-COUNT (1) TO PR1-M-NBR-3.
009580     IF WS-MED-TBL-BR-COUNT (5) NOT EQUAL +0
009590       DIVIDE WS-MED-TBL-BR-COUNT (1)
009600             BY WS-MED-TBL-BR-COUNT (5)
009610             GIVING WS-WORK-RATIO ROUNDED
009620       MOVE WS-WORK-PERCENT TO PR1-M-PCT-3.
009630     PERFORM 2600-WRITE-PRINTER1-REC.
009640*
009650     MOVE SPACE TO PRINTER1-REC.
009660     MOVE 'WIRE     ' TO PR1-M-CAPT-1 PR1-M-CAPT-2 PR1-M-CAPT-3.
009670     MOVE WS-MED-TBL-TRAN-COUNT (3, 7) TO PR1-M-NBR-1.
009680     IF WS-MED-TBL-TRAN-COUNT (5, 7) NOT EQUAL +0
009690       DIVIDE WS-MED-TBL-TRAN-COUNT (3, 7)
009700             BY WS-MED-TBL-TRAN-COUNT (5, 7)
009710             GIVING WS-WORK-RATIO ROUNDED
009720       MOVE WS-WORK-PERCENT TO PR1-M-PCT-1.
009730     MOVE WS-MED-TBL-BC-COUNT (3) TO PR1-M-NBR-2.
009740     IF WS-MED-TBL-BC-COUNT (5) NOT EQUAL +0
009750       DIVIDE WS-MED-TBL-BC-COUNT (3)
009760             BY WS-MED-TBL-BC-COUNT (5)
009770             GIVING WS-WORK-RATIO ROUNDED
009780       MOVE WS-WORK-PERCENT TO PR1-M-PCT-2.
009790     MOVE WS-MED-TBL-BR-COUNT (3) TO PR1-M-NBR-3.
009800     IF WS-MED-TBL-BR-COUNT (5) NOT EQUAL +0
009810       DIVIDE WS-MED-TBL-BR-COUNT (3)
009820             BY WS-MED-TBL-BR-COUNT (5)
009830             GIVING WS-WORK-RATIO ROUNDED
009840       MOVE WS-WORK-PERCENT TO PR1-M-PCT-3.
009850     PERFORM 2600-WRITE-PRINTER1-REC.
009860*
009870     MOVE SPACE TO PRINTER1-REC.
009880     MOVE 'PAPER    ' TO PR1-M-CAPT-1 PR1-M-CAPT-2 PR1-M-CAPT-3.
009890     MOVE WS-MED-TBL-TRAN-COUNT (4, 7) TO PR1-M-NBR-1.
009900     IF WS-MED-TBL-TRAN-COUNT (5, 7) NOT EQUAL +0
009910       DIVIDE WS-MED-TBL-TRAN-COUNT (4, 7)
009920             BY WS-MED-TBL-TRAN-COUNT (5, 7)
009930             GIVING WS-WORK-RATIO ROUNDED
009940       MOVE WS-WORK-PERCENT TO PR1-M-PCT-1.
009950     MOVE WS-MED-TBL-BC-COUNT (4) TO PR1-M-NBR-2.
009960     IF WS-MED-TBL-BC-COUNT (5) NOT EQUAL +0
009970       DIVIDE WS-MED-TBL-BC-COUNT (4)
009980             BY WS-MED-TBL-BC-COUNT (5)
009990             GIVING WS-WORK-RATIO ROUNDED
010000       MOVE WS-WORK-PERCENT TO PR1-M-PCT-2.
010010     MOVE WS-MED-TBL-BR-COUNT (4) TO PR1-M-NBR-3.
010020     IF WS-MED-TBL-BR-COUNT (5) NOT EQUAL +0
010030       DIVIDE WS-MED-TBL-BR-COUNT (4)
010040             BY WS-MED-TBL-BR-COUNT (5)
010050             GIVING WS-WORK-RATIO ROUNDED
010060       MOVE WS-WORK-PERCENT TO PR1-M-PCT-3.
010070     PERFORM 2600-WRITE-PRINTER1-REC.
010080*
010090     MOVE SPACE TO PRINTER1-REC.
010100     MOVE '* TOTAL *' TO PR1-M-CAPT-1 PR1-M-CAPT-2 PR1-M-CAPT-3.
010110     MOVE WS-MED-TBL-TRAN-COUNT (5, 7) TO PR1-M-NBR-1.
010120     MOVE WS-MED-TBL-BC-COUNT (5) TO PR1-M-NBR-2.
010130     MOVE WS-MED-TBL-BR-COUNT (5) TO PR1-M-NBR-3.
010140     PERFORM 2600-WRITE-PRINTER1-REC.
010150*
010160     MOVE WS-WIRE-COUNT TO WS-M-FT-A-WIRE-CT.
010170     MOVE WS-M-FT-A TO PRINTER1-REC.
010180     MOVE +2 TO WS-LINE-SPACER.
010190     PERFORM 2600-WRITE-PRINTER1-REC.
010200*
010210   2200-PRT-DOLLAR-RECAP.
010220     MOVE 'DOLLAR BILLING RECAP:' TO PRINTER1-REC (1:21).
010230     MOVE SPACE TO PRINTER1-REC (22:111).
010240     MOVE +3 TO WS-LINE-SPACER.
010250     PERFORM 2600-WRITE-PRINTER1-REC.
010260     MOVE ALL '_' TO PRINTER1-REC (1:21).
010270     MOVE SPACE TO PRINTER1-REC (22:111).
DLXMIG*    WRITE PRINTER1-REC AFTER NO-ADVANCE.
DLXMIG     WRITE PRINTER1-REC. 
010300     MOVE +2 TO WS-LINE-SPACER.
010310     MOVE WS-M-HD-B1 TO PRINTER1-REC.
010320     PERFORM 2600-WRITE-PRINTER1-REC.
010330     MOVE +1 TO WS-LINE-SPACER.
010340     MOVE WS-M-HD-B2 TO PRINTER1-REC.
010350     PERFORM 2600-WRITE-PRINTER1-REC.
010360*
010370     MOVE SPACE TO PRINTER1-REC.
010380     MOVE 'ACH      ' TO PR1-M-CAPT-1 PR1-M-CAPT-2 PR1-M-CAPT-3.
010390     MOVE WS-MED-TBL-ABS-DLRS (1) TO PR1-M-DLRS-1.
010400     IF WS-MED-TBL-ABS-DLRS (5) NOT EQUAL +0
010410       DIVIDE WS-MED-TBL-ABS-DLRS (1)
010420             BY WS-MED-TBL-ABS-DLRS (5)
010430             GIVING WS-WORK-RATIO ROUNDED
010440       MOVE WS-WORK-PERCENT TO PR1-M-PCT-1.
010450     MOVE WS-MED-TBL-DUE-DELUXE (1) TO PR1-M-DLRS-2.
010460     IF WS-MED-TBL-DUE-DELUXE (5) NOT EQUAL +0
010470       DIVIDE WS-MED-TBL-DUE-DELUXE (1)
010480             BY WS-MED-TBL-DUE-DELUXE (5)
010490             GIVING WS-WORK-RATIO ROUNDED
010500       MOVE WS-WORK-PERCENT TO PR1-M-PCT-2.
010510     MOVE WS-MED-TBL-ABS-UPCH (1) TO PR1-M-DLRS-3.
010520     IF WS-MED-TBL-ABS-UPCH (5) NOT EQUAL +0
010530       DIVIDE WS-MED-TBL-ABS-UPCH (1)
010540             BY WS-MED-TBL-ABS-UPCH (5)
010550             GIVING WS-WORK-RATIO ROUNDED
010560       MOVE WS-WORK-PERCENT TO PR1-M-PCT-3.
010570     PERFORM 2600-WRITE-PRINTER1-REC.
010580*
010590     MOVE SPACE TO PRINTER1-REC.
010600     MOVE 'WIRE     ' TO PR1-M-CAPT-1 PR1-M-CAPT-2 PR1-M-CAPT-3.
010610     MOVE WS-MED-TBL-ABS-DLRS (3) TO PR1-M-DLRS-1.
010620     IF WS-MED-TBL-ABS-DLRS (5) NOT EQUAL +0
010630       DIVIDE WS-MED-TBL-ABS-DLRS (3)
010640             BY WS-MED-TBL-ABS-DLRS (5)
010650             GIVING WS-WORK-RATIO ROUNDED
010660       MOVE WS-WORK-PERCENT TO PR1-M-PCT-1.
010670     MOVE WS-MED-TBL-DUE-DELUXE (3) TO PR1-M-DLRS-2.
010680     IF WS-MED-TBL-DUE-DELUXE (5) NOT EQUAL +0
010690       DIVIDE WS-MED-TBL-DUE-DELUXE (3)
010700             BY WS-MED-TBL-DUE-DELUXE (5)
010710             GIVING WS-WORK-RATIO ROUNDED
010720       MOVE WS-WORK-PERCENT TO PR1-M-PCT-2.
010730     MOVE WS-MED-TBL-ABS-UPCH (3) TO PR1-M-DLRS-3.
010740     IF WS-MED-TBL-ABS-UPCH (5) NOT EQUAL +0
010750       DIVIDE WS-MED-TBL-ABS-UPCH (3)
010760             BY WS-MED-TBL-ABS-UPCH (5)
010770             GIVING WS-WORK-RATIO ROUNDED
010780       MOVE WS-WORK-PERCENT TO PR1-M-PCT-3.
010790     PERFORM 2600-WRITE-PRINTER1-REC.
010800*
010810     MOVE SPACE TO PRINTER1-REC.
010820     MOVE 'PAPER    ' TO PR1-M-CAPT-1 PR1-M-CAPT-2 PR1-M-CAPT-3.
010830     MOVE WS-MED-TBL-ABS-DLRS (4) TO PR1-M-DLRS-1.
010840     IF WS-MED-TBL-ABS-DLRS (5) NOT EQUAL +0
010850       DIVIDE WS-MED-TBL-ABS-DLRS (4)
010860             BY WS-MED-TBL-ABS-DLRS (5)
010870             GIVING WS-WORK-RATIO ROUNDED
010880       MOVE WS-WORK-PERCENT TO PR1-M-PCT-1.
010890     MOVE WS-MED-TBL-DUE-DELUXE (4) TO PR1-M-DLRS-2.
010900     IF WS-MED-TBL-DUE-DELUXE (5) NOT EQUAL +0
010910       DIVIDE WS-MED-TBL-DUE-DELUXE (4)
010920             BY WS-MED-TBL-DUE-DELUXE (5)
010930             GIVING WS-WORK-RATIO ROUNDED
010940       MOVE WS-WORK-PERCENT TO PR1-M-PCT-2.
010950     MOVE WS-MED-TBL-ABS-UPCH (4) TO PR1-M-DLRS-3.
010960     IF WS-MED-TBL-ABS-UPCH (5) NOT EQUAL +0
010970       DIVIDE WS-MED-TBL-ABS-UPCH (4)
010980             BY WS-MED-TBL-ABS-UPCH (5)
010990             GIVING WS-WORK-RATIO ROUNDED
011000       MOVE WS-WORK-PERCENT TO PR1-M-PCT-3.
011010     PERFORM 2600-WRITE-PRINTER1-REC.
011020*
011030     MOVE SPACE TO PRINTER1-REC.
011040     MOVE '* TOTAL *' TO PR1-M-CAPT-1 PR1-M-CAPT-2 PR1-M-CAPT-3.
011050     MOVE WS-MED-TBL-ABS-DLRS (5) TO PR1-M-DLRS-1.
011060     MOVE WS-MED-TBL-DUE-DELUXE (5) TO PR1-M-DLRS-2.
011070     MOVE WS-MED-TBL-ABS-UPCH (5) TO PR1-M-DLRS-3.
011080     PERFORM 2600-WRITE-PRINTER1-REC.
011090*
011100   2300-PRT-TRANS-RECAP.
011110     MOVE 'TRANSACTION RECAP:' TO PRINTER1-REC (1:18).
011120     MOVE SPACE TO PRINTER1-REC (19:114).
011130     MOVE +3 TO WS-LINE-SPACER.
011140     PERFORM 2600-WRITE-PRINTER1-REC.
011150     MOVE ALL '_' TO PRINTER1-REC (1:18).
011160     MOVE SPACE TO PRINTER1-REC (19:114).
DLXMIG*    WRITE PRINTER1-REC AFTER NO-ADVANCE.
DLXMIG     WRITE PRINTER1-REC. 
011190     MOVE +2 TO WS-LINE-SPACER.
011200     MOVE WS-M-HD-C1 TO PRINTER1-REC.
011210     PERFORM 2600-WRITE-PRINTER1-REC.
011220     MOVE +1 TO WS-LINE-SPACER.
011230     MOVE WS-M-HD-2 TO PRINTER1-REC.
011240     PERFORM 2600-WRITE-PRINTER1-REC.
011250*
011260     MOVE SPACE TO PRINTER1-REC.
011270     MOVE 'ACH      ' TO PR1-M-CAPT-1 PR1-M-CAPT-2 PR1-M-CAPT-3.
011280     MOVE WS-MED-TBL-TRAN-COUNT (1, 1) TO PR1-M-NBR-1.
011290     IF WS-MED-TBL-TRAN-COUNT (5, 1) NOT EQUAL +0
011300       DIVIDE WS-MED-TBL-TRAN-COUNT (1, 1)
011310             BY WS-MED-TBL-TRAN-COUNT (5, 1) GIVING
011320             WS-WORK-RATIO ROUNDED
011330       MOVE WS-WORK-PERCENT TO PR1-M-PCT-1.
011340     MOVE WS-MED-TBL-TRAN-COUNT (1, 2) TO PR1-M-NBR-2.
011350     IF WS-MED-TBL-TRAN-COUNT (5, 2) NOT EQUAL +0
011360       DIVIDE WS-MED-TBL-TRAN-COUNT (1, 2)
011370             BY WS-MED-TBL-TRAN-COUNT (5, 2) GIVING
011380             WS-WORK-RATIO ROUNDED
011390       MOVE WS-WORK-PERCENT TO PR1-M-PCT-2.
011400     MOVE WS-MED-TBL-TRAN-COUNT (1, 3) TO PR1-M-NBR-3.
011410     IF WS-MED-TBL-TRAN-COUNT (5, 3) NOT EQUAL +0
011420       DIVIDE WS-MED-TBL-TRAN-COUNT (1, 3)
011430             BY WS-MED-TBL-TRAN-COUNT (5, 3) GIVING
011440             WS-WORK-RATIO ROUNDED
011450       MOVE WS-WORK-PERCENT TO PR1-M-PCT-3.
011460     PERFORM 2600-WRITE-PRINTER1-REC.
011470*
011480     MOVE SPACE TO PRINTER1-REC.
011490     MOVE 'WIRE     ' TO PR1-M-CAPT-1 PR1-M-CAPT-2 PR1-M-CAPT-3.
011500     MOVE WS-MED-TBL-TRAN-COUNT (3, 1) TO PR1-M-NBR-1.
011510     IF WS-MED-TBL-TRAN-COUNT (5, 1) NOT EQUAL +0
011520       DIVIDE WS-MED-TBL-TRAN-COUNT (3, 1)
011530             BY WS-MED-TBL-TRAN-COUNT (5, 1) GIVING
011540             WS-WORK-RATIO ROUNDED
011550       MOVE WS-WORK-PERCENT TO PR1-M-PCT-1.
011560     MOVE WS-MED-TBL-TRAN-COUNT (3, 2) TO PR1-M-NBR-2.
011570     IF WS-MED-TBL-TRAN-COUNT (5, 2) NOT EQUAL +0
011580       DIVIDE WS-MED-TBL-TRAN-COUNT (3, 2)
011590             BY WS-MED-TBL-TRAN-COUNT (5, 2) GIVING
011600             WS-WORK-RATIO ROUNDED
011610       MOVE WS-WORK-PERCENT TO PR1-M-PCT-2.
011620     MOVE WS-MED-TBL-TRAN-COUNT (3, 3) TO PR1-M-NBR-3.
011630     IF WS-MED-TBL-TRAN-COUNT (5, 3) NOT EQUAL +0
011640       DIVIDE WS-MED-TBL-TRAN-COUNT (3, 3)
011650             BY WS-MED-TBL-TRAN-COUNT (5, 3) GIVING
011660             WS-WORK-RATIO ROUNDED
011670       MOVE WS-WORK-PERCENT TO PR1-M-PCT-3.
011680     PERFORM 2600-WRITE-PRINTER1-REC.
011690*
011700     MOVE SPACE TO PRINTER1-REC.
011710     MOVE 'PAPER    ' TO PR1-M-CAPT-1 PR1-M-CAPT-2 PR1-M-CAPT-3.
011720     MOVE WS-MED-TBL-TRAN-COUNT (4, 1) TO PR1-M-NBR-1.
011730     IF WS-MED-TBL-TRAN-COUNT (5, 1) NOT EQUAL +0
011740       DIVIDE WS-MED-TBL-TRAN-COUNT (4, 1)
011750             BY WS-MED-TBL-TRAN-COUNT (5, 1) GIVING
011760             WS-WORK-RATIO ROUNDED
011770       MOVE WS-WORK-PERCENT TO PR1-M-PCT-1.
011780     MOVE WS-MED-TBL-TRAN-COUNT (4, 2) TO PR1-M-NBR-2.
011790     IF WS-MED-TBL-TRAN-COUNT (5, 2) NOT EQUAL +0
011800       DIVIDE WS-MED-TBL-TRAN-COUNT (4, 2)
011810             BY WS-MED-TBL-TRAN-COUNT (5, 2) GIVING
011820             WS-WORK-RATIO ROUNDED
011830       MOVE WS-WORK-PERCENT TO PR1-M-PCT-2.
011840     MOVE WS-MED-TBL-TRAN-COUNT (4, 3) TO PR1-M-NBR-3.
011850     IF WS-MED-TBL-TRAN-COUNT (5, 3) NOT EQUAL +0
011860       DIVIDE WS-MED-TBL-TRAN-COUNT (4, 3)
011870             BY WS-MED-TBL-TRAN-COUNT (5, 3) GIVING
011880             WS-WORK-RATIO ROUNDED
011890       MOVE WS-WORK-PERCENT TO PR1-M-PCT-3.
011900     PERFORM 2600-WRITE-PRINTER1-REC.
011910*
011920     MOVE SPACE TO PRINTER1-REC.
011930     MOVE '* TOTAL *' TO PR1-M-CAPT-1 PR1-M-CAPT-2 PR1-M-CAPT-3.
011940     MOVE WS-MED-TBL-TRAN-COUNT (5, 1) TO PR1-M-NBR-1.
011950     MOVE WS-MED-TBL-TRAN-COUNT (5, 2) TO PR1-M-NBR-2.
011960     MOVE WS-MED-TBL-TRAN-COUNT (5, 3) TO PR1-M-NBR-3.
011970     PERFORM 2600-WRITE-PRINTER1-REC.
011980*
011990     MOVE +2 TO WS-LINE-SPACER.
012000     MOVE WS-M-HD-D1 TO PRINTER1-REC.
012010     PERFORM 2600-WRITE-PRINTER1-REC.
012020     MOVE +1 TO WS-LINE-SPACER.
012030     MOVE WS-M-HD-2 TO PRINTER1-REC.
012040     PERFORM 2600-WRITE-PRINTER1-REC.
012050*
012060     MOVE SPACE TO PRINTER1-REC.
012070     MOVE 'ACH      ' TO PR1-M-CAPT-1 PR1-M-CAPT-2 PR1-M-CAPT-3.
012080     MOVE WS-MED-TBL-TRAN-COUNT (1, 4) TO PR1-M-NBR-1.
012090     IF WS-MED-TBL-TRAN-COUNT (5, 4) NOT EQUAL +0
012100       DIVIDE WS-MED-TBL-TRAN-COUNT (1, 4)
012110             BY WS-MED-TBL-TRAN-COUNT (5, 4) GIVING
012120             WS-WORK-RATIO ROUNDED
012130       MOVE WS-WORK-PERCENT TO PR1-M-PCT-1.
012140     MOVE WS-MED-TBL-TRAN-COUNT (1, 5) TO PR1-M-NBR-2.
012150     IF WS-MED-TBL-TRAN-COUNT (5, 5) NOT EQUAL +0
012160       DIVIDE WS-MED-TBL-TRAN-COUNT (1, 5)
012170             BY WS-MED-TBL-TRAN-COUNT (5, 5) GIVING
012180             WS-WORK-RATIO ROUNDED
012190       MOVE WS-WORK-PERCENT TO PR1-M-PCT-2.
012200     MOVE WS-MED-TBL-TRAN-COUNT (1, 6) TO PR1-M-NBR-3.
012210     IF WS-MED-TBL-TRAN-COUNT (5, 6) NOT EQUAL +0
012220       DIVIDE WS-MED-TBL-TRAN-COUNT (1, 6)
012230             BY WS-MED-TBL-TRAN-COUNT (5, 6) GIVING
012240             WS-WORK-RATIO ROUNDED
012250       MOVE WS-WORK-PERCENT TO PR1-M-PCT-3.
012260     PERFORM 2600-WRITE-PRINTER1-REC.
012270*
012280     MOVE SPACE TO PRINTER1-REC.
012290     MOVE 'WIRE     ' TO PR1-M-CAPT-1 PR1-M-CAPT-2 PR1-M-CAPT-3.
012300     MOVE WS-MED-TBL-TRAN-COUNT (3, 4) TO PR1-M-NBR-1.
012310     IF WS-MED-TBL-TRAN-COUNT (5, 4) NOT EQUAL +0
012320       DIVIDE WS-MED-TBL-TRAN-COUNT (3, 4)
012330             BY WS-MED-TBL-TRAN-COUNT (5, 4) GIVING
012340             WS-WORK-RATIO ROUNDED
012350       MOVE WS-WORK-PERCENT TO PR1-M-PCT-1.
012360     MOVE WS-MED-TBL-TRAN-COUNT (3, 5) TO PR1-M-NBR-2.
012370     IF WS-MED-TBL-TRAN-COUNT (5, 5) NOT EQUAL +0
012380       DIVIDE WS-MED-TBL-TRAN-COUNT (3, 5)
012390             BY WS-MED-TBL-TRAN-COUNT (5, 5) GIVING
012400             WS-WORK-RATIO ROUNDED
012410       MOVE WS-WORK-PERCENT TO PR1-M-PCT-2.
012420     MOVE WS-MED-TBL-TRAN-COUNT (3, 6) TO PR1-M-NBR-3.
012430     IF WS-MED-TBL-TRAN-COUNT (5, 6) NOT EQUAL +0
012440       DIVIDE WS-MED-TBL-TRAN-COUNT (3, 6)
012450             BY WS-MED-TBL-TRAN-COUNT (5, 6) GIVING
012460             WS-WORK-RATIO ROUNDED
012470       MOVE WS-WORK-PERCENT TO PR1-M-PCT-3.
012480     PERFORM 2600-WRITE-PRINTER1-REC.
012490*
012500     MOVE SPACE TO PRINTER1-REC.
012510     MOVE 'PAPER    ' TO PR1-M-CAPT-1 PR1-M-CAPT-2 PR1-M-CAPT-3.
012520     MOVE WS-MED-TBL-TRAN-COUNT (4, 4) TO PR1-M-NBR-1.
012530     IF WS-MED-TBL-TRAN-COUNT (5, 4) NOT EQUAL +0
012540       DIVIDE WS-MED-TBL-TRAN-COUNT (4, 4)
012550             BY WS-MED-TBL-TRAN-COUNT (5, 4) GIVING
012560             WS-WORK-RATIO ROUNDED
012570       MOVE WS-WORK-PERCENT TO PR1-M-PCT-1.
012580     MOVE WS-MED-TBL-TRAN-COUNT (4, 5) TO PR1-M-NBR-2.
012590     IF WS-MED-TBL-TRAN-COUNT (5, 5) NOT EQUAL +0
012600       DIVIDE WS-MED-TBL-TRAN-COUNT (4, 5)
012610             BY WS-MED-TBL-TRAN-COUNT (5, 5) GIVING
012620             WS-WORK-RATIO ROUNDED
012630       MOVE WS-WORK-PERCENT TO PR1-M-PCT-2.
012640     MOVE WS-MED-TBL-TRAN-COUNT (4, 6) TO PR1-M-NBR-3.
012650     IF WS-MED-TBL-TRAN-COUNT (5, 6) NOT EQUAL +0
012660       DIVIDE WS-MED-TBL-TRAN-COUNT (4, 6)
012670             BY WS-MED-TBL-TRAN-COUNT (5, 6) GIVING
012680             WS-WORK-RATIO ROUNDED
012690       MOVE WS-WORK-PERCENT TO PR1-M-PCT-3.
012700     PERFORM 2600-WRITE-PRINTER1-REC.
012710*
012720     MOVE SPACE TO PRINTER1-REC.
012730     MOVE '* TOTAL *' TO PR1-M-CAPT-1 PR1-M-CAPT-2 PR1-M-CAPT-3.
012740     MOVE WS-MED-TBL-TRAN-COUNT (5, 4) TO PR1-M-NBR-1.
012750     MOVE WS-MED-TBL-TRAN-COUNT (5, 5) TO PR1-M-NBR-2.
012760     MOVE WS-MED-TBL-TRAN-COUNT (5, 6) TO PR1-M-NBR-3.
012770     PERFORM 2600-WRITE-PRINTER1-REC.
012780*
012790   2400-PRT-INVOICE-RECAP.
012800     MOVE 'INVOICING RECAP:' TO PRINTER1-REC (1:16).
012810     MOVE SPACE TO PRINTER1-REC (17:116).
012820     MOVE +3 TO WS-LINE-SPACER.
012830     PERFORM 2600-WRITE-PRINTER1-REC.
012840     MOVE ALL '_' TO PRINTER1-REC (1:16).
012850     MOVE SPACE TO PRINTER1-REC (17:116).
DLXMIG*    WRITE PRINTER1-REC AFTER NO-ADVANCE.
DLXMIG     WRITE PRINTER1-REC. 
012880     MOVE +2 TO WS-LINE-SPACER.
012890     MOVE WS-I-HD-1 TO PRINTER1-REC.
012900     PERFORM 2600-WRITE-PRINTER1-REC.
012910     MOVE +1 TO WS-LINE-SPACER.
012920     MOVE WS-I-HD-2 TO PRINTER1-REC.
012930     PERFORM 2600-WRITE-PRINTER1-REC.
012940*
012950     MOVE SPACE TO PRINTER1-REC.
012960     MOVE WS-I8929-PRINT-INV-CNT     TO PR1-I-CT-PRINT.
012970     MOVE WS-I8929-IMAGE-INV-CNT     TO PR1-I-CT-IMAGE.
012980     MOVE WS-I8929-DNM-INV-CNT       TO PR1-I-CT-DNM.
012990     MOVE WS-I8929-DSI-INV-CNT       TO PR1-I-CT-DSI.
013000     MOVE WS-I8929-EMAIL-INV-CNT     TO PR1-I-CT-EMAIL.
013010     MOVE WS-I8929-STUB-INV-CNT      TO PR1-I-CT-STUB.
013020     PERFORM 2600-WRITE-PRINTER1-REC.
013030*
013040   2450-PRT-ACH-RECAP.
013050     MOVE 'ACH DUE DELUXE RECAP:' TO PRINTER1-REC (1:21).
013060     MOVE SPACE TO PRINTER1-REC (22:111).
013070     MOVE +3 TO WS-LINE-SPACER.
013080     PERFORM 2600-WRITE-PRINTER1-REC.
013090     MOVE ALL '_' TO PRINTER1-REC (1:21).
013100     MOVE SPACE TO PRINTER1-REC (22:111).
013110     MOVE +1 TO WS-LINE-SPACER.
013120     PERFORM 2600-WRITE-PRINTER1-REC.
013130     MOVE +2 TO WS-LINE-SPACER.
013140     MOVE WS-E-HD TO PRINTER1-REC.
013150     PERFORM 2600-WRITE-PRINTER1-REC.
013160     SET IX-WS-T-C-TBL TO +1.
013170     PERFORM WS-T-C-TBL-ENTRY-COUNT TIMES
013180       MOVE WS-T-C-TBL-ENTRY (IX-WS-T-C-TBL) TO WS-TAPE-CODE-INFO
013190       IF WS-T-C-TAPE-CODE (1:1) EQUAL '0'
013200         MOVE SPACE TO PRINTER1-REC
013210         MOVE WS-T-C-TAPE-CODE TO PR1-E-TAPE-CODE
013220         MOVE WS-T-C-DUE-DLX TO PR1-E-DUE-DLX
013230         PERFORM 2600-WRITE-PRINTER1-REC   
013240         MOVE +1 TO WS-LINE-SPACER 
013250       END-IF
013260       SET IX-WS-T-C-TBL UP BY 1
013270     END-PERFORM.
013280     MOVE +2 TO WS-LINE-SPACER.
013290     MOVE SPACE TO PRINTER1-REC.
013300     PERFORM 2600-WRITE-PRINTER1-REC.
013310*
013320   2500-PRT-ACCURACY-RECAP.
013330     MOVE 'ACCURACY RECAP:' TO PRINTER1-REC (1:15).
013340     MOVE SPACE TO PRINTER1-REC (16:117).
013350     MOVE +3 TO WS-LINE-SPACER.
013360     PERFORM 2600-WRITE-PRINTER1-REC.
013370     MOVE ALL '_' TO PRINTER1-REC (1:15).
013380     MOVE SPACE TO PRINTER1-REC (16:117).
DLXMIG*    WRITE PRINTER1-REC AFTER NO-ADVANCE.
DLXMIG     WRITE PRINTER1-REC. 
013410     MOVE +2 TO WS-LINE-SPACER.
013420     MOVE WS-A-HD-1 TO PRINTER1-REC.
013430     PERFORM 2600-WRITE-PRINTER1-REC.
013440     MOVE +1 TO WS-LINE-SPACER.
013450     MOVE WS-A-HD-2 TO PRINTER1-REC.
013460     PERFORM 2600-WRITE-PRINTER1-REC.
013470     MOVE WS-A-HD-3 TO PRINTER1-REC.
013480     PERFORM 2600-WRITE-PRINTER1-REC.
013490     MOVE WS-A-HD-4 TO PRINTER1-REC.
013500     PERFORM 2600-WRITE-PRINTER1-REC.
013510*
013520     MOVE SPACE TO PRINTER1-REC.
013530     MOVE 'TRANSACTIONS' TO PR1-A-CAPT.
013540     MOVE WS-ACCURACY-TRAN-COUNT  TO PR1-A-TRN-CT.
013550     MOVE WS-ACCURACY-ERR-01-COUNT  TO PR1-A-ERR-01-CT.
013560     MOVE WS-ACCURACY-ERR-02-COUNT  TO PR1-A-ERR-02-CT.
013570     MOVE WS-ACCURACY-ERR-03-COUNT  TO PR1-A-ERR-03-CT.
013580     MOVE WS-ACCURACY-ERR-06-COUNT  TO PR1-A-ERR-06-CT.
013590     MOVE WS-ACCURACY-ERR-08-COUNT  TO PR1-A-ERR-08-CT.
013600     MOVE WS-ACCURACY-ERR-09-COUNT  TO PR1-A-ERR-09-CT.
013610     MOVE WS-ACCURACY-TOTAL-ERR-CT  TO PR1-A-TOT-ERR-CT.
013620*
013630     IF WS-ACCURACY-TRAN-COUNT NOT EQUAL +0
013640       IF WS-ACCURACY-TOTAL-ERR-CT EQUAL +0
013650         MOVE 100 TO PR1-A-PCT
013660       ELSE
013670         SUBTRACT WS-ACCURACY-TOTAL-ERR-CT FROM
013680               WS-ACCURACY-TRAN-COUNT GIVING WS-WORK-S9
013690         DIVIDE WS-WORK-S9 BY WS-ACCURACY-TRAN-COUNT
013700               GIVING WS-WORK-RATIO ROUNDED
013710         MOVE WS-WORK-PERCENT TO PR1-A-PCT.
013720*
013730     MOVE WS-ACCURACY-ERR-BC-COUNT TO PR1-A-ERR-BC-CT.
013740     PERFORM 2600-WRITE-PRINTER1-REC.
013750*
013760     MOVE SPACE TO PRINTER1-REC.
013770     MOVE '   ORDERS ' TO PR1-A-CAPT.
013780     MOVE WS-ACCURACY-ORDER-COUNT  TO PR1-A-TRN-CT.
013790     MOVE WS-ACCURACY-ORDER-01-COUNT  TO PR1-A-ERR-01-CT.
013800     MOVE WS-ACCURACY-ORDER-02-COUNT  TO PR1-A-ERR-02-CT.
013810     MOVE WS-ACCURACY-ORDER-03-COUNT  TO PR1-A-ERR-03-CT.
013820     MOVE WS-ACCURACY-ORDER-06-COUNT  TO PR1-A-ERR-06-CT.
013830     MOVE WS-ACCURACY-ORDER-08-COUNT  TO PR1-A-ERR-08-CT.
013840     MOVE WS-ACCURACY-ORDER-09-COUNT  TO PR1-A-ERR-09-CT.
013850     MOVE WS-ACCURACY-TOTAL-ORDER-CT  TO PR1-A-TOT-ERR-CT.
013860*
013870     IF WS-ACCURACY-ORDER-COUNT NOT EQUAL +0
013880       IF WS-ACCURACY-TOTAL-ORDER-CT EQUAL +0
013890         MOVE 100 TO PR1-A-PCT
013900       ELSE
013910         SUBTRACT WS-ACCURACY-TOTAL-ORDER-CT FROM
013920               WS-ACCURACY-ORDER-COUNT GIVING WS-WORK-S9
013930         DIVIDE WS-WORK-S9 BY WS-ACCURACY-ORDER-COUNT
013940               GIVING WS-WORK-RATIO ROUNDED
013950         MOVE WS-WORK-PERCENT TO PR1-A-PCT.
013960*
013970     MOVE WS-ACCURACY-ORDER-BC-COUNT TO PR1-A-ERR-BC-CT.
013980     PERFORM 2600-WRITE-PRINTER1-REC.
013990*
014000   2600-WRITE-PRINTER1-REC.
014010*
014020     WRITE PRINTER1-REC AFTER WS-LINE-SPACER.
014030     ADD WS-LINE-SPACER TO WS-LINE-COUNT.
014040*
014050   2800-PRINT-HEADING-1.
014060*
014070     WRITE PRINTER1-REC FROM WS-HEADING-1 AFTER PAGE.
014080     MOVE +1 TO WS-LINE-COUNT.
014090*
014100   2900-FOOT-PRINTER1.
014110*
014120     MOVE WS-END-OF-REPORT TO PRINTER1-REC.
014130     MOVE +1 TO WS-LINE-SPACER.
014140     PERFORM 2600-WRITE-PRINTER1-REC.
014150*
014160   3000-PRT-BCS-W-ERRORS.
014170*
014180     MOVE 'BANK CODES WITH 100 CREDITS FOR BILLING ERRORS:'
014190           TO PRINTER1-REC (1:47).
014200     MOVE SPACE TO PRINTER1-REC (48:85).
014210     MOVE +3 TO WS-LINE-SPACER.
014220     PERFORM 2600-WRITE-PRINTER1-REC.
014230     MOVE ALL '_' TO PRINTER1-REC (1:47).
014240     MOVE SPACE TO PRINTER1-REC (48:85).
DLXMIG*    WRITE PRINTER1-REC AFTER NO-ADVANCE.
DLXMIG     WRITE PRINTER1-REC. 
014270     MOVE +2 TO WS-LINE-SPACER.
014280     MOVE WS-B-HD-1 TO PRINTER1-REC.
014290     PERFORM 2600-WRITE-PRINTER1-REC.
014300*
014310     IF IX-WS-BC-ERR-TBL-IN NOT GREATER +1
014320       MOVE SPACE TO PRINTER1-REC
014330       MOVE '(NONE)' TO PRINTER1-REC (5:6)
014340       PERFORM 2600-WRITE-PRINTER1-REC
014350     ELSE
014360       SET IX-WS-BC-ERR-TBL-OUT TO +1
014370       PERFORM 3100-RPT-BC-W-ERRORS
014380           WITH TEST AFTER
014390           UNTIL IX-WS-BC-ERR-TBL-OUT NOT LESS IX-WS-BC-ERR-TBL-IN
014400       IF IX-WS-BC-ERR-TBL-IN GREATER +801
014410         DISPLAY 'CREDIT BC TABLE OVER 80% FULL.' UPON SYSOUT
014420         MOVE 'Y' TO WS-RETURN-04-FLAG.
014430*
014440   3100-RPT-BC-W-ERRORS.
014450*
014460     IF WS-LINE-SPACER GREATER +58
014470       PERFORM 2800-PRINT-HEADING-1
014480       MOVE +3 TO WS-LINE-SPACER
014490       MOVE WS-B-HD-1 TO PRINTER1-REC
014500       PERFORM 2600-WRITE-PRINTER1-REC
014510       MOVE +2 TO WS-LINE-SPACER.
014520*
014530     MOVE WS-BC-ERR-TBL-REC (IX-WS-BC-ERR-TBL-OUT) TO
014540           WS-BC-ERR-REC.
014550     SET IX-WS-BC-ERR-TBL-OUT UP BY 1.
014560     MOVE SPACE TO PRINTER1-REC.
014570     SET IX-PR1-B-ERR TO +1.
014580     MOVE WS-BC-ERR-REC-BC TO PR1-B-BC.
014590*
014600     MOVE WS-BC-ERR-REC-COUNT TO PR1-B-COUNT.
014610     MOVE WS-BC-ERR-ORD-REC-COUNT TO PR1-B-CR-ORD-COUNT.
014620     MOVE WS-BC-ERR-REC-BASE-DEL TO PR1-B-REP-BASE-DELEVERY.
014630     MOVE WS-BC-ERR-REC-UPCHARGE TO PR1-B-UPCHARGE.
014640     MOVE WS-BC-ERR-REC-GROSS TO PR1-B-GROSS.
014650     IF WS-BC-ERR-REC-01-FLAG EQUAL 'Y'
014660       MOVE '01' TO PR1-B-ERROR-CODE (IX-PR1-B-ERR)
014670       SET IX-PR1-B-ERR UP BY 1.
014680     IF WS-BC-ERR-REC-02-FLAG EQUAL 'Y'
014690       MOVE '02' TO PR1-B-ERROR-CODE (IX-PR1-B-ERR)
014700       SET IX-PR1-B-ERR UP BY 1.
014710     IF WS-BC-ERR-REC-03-FLAG EQUAL 'Y'
014720       MOVE '03' TO PR1-B-ERROR-CODE (IX-PR1-B-ERR)
014730       SET IX-PR1-B-ERR UP BY 1.
014740     IF WS-BC-ERR-REC-06-FLAG EQUAL 'Y'
014750       MOVE '06' TO PR1-B-ERROR-CODE (IX-PR1-B-ERR)
014760       SET IX-PR1-B-ERR UP BY 1.
014770     IF WS-BC-ERR-REC-08-FLAG EQUAL 'Y'
014780       MOVE '08' TO PR1-B-ERROR-CODE (IX-PR1-B-ERR)
014790       SET IX-PR1-B-ERR UP BY 1.
014800     IF WS-BC-ERR-REC-09-FLAG EQUAL 'Y'
014810       MOVE '09' TO PR1-B-ERROR-CODE (IX-PR1-B-ERR).
014820*
014830     PERFORM 3200-GET-FI-NAME.
014840     PERFORM 2600-WRITE-PRINTER1-REC.
014850*
014860   3200-GET-FI-NAME.
014870*
014880     MOVE SPACES TO WS-I550-RECORD-KEY.
014890     MOVE WS-BC-ERR-REC-BC TO WS-I550-BC.
014900     MOVE WS-I550-RECORD-KEY TO I550-RECORD-KEY.
014910*
014920     PERFORM 3300-START-READ-I550-FILE.
014930*
014940     IF WS-I550-SUCCESSFUL
014950       IF WS-I550-BC = I550-BC
014960         MOVE I550-FI-NAME TO PR1-B-FI-NAME
014970       ELSE
014980         MOVE 'MISSING I550 FI NAME' TO PR1-B-FI-NAME
014990       END-IF
015000     ELSE
015010       IF WS-I550-NO-SUCH-RECORD
015020         MOVE 'MISSING I550 FI NAME' TO PR1-B-FI-NAME.
015030*
015040   3300-START-READ-I550-FILE.
015050*
015060     START I550-FILE KEY NOT LESS I550-RECORD-KEY.
015070*
015080     IF WS-I550-SUCCESSFUL
015090       PERFORM 3400-READ-I550-FILE-NEXT
015100     ELSE
015110       IF (NOT WS-I550-END-OF-FILE)
015120           AND (NOT WS-I550-NO-SUCH-RECORD)
015130         DISPLAY '* I550 VSAM ERROR.  KEY = "' I550-RECORD-KEY
015140               '".  FILE STATUS = ' WS-I550-FILE-STATUS '. *'
015150               UPON SYSOUT
015160         GO TO 9900-DISPLAY-ABORT.
015170*
015180   3400-READ-I550-FILE-NEXT.
015190*
015200     READ I550-FILE NEXT RECORD.
015210*
015220     IF (NOT WS-I550-SUCCESSFUL)
015230         AND (NOT WS-I550-END-OF-FILE)
015240       DISPLAY '* VSAM ERROR ON I550 READ NEXT. FILE STATUS = '
015250             WS-I550-FILE-STATUS '. *' UPON SYSOUT
015260       GO TO 9900-DISPLAY-ABORT.
015270*
015280   3500-PRT-BCS-CODE-W-ERRORS.
015290*
015300     MOVE 'OTHER SITUATIONS WHERE CREDIT TRANSACTIONS FOR A '
015310          TO PRINTER1-REC (1:49).
015320     MOVE 'GIVEN REASON CODE FOR A GIVEN BANK CODE EXCEED 49:'
015330                              TO PRINTER1-REC (50:50).
015340     MOVE SPACES TO PRINTER1-REC (100:33).
015350     MOVE +3 TO WS-LINE-SPACER.
015360     PERFORM 2600-WRITE-PRINTER1-REC.
015370     MOVE ALL '_' TO PRINTER1-REC (1:99).
015380     MOVE SPACES TO PRINTER1-REC (100:33).
DLXMIG*    WRITE PRINTER1-REC AFTER NO-ADVANCE.
DLXMIG     WRITE PRINTER1-REC. 
015410     MOVE +2 TO WS-LINE-SPACER.
015420     MOVE WS-B-HD-1 TO PRINTER1-REC.
015430     PERFORM 2600-WRITE-PRINTER1-REC.
015440
015450     IF WS-BC-ERR1-TBL-ENT-COUNT EQUAL TO +0
015460       MOVE SPACE TO PRINTER1-REC
015470       MOVE '(NONE)' TO PRINTER1-REC (5:6)
015480       PERFORM 2600-WRITE-PRINTER1-REC
015490     ELSE
015500       SET IX-WS-BC-ERR1-TBL-OUT TO +1
015510       PERFORM 3600-RPT-BC-CODE-W-ERRORS
015520                        WS-BC-ERR1-TBL-ENT-COUNT TIMES
015530       IF WS-BC-ERR1-TBL-ENT-COUNT GREATER +8001
015540         DISPLAY 'CREDIT BC CODE TABLE OVER 80% FULL.' UPON SYSOUT
015550         MOVE 'Y' TO WS-RETURN-04-FLAG.
015560*
015570   3600-RPT-BC-CODE-W-ERRORS.
015580*
015590     IF WS-LINE-SPACER GREATER +58
015600       PERFORM 2800-PRINT-HEADING-1
015610       MOVE +3 TO WS-LINE-SPACER
015620       MOVE WS-B-HD-1 TO PRINTER1-REC
015630       PERFORM 2600-WRITE-PRINTER1-REC
015640       MOVE +2 TO WS-LINE-SPACER.
015650*
015660       MOVE WS-BC-ERR1-TBL-REC (IX-WS-BC-ERR1-TBL-OUT) TO
015670                                       WS-BC-ERR1-REC
015680       SET IX-WS-BC-ERR1-TBL-OUT UP BY 1.
015690
015700     IF WS-BC-ERR1-REC-COUNT GREATER +49
015710       MOVE SPACE TO PRINTER1-REC
015720       MOVE WS-BC-ERR1-REC-BC TO PR1-C-BC
015730       MOVE WS-BC-ERR1-REC-COUNT TO PR1-C-COUNT
015740       MOVE WS-BC-ERR1-ORD-REC-COUNT TO PR1-C-CR-ORD-COUNT
015750       MOVE WS-BC-ERR1-CODE TO PR1-C-ERROR-CODE
015760       MOVE WS-BC-ERR1-REC-BASE-DEL TO PR1-C-REP-BASE-DELEVERY
015770       MOVE WS-BC-ERR1-REC-UPCHARGE TO PR1-C-UPCHARGE
015780       MOVE WS-BC-ERR1-REC-GROSS TO PR1-C-GROSS
015790*
015800       PERFORM 3700-GET-FI-NAME
015810       PERFORM 2600-WRITE-PRINTER1-REC
015820     END-IF.
015830*
015840   3700-GET-FI-NAME.
015850*
015860     MOVE SPACES TO WS-I550-RECORD-KEY.
015870     MOVE WS-BC-ERR1-REC-BC TO WS-I550-BC.
015880     MOVE WS-I550-RECORD-KEY TO I550-RECORD-KEY.
015890*
015900     PERFORM 3300-START-READ-I550-FILE.
015910*
015920     IF WS-I550-SUCCESSFUL
015930       IF WS-I550-BC = I550-BC
015940         MOVE I550-FI-NAME TO PR1-C-FI-NAME
015950       ELSE
015960         MOVE 'MISSING I550 FI NAME' TO PR1-C-FI-NAME
015970       END-IF
015980     ELSE
015990       IF WS-I550-NO-SUCH-RECORD
016000         MOVE 'MISSING I550 FI NAME' TO PR1-C-FI-NAME.
016010*
016020   3800-PRT-BCS-REST-REIM-RECAP.
016030*
016040     MOVE '- REIMBURSEMENTS AND RESTITUTIONS -'
016050          TO PRINTER1-REC (1:35).
016060     MOVE SPACES TO PRINTER1-REC (36:97).
016070     MOVE +3 TO WS-LINE-SPACER.
016080     PERFORM 2600-WRITE-PRINTER1-REC.
016090     MOVE ALL '_' TO PRINTER1-REC (1:35).
016100     MOVE SPACES TO PRINTER1-REC (36:97).
016110     MOVE +1 TO WS-LINE-SPACER.
DLXMIG*    WRITE PRINTER1-REC AFTER NO-ADVANCE.
DLXMIG     WRITE PRINTER1-REC. 
016140     MOVE +2 TO WS-LINE-SPACER.
016150     MOVE WS-D-HD-1 TO PRINTER1-REC.
016160     PERFORM 2600-WRITE-PRINTER1-REC.
016170*
016180     IF IX-WS-BC-REST-REIM-TBL-IN  NOT GREATER +1
016190       MOVE SPACE TO PRINTER1-REC
016200       MOVE '(NONE)' TO PRINTER1-REC (5:6)
016210       PERFORM 2600-WRITE-PRINTER1-REC
016220     ELSE
016230       SET IX-WS-BC-REST-REIM-TBL-OUT TO +1
016240       PERFORM 3900-RPT-BC-REST-REIM-W-INFO
016250         WITH TEST AFTER
016260         UNTIL IX-WS-BC-REST-REIM-TBL-OUT NOT LESS
016270             IX-WS-BC-REST-REIM-TBL-IN
016280       IF IX-WS-BC-REST-REIM-TBL-IN GREATER +8001
016290         DISPLAY 'CREDIT REST/REIM TABLE OVER 80% FULL.'
016300                                                  UPON SYSOUT
016310         MOVE 'Y' TO WS-RETURN-04-FLAG.
016320*
016330   3900-RPT-BC-REST-REIM-W-INFO.
016340*
016350     IF WS-LINE-SPACER GREATER +58
016360       PERFORM 2800-PRINT-HEADING-1
016370       MOVE +3 TO WS-LINE-SPACER
016380       MOVE WS-B-HD-1 TO PRINTER1-REC
016390       PERFORM 2600-WRITE-PRINTER1-REC
016400       MOVE +2 TO WS-LINE-SPACER.
016410*
016420     MOVE WS-BC-REST-REIM-TBL-REC (IX-WS-BC-REST-REIM-TBL-OUT)
016430                             TO WS-BC-REST-REIM-REC
016440     SET  IX-WS-BC-REST-REIM-TBL-OUT UP BY 1.
016450     MOVE SPACE TO PRINTER1-REC.
016460     MOVE WS-BC-REST-REIM-PROD-ID(1:13)  TO PR1-D-PROD-CODE.
016470     MOVE WS-BC-REST-REIM-BC  TO PR1-D-BC.
016480     MOVE WS-BC-REST-REIM-REP-BASE TO PR1-D-REP-BASE.
016490     MOVE WS-BC-REST-REIM-REP-DEL TO PR1-D-REP-DEL.
016500
016510     IF WS-BC-REST-REIM-PROD-ID(3:10) EQUAL TO '9201031798'
016520       MOVE 'RESTITUTION             ' TO PR1-D-PROD-INFO
016530     ELSE
016540       IF WS-BC-REST-REIM-PROD-ID(3:10) EQUAL TO '9201031799'
016550         MOVE 'REIMBURSEMENT           ' TO PR1-D-PROD-INFO
016560       END-IF
016570     END-IF.
016580
016590     PERFORM 3950-GET-REST-REIM-FI-INFO.
016600     PERFORM 2600-WRITE-PRINTER1-REC.
016610*
016620   3950-GET-REST-REIM-FI-INFO.
016630*
016640     MOVE SPACES TO WS-I550-RECORD-KEY.
016650     MOVE WS-BC-REST-REIM-BC TO WS-I550-BC.
016660     MOVE WS-I550-RECORD-KEY TO I550-RECORD-KEY.
016670*
016680     PERFORM 3300-START-READ-I550-FILE.
016690*
016700     IF WS-I550-SUCCESSFUL
016710       IF WS-I550-BC = I550-BC
016720         MOVE I550-FI-NAME TO PR1-D-FI-NAME
016730       ELSE
016740         MOVE 'MISSING I550 FI NAME' TO PR1-D-FI-NAME
016750       END-IF
016760     ELSE
016770       IF WS-I550-NO-SUCH-RECORD
016780         MOVE 'MISSING I550 FI NAME' TO PR1-D-FI-NAME.
016790*
016800   4000-GATHER-DATA.
016810     PERFORM 4100-EACH-BC
016820         WITH TEST AFTER
016830         UNTIL (END-IOWRK-FILE).
016840     PERFORM 5900-SUM-UP-TABLES.
016850*
016860   4100-EACH-BC.
016870     MOVE WS-IOWRK-BC TO WS-HOLD-BC.
016880     MOVE WS-HOLD-BC TO WS-BC-ERR-REC-BC.
016890     MOVE SPACE TO WS-BC-ERR-REC (9:6) WS-BC-FLAGS.
016900     MOVE +0 TO WS-BC-ERR-ORD-REC-COUNT.
016910     MOVE +0 TO WS-BC-ERR-REC-COUNT.
016920     MOVE +0 TO WS-BC-ERR-REC-BASE-DEL.
016930     MOVE +0 TO WS-BC-ERR-REC-UPCHARGE.
016940     MOVE +0 TO WS-BC-ERR-REC-GROSS.
016950     MOVE WS-HOLD-BC TO WS-BC-ERR1-REC-BC.
016960     MOVE SPACE TO WS-BC-ERR1-REC (9:2).
016970     MOVE +0 TO WS-BC-ERR1-REC-COUNT.
016980     MOVE +0 TO WS-BC-ERR1-ORD-REC-COUNT.
016990     MOVE +0 TO WS-BC-ERR1-REC-BASE-DEL.
017000     MOVE +0 TO WS-BC-ERR1-REC-UPCHARGE.
017010     MOVE +0 TO WS-BC-ERR1-REC-GROSS.
017020     PERFORM 4200-EACH-BRANCH
017030         WITH TEST AFTER
017040         UNTIL (WS-IOWRK-BC GREATER WS-HOLD-BC).
017050     PERFORM 5800-ADD-IN-BC-COUNTS.
017060*
017070   4200-EACH-BRANCH.
017080     MOVE SPACE TO WS-BR-MEDIUM-FLAGS.
017090     MOVE WS-IOWRK-INSTITUTION TO WS-HOLD-INSTITUTION
017100     PERFORM 4500-EACH-IOWRK-TAPE-CD
017110         WITH TEST BEFORE
017120         UNTIL WS-IOWRK-INSTITUTION NOT EQUAL WS-HOLD-INSTITUTION.
017130     IF WS-BR-MEDIUM-FLAGS NOT EQUAL SPACE
017140       PERFORM 5700-ADD-IN-BR-COUNTS.
017150*
017160   4400-READ-I8929I-FILE.
017170     READ I8929I-FILE
017180       AT END
017190         MOVE 'E' TO WS-I8929I-FILE-FLAG
017200       NOT AT END
017210         ADD +1 TO WS-I8929I-REC-COUNT
017220         IF I8929-PART-TYPE = '20'
017230           ADD +1 TO WS-I8929-IMAGE-INV-CNT
017240           IF I8929-20-SEP-INV-CD = '91' OR '92' OR '93'
017250             ADD +1 TO WS-I8929-DNM-INV-CNT
017260           END-IF
017270         END-IF
017280     END-READ.
017290*
017300*
017310   4410-READ-I8929P-FILE.
017320     READ I8929P-FILE
017330       AT END
017340         MOVE 'E' TO WS-I8929P-FILE-FLAG
017350       NOT AT END
017360         ADD +1 TO WS-I8929P-REC-COUNT
017370         IF O8929-PART-TYPE = '20'
017380           ADD +1 TO WS-I8929-PRINT-INV-CNT
017390         ELSE
017400           IF O8929-PART-TYPE = '50'
017410             ADD +1 TO WS-I8929-STUB-INV-CNT
017420           END-IF
017430         END-IF
017440     END-READ.
017450*
017460*
017470   4420-READ-I8929E-FILE.
017480     READ I8929E-FILE
017490       AT END
017500         MOVE 'E' TO WS-I8929E-FILE-FLAG
017510       NOT AT END
017520         IF W8929-PART-TYPE = '10'
017530           IF W8929-TRANSACTION-REC (44:3) EQUAL 'DLX'
017540             ADD +1 TO WS-I8929-EMAIL-INV-CNT
017550           END-IF
017560           IF W8929-TRANSACTION-REC (44:1) EQUAL 'S'
017570             ADD +1 TO WS-I8929-DSI-INV-CNT
017580           END-IF
017590         END-IF
017600         ADD +1 TO WS-I8929E-REC-COUNT
017610     END-READ.
017620*
017630   4500-EACH-IOWRK-TAPE-CD.
017640     MOVE WS-IOWRK-TAPE-CODE TO WS-HOLD-TAPE-CODE.
017650     IF WS-T-C-TAPE-CODE NOT EQUAL WS-HOLD-TAPE-CODE
017660       SEARCH ALL WS-T-C-TBL-ENTRY
017670         AT END
017680           MOVE WS-HOLD-TAPE-CODE TO WS-T-C-TAPE-CODE
017690           MOVE +4 TO WS-T-C-MED-SUBSCR
017700           SET IX-WS-T-C-TBL TO +1
017710           IF WS-HOLD-TAPE-CODE NOT EQUAL WS-PREV-BAD-TAPE-CODE
017720             MOVE WS-HOLD-TAPE-CODE TO WS-PREV-BAD-TAPE-CODE
017730             MOVE 'Y' TO WS-RETURN-12-FLAG
017740             DISPLAY 'MISSING I575 FROM IOWRK TAPE CODE "'
017750                 WS-HOLD-TAPE-CODE '" - WILL REPORT AS "PAPER".'
017760                 UPON SYSOUT
017770           END-IF
017780         WHEN WS-T-C-TBL-TAPE-CODE (IX-WS-T-C-TBL) EQUAL
017790               WS-HOLD-TAPE-CODE
017800           MOVE WS-T-C-TBL-ENTRY (IX-WS-T-C-TBL) TO
017810                 WS-TAPE-CODE-INFO
017820           IF WS-T-C-HIT-FLAG EQUAL SPACE
017830             MOVE 'Y' TO WS-T-C-TBL-HIT-FLAG (IX-WS-T-C-TBL)
017840             IF WS-T-C-MED-SUBSCR EQUAL +3
017850               ADD 1 TO WS-WIRE-COUNT
017860             END-IF
017870           END-IF
017880         END-SEARCH.
017890     MOVE 'Y' TO WS-BC-MEDIUM-FLAG (WS-T-C-MED-SUBSCR)
017900           WS-BR-MEDIUM-FLAG (WS-T-C-MED-SUBSCR).
017910     SET IX-WS-MED-TBL-MED TO WS-T-C-MED-SUBSCR.
017920     PERFORM 4600-EACH-IOWRK-SEQ
017930         WITH TEST AFTER
017940         UNTIL WS-IOWRK-TAPE-SEQ NOT EQUAL WS-HOLD-TAPE-SEQ.
017950*
017960   4600-EACH-IOWRK-SEQ.
017970*
017980     MOVE WS-IOWRK-SEQ-24-25 TO WS-HOLD-SEQ-24-25.
017990********* STARTS EXPENSE CODE EXPENSION DT:12:27:2000 *********
018000     IF WS-HOLD-EXP-BYTE-1 EQUAL '0' OR '4' OR '6' OR '8'
018010********** ENDS EXPENSE CODE EXPENSION DT:12:27:2000 **********
018020       SET IX-WS-MED-TBL-TRN TO +1
018030     ELSE
018040********* STARTS EXPENSE CODE EXPENSION DT:12:27:2000 *********
018050       IF WS-HOLD-EXP-BYTE-1 EQUAL '1' OR '5' OR '7' OR '9'
018060********** ENDS EXPENSE CODE EXPENSION DT:12:27:2000 **********
018070         SET IX-WS-MED-TBL-TRN TO +2
018080       ELSE
018090         SET IX-WS-MED-TBL-TRN TO +3.
018100     IF WS-HOLD-CREDIT-FLAG NOT EQUAL SPACE
018110       SET IX-WS-MED-TBL-TRN UP BY 3.
018120     PERFORM 4700-EACH-IOWRK-REC
018130         WITH TEST AFTER
018140         UNTIL WS-IOWRK-SEQ NOT EQUAL WS-HOLD-SEQ.
018150*
018160   4700-EACH-IOWRK-REC.
018170     ADD IOWRK-DUE-DELUXE TO WS-T-C-TBL-DUE-DLX (IX-WS-T-C-TBL).
018180     IF IOWRK-ORDER-COUNTS EQUAL 01
018190        ADD 1 TO WS-ACCURACY-ORDER-COUNT
018200     END-IF.
018210*
018220     ADD 1 TO WS-ACCURACY-TRAN-COUNT.
018230     ADD IOWRK-ABSOLUTE-REST TO WS-MED-TBL-ABS-DLRS
018240           (IX-WS-MED-TBL-MED).
018250     ADD IOWRK-ABSOLUTE-UPCHARGE TO WS-MED-TBL-ABS-UPCH
018260           (IX-WS-MED-TBL-MED).
018270     ADD IOWRK-DUE-DELUXE TO WS-MED-TBL-DUE-DELUXE
018280           (IX-WS-MED-TBL-MED).
018290     ADD 1 TO WS-MED-TBL-TRAN-COUNT
018300           (IX-WS-MED-TBL-MED, IX-WS-MED-TBL-TRN).
018310*
018320     IF (IOWRK-REASON-FOR-CREDIT EQUAL '01' OR '02' OR '03' OR
018330         '06' OR '08' OR '09')
018340       PERFORM 4900-ADD-ERROR-INFO
018350     ELSE
018360       IF IOWRK-REASON-FOR-CREDIT IS NUMERIC
018370         PERFORM 4950-ADD-ERROR1-INFO
018380       END-IF
018390     END-IF.
018400     IF (IOWRK-PRODUCT-ID(3:10) EQUAL
018410                     '9201031798' OR '9201031799')
018420       PERFORM 4970-REST-REIM-INFO
018430     ELSE
018440       CONTINUE
018450     END-IF.
018460     PERFORM 4800-READ-IOWRK-FILE.
018470*
018480   4800-READ-IOWRK-FILE.
018490*
018500     READ IOWRK-FILE
018510       AT END
018520         MOVE 'E' TO WS-IOWRK-FILE-FLAG
018530         MOVE HIGH-VALUES TO WS-IOWRK-INSTITUTION
018540*
018550       NOT AT END
018560         IF IOWRK-SEQ NOT LESS WS-IOWRK-SEQ
018570             MOVE IOWRK-SEQ TO WS-IOWRK-SEQ
018580             ADD 1 TO WS-IOWRK-REC-COUNT
018590         ELSE
018600           DISPLAY '* IOWRK SEQUENCE ERROR "' WS-IOWRK-SEQ '" "'
018610                 IOWRK-SEQ '" *' UPON SYSOUT
018620           GO TO 9900-DISPLAY-ABORT
018630         END-IF
018640       END-READ.
018650*
018660   4900-ADD-ERROR-INFO.
018670*
018680     ADD 1 TO WS-BC-ERR-REC-COUNT.
018690     ADD IOWRK-REPORT-BASE-DELIVERY TO WS-BC-ERR-REC-BASE-DEL.
018700     ADD IOWRK-UPCHRAGE TO WS-BC-ERR-REC-UPCHARGE.
018710     ADD IOWRK-GROSS TO WS-BC-ERR-REC-GROSS.
018720     IF IOWRK-REGION NOT EQUAL SPACE
018730       MOVE 'Y' TO WS-BC-ERROR-FLAG.
018740*
018750     IF IOWRK-ORDER-COUNTS EQUAL 01
018760       ADD 1 TO WS-BC-ERR-ORD-REC-COUNT
018770     END-IF.
018780*
018790     IF IOWRK-REASON-FOR-CREDIT EQUAL '01'
018800       ADD 1 TO WS-ACCURACY-ERR-01-COUNT
018810         IF IOWRK-ORDER-COUNTS EQUAL 01
018820           ADD 1 TO WS-ACCURACY-ORDER-01-COUNT
018830         END-IF
018840       MOVE 'Y' TO WS-BC-ERR-REC-01-FLAG
018850     ELSE
018860       IF IOWRK-REASON-FOR-CREDIT EQUAL '02'
018870         ADD 1 TO WS-ACCURACY-ERR-02-COUNT
018880           IF IOWRK-ORDER-COUNTS EQUAL 01
018890             ADD 1 TO WS-ACCURACY-ORDER-02-COUNT
018900           END-IF
018910         MOVE 'Y' TO WS-BC-ERR-REC-02-FLAG
018920       ELSE
018930         IF IOWRK-REASON-FOR-CREDIT EQUAL '03'
018940           ADD 1 TO WS-ACCURACY-ERR-03-COUNT
018950             IF IOWRK-ORDER-COUNTS EQUAL 01
018960               ADD 1 TO WS-ACCURACY-ORDER-03-COUNT
018970             END-IF
018980           MOVE 'Y' TO WS-BC-ERR-REC-03-FLAG
018990         ELSE
019000           IF IOWRK-REASON-FOR-CREDIT EQUAL '06'
019010             ADD 1 TO WS-ACCURACY-ERR-06-COUNT
019020               IF IOWRK-ORDER-COUNTS EQUAL 01
019030                 ADD 1 TO WS-ACCURACY-ORDER-06-COUNT
019040               END-IF
019050             MOVE 'Y' TO WS-BC-ERR-REC-06-FLAG
019060           ELSE
019070             IF IOWRK-REASON-FOR-CREDIT EQUAL '08'
019080               ADD 1 TO WS-ACCURACY-ERR-08-COUNT
019090                       IF IOWRK-ORDER-COUNTS EQUAL 01
019100                         ADD 1 TO WS-ACCURACY-ORDER-08-COUNT
019110                       END-IF
019120               MOVE 'Y' TO WS-BC-ERR-REC-08-FLAG
019130             ELSE
019140               ADD 1 TO WS-ACCURACY-ERR-09-COUNT
019150                       IF IOWRK-ORDER-COUNTS EQUAL 01
019160                         ADD 1 TO WS-ACCURACY-ORDER-09-COUNT
019170                       END-IF
019180               MOVE 'Y' TO WS-BC-ERR-REC-09-FLAG.
019190*
019200   4950-ADD-ERROR1-INFO.
019210*
019220     IF IOWRK-REGION NOT EQUAL SPACE
019230       MOVE 'Y' TO WS-BC-ERROR1-FLAG
019240     END-IF.
019250
019260     MOVE SPACES TO WS-BC-ERR1-COMB.
019270     MOVE IOWRK-INSTITUTION(1:8)  TO WS-BC-ERR1-REC-BC.
019280     MOVE IOWRK-REASON-FOR-CREDIT TO WS-BC-ERR1-CODE.
019290
019300     IF WS-REASON-SEQUENCE-FLAG EQUAL TO SPACES
019310        MOVE WS-BC-ERR1-COMB TO WS-BC-ERR1-TBL-COMB
019320                                 (IX-WS-BC-ERR1-TBL-IN)
019330        MOVE 'N' TO WS-REASON-SEQUENCE-FLAG
019340     END-IF.
019350
019360     SET IX-WS-BC-ERR1-TBL-IN TO +1.
019370     SEARCH WS-BC-ERR1-TBL-REC
019380       AT END
019390*
019400         PERFORM 4960-ADD-IN-BC-CODE-COUNTS
019410*
019420           WHEN WS-BC-ERR1-TBL-COMB (IX-WS-BC-ERR1-TBL-IN)
019430                                 EQUAL WS-BC-ERR1-COMB
019440             ADD 1 TO WS-BC-ERR1-TBL-REC-COUNT
019450                               (IX-WS-BC-ERR1-TBL-IN)
019460             ADD IOWRK-REPORT-BASE-DELIVERY TO
019470               WS-BC-ERR1-TBL-REC-BASE-DEL (IX-WS-BC-ERR1-TBL-IN)
019480             ADD IOWRK-UPCHRAGE TO
019490               WS-BC-ERR1-TBL-REC-UPCHARGE (IX-WS-BC-ERR1-TBL-IN)
019500             ADD IOWRK-GROSS TO
019510               WS-BC-ERR1-TBL-REC-GROSS (IX-WS-BC-ERR1-TBL-IN)
019520             IF IOWRK-ORDER-COUNTS EQUAL 01
019530                ADD 1 TO WS-BC-ERR1-TBL-ORD-REC-COUNT
019540                               (IX-WS-BC-ERR1-TBL-IN)
019550             END-IF
019560     END-SEARCH.
019570*
019580   4960-ADD-IN-BC-CODE-COUNTS.
019590*
019600        MOVE WS-BC-ERR1-COMB TO  WS-BC-ERR1-TBL-COMB
019610                          (IX-WS-BC-ERR1-TBL-IN).
019620
019630        MOVE 1 TO WS-BC-ERR1-TBL-REC-COUNT
019640                          (IX-WS-BC-ERR1-TBL-IN).
019650        ADD IOWRK-REPORT-BASE-DELIVERY TO
019660          WS-BC-ERR1-TBL-REC-BASE-DEL (IX-WS-BC-ERR1-TBL-IN).
019670        ADD IOWRK-UPCHRAGE TO
019680          WS-BC-ERR1-TBL-REC-UPCHARGE (IX-WS-BC-ERR1-TBL-IN).
019690        ADD IOWRK-GROSS TO
019700          WS-BC-ERR1-TBL-REC-GROSS (IX-WS-BC-ERR1-TBL-IN).
019710        IF IOWRK-ORDER-COUNTS EQUAL 01
019720          MOVE 1 TO WS-BC-ERR1-TBL-ORD-REC-COUNT
019730                          (IX-WS-BC-ERR1-TBL-IN)
019740        END-IF.
019750
019760        ADD +1 TO WS-BC-ERR1-TBL-ENT-COUNT.
019770
019780        IF WS-BC-ERR1-TBL-ENT-COUNT = 8000
019790           DISPLAY '* BC ERR1 TABLE IS 80% FULL *' UPON SYSOUT
019800           MOVE 'Y'                  TO WS-RETURN-04-FLAG
019810        END-IF.
019820
019830        IF WS-BC-ERR1-TBL-ENT-COUNT = 10000
019840           DISPLAY '* BC ERR1 TABLE HAS OVERFLOWED *' UPON SYSOUT
019850           GO TO 9900-DISPLAY-ABORT
019860        END-IF.
019870*
019880   4970-REST-REIM-INFO.
019890*
019900       MOVE IOWRK-PRODUCT-ID TO WS-BC-REST-REIM-TBL-PROD-ID
019910                                (IX-WS-BC-REST-REIM-TBL-IN).
019920       MOVE IOWRK-INSTITUTION(1:8) TO WS-BC-REST-REIM-TBL-BC
019930                                (IX-WS-BC-REST-REIM-TBL-IN).
019940       MOVE IOWRK-REPORT-BASE  TO WS-BC-REST-REIM-TBL-REP-BASE
019950                                (IX-WS-BC-REST-REIM-TBL-IN).
019960       MOVE IOWRK-REPORT-DELIVERY  TO WS-BC-REST-REIM-TBL-REP-DEL
019970                                (IX-WS-BC-REST-REIM-TBL-IN).
019980       SET IX-WS-BC-REST-REIM-TBL-IN UP BY 1.
019990
020000       IF IX-WS-BC-REST-REIM-TBL-IN = 8000
020010         DISPLAY '* REST/REIM TABLE IS 80% FULL *' UPON SYSOUT
020020         MOVE 'Y'                  TO WS-RETURN-04-FLAG
020030       END-IF.
020040
020050       IF IX-WS-BC-REST-REIM-TBL-IN = 10000
020060         DISPLAY '* REST/REIM TABLE HAS OVERFLOWED *' UPON SYSOUT
020070         GO TO 9900-DISPLAY-ABORT
020080        END-IF.
020090*
020100   5700-ADD-IN-BR-COUNTS.
020110*
020120     ADD 1 TO WS-MED-TBL-BR-COUNT (5).
020130     IF WS-BR-MEDIUM-FLAG (1) NOT EQUAL SPACE
020140       ADD 1 TO WS-MED-TBL-BR-COUNT (1).
020150     IF WS-BR-MEDIUM-FLAG (2) NOT EQUAL SPACE
020160       ADD 1 TO WS-MED-TBL-BR-COUNT (2).
020170     IF WS-BR-MEDIUM-FLAG (3) NOT EQUAL SPACE
020180       ADD 1 TO WS-MED-TBL-BR-COUNT (3).
020190     IF WS-BR-MEDIUM-FLAG (4) NOT EQUAL SPACE
020200       ADD 1 TO WS-MED-TBL-BR-COUNT (4).
020210*
020220   5800-ADD-IN-BC-COUNTS.
020230*
020240     IF WS-BC-ERR-REC-COUNT GREATER +99
020250       IF IX-WS-BC-ERR-TBL-IN NOT GREATER +1000
020260         MOVE WS-BC-ERR-REC TO WS-BC-ERR-TBL-REC
020270               (IX-WS-BC-ERR-TBL-IN)
020280         SET IX-WS-BC-ERR-TBL-IN UP BY 1
020290       ELSE
020300         DISPLAY '* CREDIT BC TABLE OVERFLOWED *' UPON SYSOUT
020310         GO TO 9900-DISPLAY-ABORT.
020320*
020330     IF WS-BC-MEDIUM-FLAGS NOT EQUAL SPACE
020340       ADD 1 TO WS-MED-TBL-BC-COUNT (5)
020350       IF WS-BC-MEDIUM-FLAG (1) NOT EQUAL SPACE
020360         ADD 1 TO WS-MED-TBL-BC-COUNT (1)
020370       END-IF
020380       IF WS-BC-MEDIUM-FLAG (2) NOT EQUAL SPACE
020390         ADD 1 TO WS-MED-TBL-BC-COUNT (2)
020400       END-IF
020410       IF WS-BC-MEDIUM-FLAG (3) NOT EQUAL SPACE
020420         ADD 1 TO WS-MED-TBL-BC-COUNT (3)
020430       END-IF
020440       IF WS-BC-MEDIUM-FLAG (4) NOT EQUAL SPACE
020450         ADD 1 TO WS-MED-TBL-BC-COUNT (4).
020460*
020470       IF WS-BC-INVOICE-FLAG (1) NOT EQUAL SPACE
020480         ADD 1 TO WS-INV-TBL-INV-BC-COUNT (1)
020490       END-IF
020500       IF WS-BC-DEBIT-FLAG (1) NOT EQUAL SPACE
020510         ADD 1 TO WS-INV-TBL-DEB-BC-COUNT (1)
020520       END-IF
020530*
020540     IF WS-BC-ERROR
020550       ADD 1 TO WS-ACCURACY-ERR-BC-COUNT
020560       ADD 1 TO WS-ACCURACY-ORDER-BC-COUNT
020570       MOVE SPACE TO WS-BC-ERROR-FLAG.
020580     IF WS-BC-ERROR1
020590       ADD 1 TO WS-BC-ERR1-REC-COUNT
020600       ADD 1 TO WS-BC-ERR1-ORD-REC-COUNT
020610       MOVE SPACE TO WS-BC-ERROR1-FLAG.
020620*
020630   5900-SUM-UP-TABLES.
020640*
020650     ADD WS-MED-TBL-TRAN-COUNT (1, 1)
020660           WS-MED-TBL-TRAN-COUNT (1, 2)
020670           WS-MED-TBL-TRAN-COUNT (1, 3)
020680           WS-MED-TBL-TRAN-COUNT (1, 4)
020690           WS-MED-TBL-TRAN-COUNT (1, 5)
020700           WS-MED-TBL-TRAN-COUNT (1, 6)
020710           GIVING WS-MED-TBL-TRAN-COUNT (1, 7).
020720     ADD WS-MED-TBL-TRAN-COUNT (2, 1)
020730           WS-MED-TBL-TRAN-COUNT (2, 2)
020740           WS-MED-TBL-TRAN-COUNT (2, 3)
020750           WS-MED-TBL-TRAN-COUNT (2, 4)
020760           WS-MED-TBL-TRAN-COUNT (2, 5)
020770           WS-MED-TBL-TRAN-COUNT (2, 6)
020780           GIVING WS-MED-TBL-TRAN-COUNT (2, 7).
020790     ADD WS-MED-TBL-TRAN-COUNT (3, 1)
020800           WS-MED-TBL-TRAN-COUNT (3, 2)
020810           WS-MED-TBL-TRAN-COUNT (3, 3)
020820           WS-MED-TBL-TRAN-COUNT (3, 4)
020830           WS-MED-TBL-TRAN-COUNT (3, 5)
020840           WS-MED-TBL-TRAN-COUNT (3, 6)
020850           GIVING WS-MED-TBL-TRAN-COUNT (3, 7).
020860     ADD WS-MED-TBL-TRAN-COUNT (4, 1)
020870           WS-MED-TBL-TRAN-COUNT (4, 2)
020880           WS-MED-TBL-TRAN-COUNT (4, 3)
020890           WS-MED-TBL-TRAN-COUNT (4, 4)
020900           WS-MED-TBL-TRAN-COUNT (4, 5)
020910           WS-MED-TBL-TRAN-COUNT (4, 6)
020920           GIVING WS-MED-TBL-TRAN-COUNT (4, 7).
020930     ADD WS-MED-TBL-ABS-UPCH (1) TO WS-MED-TBL-ABS-DLRS (1).
020940     ADD WS-MED-TBL-ABS-UPCH (2) TO WS-MED-TBL-ABS-DLRS (2).
020950     ADD WS-MED-TBL-ABS-UPCH (3) TO WS-MED-TBL-ABS-DLRS (3).
020960     ADD WS-MED-TBL-ABS-UPCH (4) TO WS-MED-TBL-ABS-DLRS (4).
020970     ADD WS-MED-TBL-ABS-DLRS (1) WS-MED-TBL-ABS-DLRS (2)
020980           WS-MED-TBL-ABS-DLRS (3) WS-MED-TBL-ABS-DLRS (4)
020990           GIVING WS-MED-TBL-ABS-DLRS (5).
021000     ADD WS-MED-TBL-DUE-DELUXE (1) WS-MED-TBL-DUE-DELUXE (2)
021010           WS-MED-TBL-DUE-DELUXE (3) WS-MED-TBL-DUE-DELUXE (4)
021020           GIVING WS-MED-TBL-DUE-DELUXE (5).
021030*
021040     ADD WS-MED-TBL-ABS-UPCH (1) WS-MED-TBL-ABS-UPCH (2)
021050           WS-MED-TBL-ABS-UPCH (3) WS-MED-TBL-ABS-UPCH (4)
021060           GIVING WS-MED-TBL-ABS-UPCH (5).
021070     ADD WS-MED-TBL-TRAN-COUNT (1, 1)
021080           WS-MED-TBL-TRAN-COUNT (2, 1)
021090           WS-MED-TBL-TRAN-COUNT (3, 1)
021100           WS-MED-TBL-TRAN-COUNT (4, 1) GIVING
021110           WS-MED-TBL-TRAN-COUNT (5, 1).
021120*
021130     ADD WS-MED-TBL-TRAN-COUNT (1, 2)
021140           WS-MED-TBL-TRAN-COUNT (2, 2)
021150           WS-MED-TBL-TRAN-COUNT (3, 2)
021160           WS-MED-TBL-TRAN-COUNT (4, 2) GIVING
021170           WS-MED-TBL-TRAN-COUNT (5, 2).
021180     ADD WS-MED-TBL-TRAN-COUNT (1, 3)
021190           WS-MED-TBL-TRAN-COUNT (2, 3)
021200           WS-MED-TBL-TRAN-COUNT (3, 3)
021210           WS-MED-TBL-TRAN-COUNT (4, 3) GIVING
021220           WS-MED-TBL-TRAN-COUNT (5, 3).
021230*
021240     ADD WS-MED-TBL-TRAN-COUNT (1, 4)
021250           WS-MED-TBL-TRAN-COUNT (2, 4)
021260           WS-MED-TBL-TRAN-COUNT (3, 4)
021270           WS-MED-TBL-TRAN-COUNT (4, 4) GIVING
021280           WS-MED-TBL-TRAN-COUNT (5, 4).
021290     ADD WS-MED-TBL-TRAN-COUNT (1, 5)
021300           WS-MED-TBL-TRAN-COUNT (2, 5)
021310           WS-MED-TBL-TRAN-COUNT (3, 5)
021320           WS-MED-TBL-TRAN-COUNT (4, 5) GIVING
021330           WS-MED-TBL-TRAN-COUNT (5, 5).
021340*
021350     ADD WS-MED-TBL-TRAN-COUNT (1, 6)
021360           WS-MED-TBL-TRAN-COUNT (2, 6)
021370           WS-MED-TBL-TRAN-COUNT (3, 6)
021380           WS-MED-TBL-TRAN-COUNT (4, 6) GIVING
021390           WS-MED-TBL-TRAN-COUNT (5, 6).
021400     ADD WS-MED-TBL-TRAN-COUNT (1, 7)
021410           WS-MED-TBL-TRAN-COUNT (2, 7)
021420           WS-MED-TBL-TRAN-COUNT (3, 7)
021430           WS-MED-TBL-TRAN-COUNT (4, 7) GIVING
021440           WS-MED-TBL-TRAN-COUNT (5, 7).
021450*
021460     ADD WS-ACCURACY-ERR-01-COUNT WS-ACCURACY-ERR-02-COUNT
021470           WS-ACCURACY-ERR-03-COUNT WS-ACCURACY-ERR-06-COUNT
021480           WS-ACCURACY-ERR-08-COUNT WS-ACCURACY-ERR-09-COUNT
021490           GIVING WS-ACCURACY-TOTAL-ERR-CT.
021500*
021510     ADD WS-ACCURACY-ORDER-01-COUNT
021520           WS-ACCURACY-ORDER-02-COUNT
021530           WS-ACCURACY-ORDER-03-COUNT
021540           WS-ACCURACY-ORDER-06-COUNT
021550           WS-ACCURACY-ORDER-08-COUNT
021560           WS-ACCURACY-ORDER-09-COUNT
021570           GIVING WS-ACCURACY-TOTAL-ORDER-CT.
021580*
021590   6000-LOAD-TAPE-CODE-TBL.
021600*
021610     MOVE SPACE TO WS-T-C-TAPE-CODE.
021620     MOVE 'Y' TO WS-T-C-HIT-FLAG.
021630     MOVE +4 TO WS-T-C-MED-SUBSCR.
021640     MOVE +1 TO WS-T-C-TBL-ENTRY-COUNT.
021650     MOVE +0.00 TO WS-T-C-DUE-DLX.
021660     MOVE WS-TAPE-CODE-INFO TO WS-T-C-TBL-ENTRY (1).
021670*
021680     PERFORM
021690         WITH TEST AFTER
021700         UNTIL END-I575-FILE
021710       ADD 1 TO WS-I575-REC-COUNT
021720       IF I575-GENERAL-INFO
021730         IF I575-TAPE-CODE NOT EQUAL WS-T-C-TAPE-CODE
021740           IF WS-T-C-TBL-ENTRY-COUNT LESS +1000
021750             MOVE I575-TAPE-CODE TO WS-T-C-TAPE-CODE
021760             IF WS-T-C-TAPE-CODE (1:1) EQUAL '0'
021770               MOVE 'Y' TO WS-T-C-HIT-FLAG
021780               MOVE +1 TO WS-T-C-MED-SUBSCR
021790             ELSE
021800               MOVE SPACE TO WS-T-C-HIT-FLAG
021810               MOVE +3 TO WS-T-C-MED-SUBSCR
021820             END-IF
021830             ADD 1 TO WS-T-C-TBL-ENTRY-COUNT
021840             MOVE WS-TAPE-CODE-INFO TO WS-T-C-TBL-ENTRY
021850                   (WS-T-C-TBL-ENTRY-COUNT)
021860           ELSE
021870             DISPLAY '* TAPE CODE TABLE OVERFLOWED *' UPON SYSOUT
021880             GO TO 9900-DISPLAY-ABORT
021890         ELSE
021900           DISPLAY '* I575 SEQUENCE ERROR - MULTIPLE "A" RECORDS F
021910-                'OR TAPE CODE "' I575-TAPE-CODE '" *' UPON SYSOUT
021920           GO TO 9900-DISPLAY-ABORT
021930       ELSE
021940         ADD 1 TO WS-I575-BYPASS-COUNT
021950       END-IF
021960       PERFORM 6100-READ-I575-FILE
021970     END-PERFORM.
021980*
021990     IF WS-T-C-TBL-ENTRY-COUNT GREATER +800
022000       DISPLAY 'TAPE CODE TABLE IS OVER 80% FULL.' UPON SYSOUT
022010       MOVE 'Y' TO WS-RETURN-04-FLAG.
022020     MOVE WS-T-C-TBL-ENTRY (1) TO WS-TAPE-CODE-INFO.
022030*
022040   6100-READ-I575-FILE.
022050     READ I575-FILE
022060       AT END
022070         MOVE 'E' TO WS-I575-FILE-FLAG
022080       NOT AT END
022090         IF I575-TAPE-CODE NOT LESS WS-I575-TAPE-CODE
022100           MOVE I575-TAPE-CODE TO WS-I575-TAPE-CODE
022110         ELSE
022120           DISPLAY '* I575 SEQUENCE ERROR "' WS-I575-TAPE-CODE
022130                 '" "' I575-TAPE-CODE '" *' UPON SYSOUT
022140           GO TO 9900-DISPLAY-ABORT
022150         END-IF
022160       END-READ.
022170*
022180   7000-PASS-1.
022190     PERFORM 8100-PASS-1-INIT.
022200     PERFORM 7100-EACH-I545-REC
022210         WITH TEST AFTER
022220         UNTIL END-I545-FILE.
022230     PERFORM 9100-PASS-1-CLOSE.
022240*
022250   7100-EACH-I545-REC.
022260     ADD 1 TO WS-I545-REC-COUNT.
022270     IF I545-NON-WEEKLY-BILLING-FLAG EQUAL '1'
022280       ADD 1 TO WS-I545-BYPASS-COUNT
022290     ELSE
022300       PERFORM 7300-BUILD-IOWRK-REC.
022310     PERFORM 7200-READ-I545-FILE.
022320*
022330   7200-READ-I545-FILE.
022340     READ I545-FILE
022350       AT END
022360         MOVE 'E' TO WS-I545-FILE-FLAG
022370       NOT AT END
022380         IF I545-UPDATE-CODE EQUAL 'J' OR 'K'
022390           IF I545-NON-WEEKLY-BILLING-FLAG NOT EQUAL '2'
022400             IF I545-BILLING-DATE NOT EQUAL WS-BILLING-DATE
022410               IF WS-BILLING-DATE EQUAL LOW-VALUES
022420                 MOVE I545-BILLING-DATE TO WS-BILLING-DATE
022430                 DISPLAY 'I545 BILLING DATE = "' WS-BILLING-DATE
022440                       '".' UPON SYSOUT
022450               ELSE
022460                 DISPLAY '* I545 HAS CONFLICTING BILLING DATES "'
022470                       WS-BILLING-DATE '" AND "'
022480                       I545-BILLING-DATE '" *' UPON SYSOUT
022490                 GO TO 9900-DISPLAY-ABORT
022500               END-IF
022510             END-IF
022520           END-IF
022530         ELSE
022540           DISPLAY '* INCORRECT I545 UPDATE CODE "'
022550                 I545-UPDATE-CODE
022560                 '" ENCOUNTERED  -  MUST BE "J" OR "K" *'
022570                 UPON SYSOUT
022580           GO TO 9900-DISPLAY-ABORT
022590         END-IF
022600     END-READ.
022610*
022620   7300-BUILD-IOWRK-REC.
022630     MOVE I545-PRICING-INSTITUTION TO IOWRK-INSTITUTION.
022640     MOVE I545-NACHA-TAPE TO IOWRK-TAPE-CODE.
022650     MOVE I545-PRODUCT-BYTE-1 TO IOWRK-EXP-BYTE-1.
022660     MOVE SPACE TO IOWRK-CREDIT-FLAG IOWRK-REASON-FOR-CREDIT.
022670     IF I545-PRODUCT-CODE(3:10) EQUAL '9201031798' OR '9201031799'
022680       MOVE I545-PRODUCT-ID TO IOWRK-PRODUCT-ID
022690       MOVE I545-REPORT-BASE  TO IOWRK-REPORT-BASE
022700       MOVE I545-REPORT-DELIVERY TO IOWRK-REPORT-DELIVERY
022710     ELSE
022720       MOVE SPACE TO IOWRK-PRODUCT-ID
022730       MOVE ZEROES TO IOWRK-REPORT-BASE
022740       MOVE ZEROES TO IOWRK-REPORT-DELIVERY
022750     END-IF.
022760     IF (I545-GROSS IS NEGATIVE)
022770         OR ((I545-GROSS EQUAL +0)
022780             AND (I545-CREDIT-NO-CHARGE-FLAG (1:1) EQUAL 'C'))
022790       MOVE 'C' TO IOWRK-CREDIT-FLAG
022800       IF I545-REASON-FOR-CREDIT IS NUMERIC
022810         MOVE I545-REASON-FOR-CREDIT TO IOWRK-REASON-FOR-CREDIT.
022820     MOVE I545-RECEIVABLES-REGION TO IOWRK-REGION.
022830     MOVE I545-BILLING-DATE TO IOWRK-BILLING-DATE.
022840     MOVE SPACE TO IOWRK-NON-WEEKLY-FLAG.
022850     SUBTRACT I545-UPCHARGE FROM I545-GROSS GIVING
022860            IOWRK-ABSOLUTE-REST.
022870     IF IOWRK-ABSOLUTE-REST IS NEGATIVE
022880       MULTIPLY -1 BY IOWRK-ABSOLUTE-REST.
022890     IF I545-UPCHARGE IS NEGATIVE
022900       SUBTRACT I545-UPCHARGE FROM +0 GIVING
022910             IOWRK-ABSOLUTE-UPCHARGE
022920     ELSE
022930       MOVE I545-UPCHARGE TO IOWRK-ABSOLUTE-UPCHARGE.
022940     ADD I545-REPORT-BASE I545-REPORT-DELIVERY GIVING
022950           IOWRK-DUE-DELUXE.
022960     IF I545-TAX-REBATE-FLAG NOT EQUAL 'R'
022970       ADD I545-TAX TO IOWRK-DUE-DELUXE.
022980     IF I545-UPCHARGE-REBATE-FLAG EQUAL 'C'
022990       ADD I545-UPCHARGE TO IOWRK-DUE-DELUXE.
023000     MOVE I545-WIP-TRANS-REC-NBR TO IOWRK-ORDER-COUNTS.
023010     ADD I545-REPORT-BASE  I545-REPORT-DELIVERY GIVING
023020          IOWRK-REPORT-BASE-DELIVERY.
023030     MOVE I545-UPCHARGE TO IOWRK-UPCHRAGE.
023040     MOVE I545-GROSS TO IOWRK-GROSS.
023050     WRITE IOWRK-REC.
023060     ADD 1 TO WS-IOWRK-REC-COUNT.
023070*
023080   8000-COMMON-INIT.
023090*
023100 COPY CPY005.
023110*
023120     MOVE LS-PARM-LENGTH TO CPY108-PARM-LENGTH.
023130     MOVE LS-PARM-DATA TO CPY108-PARM-DATA.
DLXMIG* DLXMIG Changes start here
DLXMIG*    CALL 'MIS11500' USING CPY108-REC.
DLXMIG     CALL 'mis11500' USING CPY108-REC.
DLXMIG* DLXMIG Changes end here
023180     IF CPY108-PARSE-ERROR
023190       DISPLAY '* CALLED PROGRAM MIS115 ABORTED *' UPON SYSOUT
023200       GO TO 9900-DISPLAY-ABORT.
023210     PERFORM
023220           VARYING CPY108-STMT-IX FROM +1 BY +1
023230           UNTIL CPY108-STMT-IX GREATER +5
023240       IF CPY108-RET-STATEMENT (CPY108-STMT-IX) NOT EQUAL SPACE
023250         MOVE CPY108-RET-STATEMENT (CPY108-STMT-IX) TO
023260               WS-DP-STATEMENT
023270         DISPLAY WS-DISPLAY-PARM UPON SYSOUT
023280         MOVE SPACE TO WS-DP-CAPTION
023290       END-IF
023300     END-PERFORM.
023310     SET CPY108-VAL-IX TO +1.
023320     SEARCH CPY108-RET-VALUES-ENTRY
023330       AT END
023340         DISPLAY '* MISSING PARM KEYWORD "PASS      " *'
023350               UPON SYSOUT
023360         GO TO 9900-DISPLAY-ABORT
023370       WHEN CPY108-RET-KEYWORD (CPY108-VAL-IX) EQUAL 'PASS      '
023380         IF CPY108-RET-VALUE (CPY108-VAL-IX) EQUAL '1         '
023390               OR '2         '
023400           MOVE CPY108-RET-VALUE (CPY108-VAL-IX) TO
023410                 WS-PASS-NUMBER
023420           MOVE SPACE TO CPY108-RET-KEYWORD (CPY108-VAL-IX)
023430         ELSE
023440           DISPLAY '* INVALID VALUE "' CPY108-RET-VALUE
023450                 (CPY108-VAL-IX) '" FOR PARM KEYWORD "'
023460                 CPY108-RET-KEYWORD (CPY108-VAL-IX) '" *'
023470                 UPON SYSOUT
023480           GO TO 9900-DISPLAY-ABORT
023490         END-IF
023500     END-SEARCH.
023510     PERFORM
023520           VARYING CPY108-VAL-IX FROM +1 BY +1
023530           UNTIL CPY108-VAL-IX GREATER +5
023540       IF CPY108-RET-KEYWORD (CPY108-VAL-IX) NOT EQUAL SPACE
023550         MOVE 'Y' TO WS-RETURN-04-FLAG
023560         DISPLAY 'INFO ONLY: EXTRANEOUS PARM KEYWORD "'
023570               CPY108-RET-KEYWORD (CPY108-VAL-IX) '".'
023580               UPON SYSOUT
023590       END-IF
023600     END-PERFORM.
023610*
023620   8100-PASS-1-INIT.
023630     OPEN INPUT I545-FILE OUTPUT IOWRK-FILE.
023640     PERFORM 7200-READ-I545-FILE.
023650     IF END-I545-FILE
023660       DISPLAY '* I545 IS AN EMPTY FILE *' UPON SYSOUT
023670       GO TO 9900-DISPLAY-ABORT.
023680*
023690   8200-PASS-2-INIT.
023700*
023710     OPEN OUTPUT PRINTER1-FILE
023720           INPUT IOWRK-FILE
023730                 I550-FILE
023740                 I8929I-FILE
023750                 I8929P-FILE
023760                 I8929E-FILE
023770                 I575-FILE.
023780*
023790     PERFORM 4400-READ-I8929I-FILE UNTIL END-I8929I-FILE
023800     IF END-I8929I-FILE
023810       IF WS-I8929I-REC-COUNT EQUAL +0
023820         DISPLAY '* I8929I IS AN EMPTY FILE *' UPON SYSOUT
023830         MOVE 'Y' TO WS-RETURN-08-FLAG.
023840*
023850     PERFORM 4410-READ-I8929P-FILE UNTIL END-I8929P-FILE
023860     IF END-I8929P-FILE
023870       IF WS-I8929P-REC-COUNT EQUAL +0
023880         DISPLAY '* I8929P IS AN EMPTY FILE *' UPON SYSOUT
023890         MOVE 'Y' TO WS-RETURN-08-FLAG.
023900*
023910     PERFORM 4420-READ-I8929E-FILE UNTIL END-I8929E-FILE
023920     IF END-I8929I-FILE
023930       IF WS-I8929E-REC-COUNT EQUAL +0
023940         DISPLAY '* I8929E IS AN EMPTY FILE *' UPON SYSOUT
023950         MOVE 'Y' TO WS-RETURN-08-FLAG.
023960*
023970     PERFORM 4800-READ-IOWRK-FILE.
023980     IF END-IOWRK-FILE
023990       DISPLAY '* IOWRK IS AN EMPTY FILE *' UPON SYSOUT
024000       GO TO 9900-DISPLAY-ABORT.
024010     PERFORM 6100-READ-I575-FILE.
024020     IF END-I575-FILE
024030       DISPLAY '* I575 IS AN EMPTY FILE *' UPON SYSOUT
024040       GO TO 9900-DISPLAY-ABORT.
024050*
024060     MOVE WS-PROGRAM-NUMBER TO WS-HD1-PROGRAM-NUMBER.
024070     MOVE IOWRK-BILLING-DATE(1:2) TO WS-HD1-BILL-MONTH.
024080     MOVE IOWRK-BILLING-DATE(3:2) TO WS-HD1-BILL-DAY.
024090     MOVE IOWRK-BILLING-DATE(5:2) TO WS-HD1-BILL-YEAR.
024100*                * REPORT CAN BE RUN ONLY FOR A BILLING DATE THAT
024110*                 HAS ALREADY OCCURRED, NEVER FOR A FUTURE BILLING
024120*                 DATE.  GENERALLY THE REPORT WILL BE RUN IN THE
024130*                 SAME CENTURY AS THE BILLING DATE, BUT THE
024140*                 EXCEPTION OCCURS WHEN THE REPORT FOR A BILLING
024150*                 DATE LATE IN ONE CENTURY IS RUN EARLY IN THE
024160*                 NEXT CENTURY.  SO IN MOST CASES THE CENTURY
024170*                 DIGITS FOR THE BILLING DATE WILL BE THE SAME
024180*                 AS THOSE OF THE SYSTEM (RUN DATE).  THE
024190*                 EXCEPTION CAN BE DETECTED BY THE 2-DIGIT SYSTEM
024200*                 YEAR BEING LESS THAN THE 2-DIGIT BILLING YEAR.
024210*                 SINCE THE REPORT CANNOT BE ABOUT A DATE WHICH
024220*                 HASN'T YET OCCURRED, IT MUST BE ABOUT A BILLING
024230*                 DATE IN THE PREVIOUS CENTURY.  WHEN THIS DOES
024240*                 HAPPEN, THE BILLING DATE'S CENTURY DIGITS MUST
024250*                 BE LESS BY 1 THAN THE RUN/SYSTEM DATE'S.  *
024260     MOVE CPY4-CENTURY-DIGITS TO WS-HD1-BILL-CENTURY.
024270     IF WS-SYSOUT-RUN-YEAR LESS WS-HD1-BILL-YEAR
024280       SUBTRACT 1 FROM WS-HD1-BILL-CENTURY.
024290     DISPLAY 'BILLING DATE = ' WS-HD1-BILL-DATE '.' UPON SYSOUT.
024300     MOVE WS-COMPUTER-MONTH TO WS-HD1-RUN-MONTH.
024310     MOVE WS-COMPUTER-DAY TO WS-HD1-RUN-DAY.
024320     MOVE CPY4-FOUR-DIGIT-YEAR TO WS-HD1-RUN-YEAR.
024330*
024340     MOVE +0 TO WS-MED-TBL-BC-COUNT (1) WS-MED-TBL-BR-COUNT (1)
024350           WS-MED-TBL-ABS-DLRS (1) WS-MED-TBL-DUE-DELUXE (1)
024360           WS-MED-TBL-ABS-UPCH (1) WS-MED-TBL-TRAN-COUNT (1, 1)
024370           WS-MED-TBL-TRAN-COUNT (1, 2)
024380           WS-MED-TBL-TRAN-COUNT (1, 3)
024390           WS-MED-TBL-TRAN-COUNT (1, 4)
024400           WS-MED-TBL-TRAN-COUNT (1, 5)
024410           WS-MED-TBL-TRAN-COUNT (1, 6)
024420           WS-MED-TBL-TRAN-COUNT (1, 7)
024430           WS-INV-TBL-INV-BC-COUNT (1)
024440           WS-INV-TBL-DEB-BC-COUNT (1)
024450           WS-ACCURACY-TRAN-COUNT   WS-ACCURACY-ERR-01-COUNT
024460           WS-ACCURACY-ERR-02-COUNT WS-ACCURACY-ERR-03-COUNT
024470           WS-ACCURACY-ERR-06-COUNT WS-ACCURACY-ERR-08-COUNT
024480           WS-ACCURACY-ERR-09-COUNT WS-ACCURACY-TOTAL-ERR-CT
024490           WS-ACCURACY-ERR-BC-COUNT.
024500     MOVE +0 TO WS-ACCURACY-ORDER-COUNT
024510                WS-ACCURACY-ORDER-01-COUNT
024520                WS-ACCURACY-ORDER-02-COUNT
024530                WS-ACCURACY-ORDER-06-COUNT
024540                WS-ACCURACY-ORDER-09-COUNT
024550                WS-ACCURACY-ORDER-03-COUNT
024560                WS-ACCURACY-ORDER-08-COUNT
024570                WS-ACCURACY-TOTAL-ORDER-CT
024580                WS-ACCURACY-ORDER-BC-COUNT.
024590     MOVE WS-MED-TBL-MEDIUM (1) TO WS-MED-TBL-MEDIUM (2)
024600           WS-MED-TBL-MEDIUM (3) WS-MED-TBL-MEDIUM (4)
024610           WS-MED-TBL-MEDIUM (5).
024620     SET IX-WS-BC-ERR-TBL-IN TO +1.
024630     SET IX-WS-BC-ERR1-TBL-IN TO +1.
024640     SET IX-WS-BC-REST-REIM-TBL-IN TO +1.
024650*
024660   9000-COMMON-CLOSE.
024670*
024680     CLOSE IOWRK-FILE.
024690     IF RETURN-12
024700       MOVE 12 TO RETURN-CODE WS-RETURN-CODE
024710     ELSE
024720       IF RETURN-08
024730         MOVE 08 TO RETURN-CODE WS-RETURN-CODE
024740       ELSE
024750         IF RETURN-04
024760           MOVE 04 TO RETURN-CODE WS-RETURN-CODE.
024770     DISPLAY WS-RETURN-CODE-LINE UPON SYSOUT.
024780*
024790   9100-PASS-1-CLOSE.
024800*
024810     IF WS-IOWRK-REC-COUNT EQUAL +0
024820       DISPLAY '* ALL I545 RECORDS HAD NON-WEEKLY FLAG "1".  NO IO
024830-            'WRK RECORDS CREATED. *' UPON SYSOUT
024840       GO TO 9900-DISPLAY-ABORT.
024850     CLOSE I545-FILE.
024860     MOVE WS-I545-REC-COUNT TO WS-CD-I545-REC-CT.
024870     MOVE WS-I545-BYPASS-COUNT TO WS-CD-I545-BYPASS-CT.
024880     MOVE WS-IOWRK-REC-COUNT TO WS-CD-IOWRK-REC-CT.
024890     DISPLAY '=======  ' WS-PROGRAM-NUMBER ' FILE COUNTS  ======='
024900           UPON SYSOUT.
024910     DISPLAY WS-CD-I545-LINE UPON SYSOUT.
024920     DISPLAY WS-CD-IOWRK-LINE UPON SYSOUT.
024930*
024940   9200-PASS-2-CLOSE.
024950*
024960     CLOSE PRINTER1-FILE
024970           I8929I-FILE
024980           I8929P-FILE
024990           I8929E-FILE
025000           I550-FILE
025010           I575-FILE.
025020*
025030     MOVE WS-I575-REC-COUNT TO WS-CD-I575-REC-CT.
025040     MOVE WS-I8929I-REC-COUNT TO WS-CD-I8929I-REC-CT.
025050     MOVE WS-I8929P-REC-COUNT TO WS-CD-I8929P-REC-CT.
025060     MOVE WS-I8929E-REC-COUNT TO WS-CD-I8929E-REC-CT.
025070     MOVE WS-IOWRK-REC-COUNT TO WS-CD-IOWRK-REC-CT.
025080     MOVE 'READ' TO WS-CD-IOWRK-VERB.
025090     DISPLAY '=======  ' WS-PROGRAM-NUMBER ' FILE COUNTS  ======='
025100           UPON SYSOUT.
025110     DISPLAY WS-CD-I575-LINE UPON SYSOUT.
025120     DISPLAY WS-CD-IOWRK-LINE UPON SYSOUT.
025130     DISPLAY WS-CD-I8929P-LINE UPON SYSOUT.
025140     DISPLAY WS-CD-I8929I-LINE UPON SYSOUT.
025150     DISPLAY WS-CD-I8929E-LINE UPON SYSOUT.
025160   9900-DISPLAY-ABORT.
025170     DISPLAY '* ' WS-PROGRAM-NUMBER ' * RUN ABORTED *'
025180           UPON SYSOUT.
025190     MOVE 16 TO RETURN-CODE WS-RETURN-CODE.
025200     DISPLAY WS-RETURN-CODE-LINE UPON SYSOUT.
025210     STOP RUN.
025220*
025230*
