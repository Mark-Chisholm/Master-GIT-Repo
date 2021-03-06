000010  CBL SQL('QUALIFIER CICS BINDFILE')
000020 IDENTIFICATION DIVISION.
DLXMIG* DLXMIG Changes start here
DLXMIG*  PROGRAM-ID. BIL02200.
DLXMIG   PROGRAM-ID. "bil02200".
DLXMIG* DLXMIG Changes end here
000070*PROGRAM-NAME. LIST FI EXCLUSIVES.
000080   AUTHOR. JACK MASON, DAHL CONSULTING.
000090   INSTALLATION. DELUXE CORPORATION.
000100   DATE-WRITTEN. 07-14-08.
000110   DATE-COMPILED.
000120******************************************************************        
000130*
000140*RUN INSTRUCTIONS                                         07-14-08
000150*
000160*I58      = 58 INPUT FROM BIL570                       LRECL=250
000170*           UNIVERSAL PRODUCT CODE FILE
000180*
000190*I550     = 550 INPUT FROM BIL502                      LRECL=525
000200*           BILLING MASTER FILE
000210*
000220*I551     = 551 INPUT FROM BIL511             VARIABLE LRECL=1273
000230*           MASTER PRICE FILE
000240*
000250*I885     = 885 INPUT FROM BIL476                      LRECL=50
000260*           RECAP FI PRODUCT HISTORY FILE
000270*
000280*I1220P   = C1220 INPUT FROM PARMLIB                   LRECL=80
000290*           PRICE DESIGNATION CODES FOR PRICING
000300*
000310*O1475    = 1475 OUTPUT TO BIL021                      LRECL=75
000320*           FI EXCLUSIVES FILE
000330*
000340*PRINTER1 = EXCLUSIVE PRODUCTS BY BC, 132 CH/LINE (FBA LRECL=133)
000350*
000360*SYSOUT   = REVISION INFO AND ERROR MESSAGES; ANY PAPER
000370*
000380******************************************************************        
000390*
000400*RETURN CODES                                             07-14-08
000410*
000420*  16 = RUN ABORTED.  SEE SYSOUT.
000430*         - INVALID C1220 DATA
000440*         - EMPTY FILE; I58, I550 OR I885
000450*         - SEQUENCE ERROR; I58 OR I885
000460*         - TABLE OVERFLOW; 58 EXCLUSIVES OR BC EXCLUSIVES
000470*         - NO NON-OBSOLETE EXCLUSIVES FOUND ON I58 FILE.
000480*         - VSAM ERROR (EXCEPT ON CLOSE); I550 OR I551
000490*
000500*  08 = ERROR.  RUN CONTINUED.  SEE SYSOUT.
000510*         - MISSING ONE OR MORE I551 RECORDS FOR COMBINATION OF
000520*             I58 EXCLUSIVE PRODUCT / I1220P SPECIFIC PRICE
000530*             DESIGNATION; PRICES WILL BE ZERO.
000540*
000550*  04 = INFO ONLY.  RUN CONTINUED.  SEE SYSOUT.
000560*         - VSAM ERROR ON CLOSE; I550 OR I551
000570*         - TABLE OVER 80% FULL; 58 EXCLUSIVES OR BC EXCLUSIVES
000580*
000590******************************************************************        
000600*
000610*NARRATIVE                                                07-14-08
000620*
000630*PURPOSE:
000640*  TO IDENTIFY EXCLUSIVE PRODUCTS BY BC, OBTAIN THEIR 551 PRICES
000650*    (EXCLUDING ADD-ONS), TO PRINT THEM ON A REPORT, AND TO WRITE
000660*    THEM TO A 1475 FILE FOR BIL021.
000670*
000680*SOURCE OF INFORMATION:
000690*  THE C1220 CARD.
000700*  THE 58 FILE IN PRODUCT/DQ ORDER
000710*    NO SORT NEEDED - THE FILE IS MAINTAINED IN THIS ORDER
000720*      SORT FIELDS=(1,16,A).
000730*  THE 550 FILE - VSAM.
000740*  THE 551 FILE - VSAM.
000750*  THE 885 FILE IN BC/BRANCH ORDER
000760*    NO SORT NEEDED - THE FILE IS MAINTAINED IN THIS ORDER
000770*      SORT FIELDS=(1,13,A).
000780*
000790*PROGRAM FLOW:
000800*  IN INITIALIZATION, THE PROGRAM DISPLAYS REVISION INFO, OPENS
000810*    FILES, PERFORMS INITIAL READS ON SEQUENTIAL INPUTS (CHECKING
000820*    AGAINST EMPTY FILES), AND PROCESSES THE C1220 DATA (EDIT AND
000830*    INSTALL IN TABLE).
000840*  NEXT, THE PROGRAM BUILDS ITS EXCLUSIVE PRODUCTS TABLE USING THE
000850*    58 AND 551 FILES, AND THE C1220 TABLE.
000860*    -  IT CHECKS EACH I58 RECORD FOR HAVING A COMPLETE PRODUCT
000870*         CODE (NO '*'), BEING FLAGGED AS AN EXCLUSIVE, AND NOT
000880*         BEING FLAGGED AS OBSOLETE.
000890*    -  IT CREATES A ENTRY IN A TABLE FOR EACH SUCH RECORD, AND
000900*         APPENDS 551 PRICES FOR EACH C1220 PRICE DESIGNATION
000910*         PAIR.
000920*  NEXT, THE PROGRAM PROCESSES THE 885 AND 550 FILES.
000930*    -  AT THE BEGINNING OF EACH BC THAT IS ON BOTH FILES, THE
000940*         PROGRAM SAVES THE 550 FI NAME (FOR POTENTIAL REPORTING),
000950*         AND SETS AN INDEX TO INDICATE THAT THE FI EXCLUSIVES
000960*         TABLE IS EMPTY.
000970*    -  THE PROGRAM BYPASSES RECORDS ON EITHER FILE FOR BRANCHES
000980*         NOT ON THE OTHER.  THESE ARE LIKELY TO BE CLOSED
000990*         BRANCHES (885) OR NEW BRANCHES (550).
001000*    -  WHERE THE BC/BRANCH MATCHES, THE PROGRAM GETS THE GENERIC
001010*         PRICE DESIGNATION FROM THE 550 AND USES IT TO SET AN
001020*         INDEX SO THAT THE CORRECT PRICES ARE USED.
001030*    -  FOR THE DURATION OF THE BC/BRANCH MATCH, THE PROGRAM
001040*         CHECKS EACH I885 PRODUCT AGAINST THE EXCLUSIVE TABLE IT
001050*         BUILT FROM THE 58 FILE.  IF THE PRODUCT IS AN EXCLUSIVE
001060*         (EXISTS IN THAT TABLE), THE PROGRAM CHECKS ITS FI
001070*         EXCLUSIVES TABLE.  IF THE EXCLUSIVE, COMBINED WITH THE
001080*         CURRENT BRANCH'S PRICE DESIGNATION, IS NOT ALREADY IN
001090*         THAT TABLE, THE PROGRAM CREATES AND INSTALLS AN ENTRY.
001100*    -  AT THE END OF THE BC, THE PROGRAM CHECKS IF IT INSTALLED
001110*         ANY ENTRIES IN THE FI EXCLUSIVES TABLE.  IF IT DID, IT
001120*         PRINTS A REPORT OF THEM, AND CREATES 1475 RECORDS FROM
001130*         THEM.
001140*  WHEN THE 550 AND 885 FILES ARE AT THEIR ENDS, THE PROGRAM
001150*    CLOSES ALL FILES, DISPLAYS THE RETURN-CODE, AND ENDS.
001160*
001170******************************************************************        
001180*
001190*REVISION HISTORY
001200*
001210*07-14-08  PROGRAM WRITTEN.
001220*          PART OF SIMPLIFICATION PROJECT.
001230*          IMS ISSUE = JDBT-6VJSF9                  JACK MASON  01
001240*
DLXMIG**----------------------------------------------------------------    
DLXMIG**  DATE      DESCRIPTION                        INITIALS             
DLXMIG** -------  ------------------------------------------------------    
DLXMIG** 092909   UPDATED FOR DLX MIGRATION            COGNIZANT            
DLXMIG** -------  ------------------------------------------------------    
001300******************************************************************        
001310*
001320 ENVIRONMENT DIVISION.
001330 CONFIGURATION SECTION.
DLXMIG* DLXMIG Changes start here
DLXMIG*  SOURCE-COMPUTER. IBM-370.
DLXMIG   SOURCE-COMPUTER. UNIX.
DLXMIG* DLXMIG Changes end here
DLXMIG* DLXMIG Changes start here
DLXMIG*  OBJECT-COMPUTER. IBM-370.
DLXMIG   OBJECT-COMPUTER. UNIX.
DLXMIG* DLXMIG Changes end here
001420   SPECIAL-NAMES.
001430     C12 IS C12-LINE-61.
001440*
001450 INPUT-OUTPUT SECTION.
001460   FILE-CONTROL.
DLXMIG     SELECT I58-FILE      ASSIGN RSD-I58.
001480     SELECT I550-FILE     ASSIGN I550
001490                            ORGANIZATION IS INDEXED
001500                            ACCESS MODE IS SEQUENTIAL
001510                            RECORD KEY IS I550-RECORD-KEY
001520                            FILE STATUS IS WS-I550-FILE-STATUS.
001530     SELECT I551-FILE     ASSIGN I551
001540                            ORGANIZATION IS INDEXED
001550                            ACCESS MODE IS RANDOM
001560                            RECORD KEY IS I551-MP-KEY
001570                            FILE STATUS IS WS-I551-FILE-STATUS.
DLXMIG     SELECT I885-FILE     ASSIGN RSD-I885.
DLXMIG     SELECT I1220P-FILE   ASSIGN RSD-I1220P.
DLXMIG     SELECT O1475-FILE    ASSIGN RSD-O1475.
001610     SELECT PRINTER1-FILE ASSIGN PRINTER1
DLXMIG                            ORGANIZATION IS LINE SEQUENTIAL. 
001630*
001640 DATA DIVISION.
001650 FILE SECTION.
001660*
001670 FD  I58-FILE
001680*
001690 COPY T0058I.
001700*
001710 FD  I550-FILE
001720*
001730 COPY T0550I.
001740*
001750 FD  I551-FILE
001760*
001770 COPY T0551I.
001780*
001790 FD  I885-FILE
001800*
001810 COPY T0885I.
001820*
001830 FD  I1220P-FILE
001840     RECORDING MODE IS F
001850     BLOCK CONTAINS 0 RECORDS
001860     RECORD CONTAINS 80 CHARACTERS
001870     LABEL RECORDS STANDARD.
001880   01  I1220P-REC.
001890     03  I1220P-DATA.
001900       05  I1220P-PR-DESIG-PAIR  OCCURS 10 TIMES PIC X(4).
001910     03  FILLER                                  PIC X(40).
001920*
001930 FD  O1475-FILE
001940*
001950 COPY T1475O.
001960*
001970 FD  PRINTER1-FILE
001980     RECORDING MODE IS F
001990     BLOCK CONTAINS 0 RECORDS
002000     RECORD CONTAINS 132 CHARACTERS
002010     LABEL RECORDS OMITTED.
002020   01  PR1-REC                                   PIC X(132).
002030*
002040 WORKING-STORAGE SECTION.
DLXMIG* DLXMIG Changes start here
DLXMIG COPY TDLXMIGE.
DLXMIG     EXEC SQL INCLUDE SQLCA END-EXEC.
DLXMIG* DLXMIG Changes end here
002090*
002100 01  WS-FLAGS-AND-SUCH                           VALUE SPACES.
002110   03  WS-I550-FILE-STATUS        PIC X(2).
002120     88  WS-I550-SUCCESSFUL         VALUE '00'.
002130     88  WS-I550-END-OF-FILE        VALUE '10'.
002140     88  WS-I550-SUCCESSFUL-OPEN    VALUES '00', '97'.
002150   03  WS-I551-FILE-STATUS        PIC X(2).
002160     88  WS-I551-SUCCESSFUL         VALUE '00'.
002170     88  WS-I551-NO-SUCH-RECORD     VALUES '20', '23'.
002180     88  WS-I551-SUCCESSFUL-OPEN    VALUES '00', '97'.
002190   03  WS-I550-FILE-FLAG          PIC X.
002200     88  OPENED-I550-FILE           VALUE 'Y'.
002210   03  WS-I551-FILE-FLAG          PIC X.
002220     88  OPENED-I551-FILE           VALUE 'Y'.
002230   03  WS-I58-FILE-FLAG           PIC X.
002240     88  END-I58-FILE               VALUE 'E'.
002250   03  WS-I885-FILE-FLAG          PIC X.
002260     88  END-I885-FILE              VALUE 'E'.
002270   03  WS-RETURN-08-FLAG          PIC X.
002280     88  RETURN-08                  VALUE 'Y'.
002290   03  WS-RETURN-04-FLAG          PIC X.
002300     88  RETURN-04                  VALUE 'Y'.
002310*
002320 01  WS-SEQUENCES                                VALUE LOW-VALUES.
002330   03  WS-I885-SEQ                PIC X(13).
002340   03  WS-I58-SEQ.
002350     05  WS-I58-SEQ-1ST-10        PIC X(10).
002360     05  WS-I58-SEQ-SUFFIX        PIC X.
002370     05  WS-I58-SEQ-DQ            PIC X(5).
002380*
002390 01  WS-BR-INFO                                  VALUE SPACE.
002400   03  WS-BR-INF-BC-BR            PIC X(13).
002410   03  WS-BR-INF-PR-DESIG-PAIR.
002420     05  WS-BR-INF-GEN-PR-DESIG   PIC X(2).
002430     05  WS-BR-INF-SPEC-PR-DESIG  PIC X(2).
002440*
002450 01  WS-TARGET-551-KEY                           VALUE SPACE.
002460   03  FILLER                     PIC X(4).
002470   03  WS-TGT-551-PROD-DQ         PIC X(16).
002480   03  WS-TGT-551-PRICE-DESIG     PIC X(2).
002490*
002500 01  WS-PRINT-CONTROLS.
002510   03  WS-LINE-COUNT              PIC S9(3)      COMP VALUE +090.
002520   03  WS-FI-LINES                PIC S9(3)      COMP VALUE +0.
002530   03  WS-LINES-NEEDED            PIC S9(3)      COMP VALUE +0.
002540   03  WS-LINE-SPACER             PIC S9         COMP VALUE +2.
002550   03  WS-PAGE-NBR                PIC S9(5)      COMP-3 VALUE +0.
002560*
002570 01  WS-HEADINGS-ETC.
002580   03  WS-HEADING-1.
002590     05  FILLER                   PIC X          VALUE '-'.
002600     05  WS-HD1-PROGRAM-NUMBER    PIC X(8)       VALUE SPACE.
002610     05  FILLER                   PIC X(24)      VALUE '-'.
002620     05  FILLER        PIC X(47) VALUE 'EXCLUSIVE PRODUCTS BY BC'.
002630     05  FILLER                   PIC X(10)      VALUE 'RUN DATE'.
002640     05  FILLER                                VALUE '00-00-0000'.
002650       07  WS-HD1-RUN-MONTH       PIC X(2).
002660       07  FILLER                 PIC X.
002670       07  WS-HD1-RUN-DAY         PIC X(2).
002680       07  FILLER                 PIC X.
002690       07  WS-HD1-RUN-YEAR        PIC X(4).
002700     05  FILLER                   PIC X(23)      VALUE SPACE.
002710     05  FILLER                   PIC X(4)       VALUE 'PAGE'.
002720     05  FILLER                                  VALUE '    0'.
002730       07  WS-HD1-PAGE-NBR        PIC ZZZZ9.
002740   03  WS-HEADING-2.
002750     05  FILLER                   PIC X(67)      VALUE '   BC'.
002760     05  FILLER                   PIC X(65)      VALUE
002770               'PRICE       UNIT    UNIT    ADDL    ADDL    ADDL'.
002780   03  WS-HEADING-3.
002790     05  FILLER                   PIC X(14)      VALUE SPACE.
002800     05  FILLER                   PIC X(53)      VALUE
002810                       'FEB PRODUCT & DQ    STYLE    DESCRIPTION'.
002820     05  FILLER                   PIC X(65)      VALUE
002830               'DESIG       BASE   DELIV     QTY    BASE   DELIV'.
002840   03  WS-FI-HEADING                             VALUE SPACE.
002850     05  WS-FI-HDG-BC             PIC X(8).
002860     05  FILLER                   PIC X(3).
002870     05  WS-FI-HDG-FI-NAME        PIC X(35).
002880     05  FILLER                   PIC X(3).
002890     05  WS-FI-HDG-CONTINUED      PIC X(11).
002900     05  FILLER                   PIC X(72).
002910   03  WS-DETAIL-LINE.
002920     05  FILLER                   PIC X(69)      VALUE SPACE.
002930     05  FILLER                   PIC X          VALUE '/'.
002940     05  FILLER                   PIC X(62)      VALUE SPACE.
002950   03  WS-DETAIL-FIELDS REDEFINES WS-DETAIL-LINE.
002960     05  FILLER                   PIC X(13).
002970     05  WS-DTL-PROD-MAJOR        PIC X(2).
002980     05  WS-DTL-PROD-MINOR        PIC X(2).
002990     05  FILLER                   PIC X.
003000     05  WS-DTL-PROD-NUMBER       PIC X(6).
003010     05  WS-DTL-PROD-SUFFIX       PIC X.
003020     05  FILLER                   PIC X.
003030     05  WS-DTL-DESCRIPTIVE-QTY   PIC X(5).
003040     05  FILLER                   PIC X(3).
003050     05  WS-DTL-58-STYLE-CODE     PIC X(6).
003060     05  FILLER                   PIC X(3).
003070     05  WS-DTL-58-ABBREV-DESCR   PIC X(21).
003080     05  FILLER                   PIC X(3).
003090     05  WS-DTL-GENERIC-PR-DESIG  PIC X(2).
003100     05  FILLER                   PIC X.
003110     05  WS-DTL-SPECIFIC-PR-DESIG PIC X(2).
003120     05  FILLER                   PIC X.
003130     05  WS-DTL-UNIT-BASE         PIC ---,---.99.
003140     05  FILLER                   PIC X.
003150     05  WS-DTL-UNIT-DELIVERY     PIC ----.99.
003160     05  FILLER                   PIC X.
003170     05  WS-DTL-ADDL-QTY-PRICES.
003180       07  WS-DTL-ADDL-QUANTITY   PIC ---,--9.
003190       07  FILLER                 PIC X.
003200       07  WS-DTL-ADDL-QTY-BASE   PIC ----.99.
003210       07  FILLER                 PIC X.
003220       07  WS-DTL-ADDL-QTY-DELIV  PIC ----.99.
003230     05  FILLER                   PIC X(17).
003240   03  WS-PAGE-FOOTER.
003250     05  FILLER                   PIC X(44)      VALUE
003260                   '     * UNADJUSTED CURRENT 551 PRICES, EXCLUD'.
003270     05  FILLER                   PIC X(44)      VALUE
003280                   'ING POTENTIAL ADD-ONS, USING THE PRICE DESIG'.
003290     05  FILLER                   PIC X(44)      VALUE
003300                   'NATION(S) OF THE BRANCH(ES) INVOLVED *      '.
003310   03  WS-END-OF-REPORT.
003320     05  FILLER                   PIC X(20)      VALUE ALL '-'.
003330     05  FILLER                 PIC X(15) VALUE ' END OF REPORT '.
003340     05  FILLER                   PIC X(20)      VALUE ALL '-'.
003350*
003360 01  WS-BC-INFO.
003370   03  WS-BC-INF-BC               PIC X(8).
003380   03  WS-BC-INF-FI-NAME          PIC X(35).
003390*
003400 01  WS-BC-EXCL-REC.
003410   03  WS-BC-EXCL-SEQ.
003420     05  WS-BC-EXCL-PROD-DQ       PIC X(16).
003430     05  WS-BC-EXCL-PR-DESIG-PAIR PIC X(4).
003440   03  WS-BC-EXCL-PRICE-INFO      PIC X(16).
003450   03  WS-BC-EXCL-MISC-INFO       PIC X(27).
003460*
003470 01  WS-58-551-REC.
003480   03  WS-58-551-FIXED-INFO.
003490     05  WS-58-551-PROD-DQ        PIC X(16).
003500     05  WS-58-MISC-INFO.
003510       07  WS-58-ABBREV-DESCR     PIC X(21).
003520       07  WS-58-STYLE-CODE       PIC X(6).
003530   03  WS-551-PRICE-TABLE.
003540     05  WS-551-PR-DESIG-PRICES   OCCURS 10 TIMES
003550                                  INDEXED BY IX-WS-551-PR-DESIG.
003560       07  WS-551-UNIT-BASE       PIC S9(5)V9(2) COMP-3.
003570       07  WS-551-UNIT-DELIV      PIC S9(3)V9(2) COMP-3.
003580       07  WS-551-ADDL-QTY        PIC S9(5)      COMP-3.
003590       07  WS-551-ADDL-BASE       PIC S9(3)V9(2) COMP-3.
003600       07  WS-551-ADDL-DELIV      PIC S9(3)V9(2) COMP-3.
003610*
003620 01  WS-C1220-TABLE.
003630   03  WS-C1220-ENTRY             OCCURS 10 TIMES
003640                                  INDEXED BY
003650                                      IX-WS-C1220
003660                                      IX-WS-C1220-MAX.
003670     05  WS-C1220-GEN-PR-DESIG.
003680       07  WS-C1220-GEN-PR-D-1    PIC X.
003690       07  WS-C1220-GEN-PR-D-2    PIC X.
003700     05  WS-C1220-SPEC-PR-DESIG.
003710       07  WS-C1220-SPEC-PR-D-1   PIC X.
003720       07  WS-C1220-SPEC-PR-D-2   PIC X.
003730*
003740 01  WS-BC-EXCL-TABLE.
003750   03  WS-BC-EXCL-TBL-ENTRY       OCCURS 500 TIMES
003760                                  INDEXED BY
003770                                      IX-WS-BC-EXCL-TBL
003780                                      IX-WS-BC-EXCL-TBL-EMPTY
003790                                      IX-WS-BC-EXCL-TBL-FROM
003800                                      IX-WS-BC-EXCL-TBL-TO.
003810     05  WS-BC-EXCL-TBL-SEQ       PIC X(20).
003820     05  FILLER                   PIC X(43).
003830*
003840 01  WS-58-551-TABLE.
003850   03  WS-58-551-ENTRY-COUNT      PIC S9(4)      COMP.
003860   03  WS-58-551-TBL-ENTRY        OCCURS 0 TO 2000 TIMES
003870                                  DEPENDING ON
003880                                      WS-58-551-ENTRY-COUNT
003890                                  ASCENDING KEY IS
003900                                      WS-58-551-TBL-PROD-DQ
003910                                  INDEXED BY IX-WS-58-551-TBL.
003920     05  WS-58-551-TBL-PROD-DQ    PIC X(16).
003930     05  FILLER                   PIC X(187).
003940*
003950 01  WS-INIT-58-551-ENTRY         PIC X(203).
003960*
003970 01  WS-RETURN-CODE-LINE.
003980   03  FILLER                    PIC X(14) VALUE 'RETURN CODE = '.
003990   03  WS-RETURN-CODE             PIC 9(2)       DISPLAY VALUE 00.
004000   03  FILLER                     PIC X          VALUE '.'.
004010*
004020 COPY CPY004.
004030*
004040 01  WS-PROGRAM-NUMBER            PIC X(8)       VALUE 'BIL02201'.
004050 01  WS-REVISION-DATE             PIC X(8)       VALUE '07-14-08'.
004060*
004070 PROCEDURE DIVISION.
004080*
DLXMIG 0000-SQL-DB-CONNECT.
DLXMIG     MOVE 'DLXVSAM' TO DBNAME.
DLXMIG COPY CPYMIGUD.
004120   0000-MAIN-LINE.
004130     PERFORM 8000-INITIALIZE.
004140     PERFORM 5000-LOAD-58-551-TABLE.
004150     PERFORM 1000-EACH-BC
004160         WITH TEST AFTER
004170         UNTIL END-I885-FILE OR WS-I550-END-OF-FILE.
004180     PERFORM 9000-CLOSE.
004190     STOP RUN.
004200*
004210*
004220   1000-EACH-BC.
004230     IF I885-BC EQUAL I550-BC
004240       MOVE I550-BC TO WS-BC-INF-BC
004250       MOVE I550-FI-NAME TO WS-BC-INF-FI-NAME
004260       SET IX-WS-BC-EXCL-TBL-EMPTY TO +1
004270       PERFORM 1100-EACH-BRANCH
004280           WITH TEST AFTER
004290           UNTIL WS-I550-END-OF-FILE
004300               OR END-I885-FILE
004310               OR (I885-BC NOT EQUAL WS-BC-INF-BC)
004320               OR (I550-BC NOT EQUAL WS-BC-INF-BC)
004330       IF IX-WS-BC-EXCL-TBL-EMPTY GREATER +1
004340         IF IX-WS-BC-EXCL-TBL-EMPTY GREATER +400
004350           DISPLAY 'BC EXCLUSIVE TABLE OVER 80% FULL FOR BC '
004360                 WS-BC-INF-BC '.' UPON SYSOUT
004370           MOVE 'Y' TO WS-RETURN-04-FLAG
004380         END-IF
004390         PERFORM 2000-FINISH-BC-W-EXCL
004400       END-IF
004410     ELSE
004420       IF I885-BC LESS I550-BC
004430         PERFORM 1300-READ-I885-FILE
004440             WITH TEST AFTER
004450             UNTIL END-I885-FILE
004460                 OR (I885-BC NOT LESS I550-BC)
004470       ELSE
004480         PERFORM 1200-READ-I550-FILE-NEXT
004490             WITH TEST AFTER
004500             UNTIL WS-I550-END-OF-FILE
004510                 OR (I550-BC NOT LESS I885-BC).
004520*
004530   1100-EACH-BRANCH.
004540     IF I550-RECORD-KEY (1:13) EQUAL WS-I885-SEQ
004550       MOVE WS-I885-SEQ TO WS-BR-INF-BC-BR
004560       IF I550-NORMAL-PRICE-DESIGNATION NOT EQUAL
004570             WS-BR-INF-GEN-PR-DESIG
004580         MOVE I550-NORMAL-PRICE-DESIGNATION TO
004590               WS-BR-INF-GEN-PR-DESIG
004600         MOVE SPACE TO
004610               WS-BR-INF-SPEC-PR-DESIG
004620         SET IX-WS-C1220 TO +1
004630         PERFORM
004640             WITH TEST AFTER
004650             UNTIL (WS-BR-INF-SPEC-PR-DESIG NOT EQUAL SPACE)
004660                 OR (IX-WS-C1220 GREATER IX-WS-C1220-MAX)
004670           IF WS-C1220-GEN-PR-DESIG (IX-WS-C1220) EQUAL
004680                 WS-BR-INF-GEN-PR-DESIG
004690             MOVE WS-C1220-SPEC-PR-DESIG (IX-WS-C1220) TO
004700                   WS-BR-INF-SPEC-PR-DESIG
004710             SET IX-WS-551-PR-DESIG TO IX-WS-C1220
004720           ELSE
004730             SET IX-WS-C1220 UP BY 1
004740           END-IF
004750         END-PERFORM
004760       END-IF
004770       PERFORM 1500-EACH-I885-REC
004780           WITH TEST AFTER
004790           UNTIL END-I885-FILE
004800               OR (WS-I885-SEQ GREATER WS-BR-INF-BC-BR)
004810       PERFORM 1200-READ-I550-FILE-NEXT
004820     ELSE
004830       IF I550-RECORD-KEY (1:13) GREATER WS-I885-SEQ
004840         PERFORM 1300-READ-I885-FILE
004850             WITH TEST AFTER
004860             UNTIL END-I885-FILE
004870                 OR (WS-I885-SEQ NOT LESS I550-RECORD-KEY (1:13))
004880       ELSE
004890         PERFORM 1200-READ-I550-FILE-NEXT
004900             WITH TEST AFTER
004910             UNTIL WS-I550-END-OF-FILE
004920                 OR (I550-RECORD-KEY (1:13) NOT LESS WS-I885-SEQ).
004930*
004940   1200-READ-I550-FILE-NEXT.
004950     READ I550-FILE NEXT RECORD.
004960     IF (NOT WS-I550-SUCCESSFUL)
004970         AND (NOT WS-I550-END-OF-FILE)
004980       DISPLAY '* VSAM ERROR ON I550 READ.  FILE STATUS = '
004990             WS-I550-FILE-STATUS '. *' UPON SYSOUT
005000       GO TO 9900-DISPLAY-ABORT.
005010*
005020   1300-READ-I885-FILE.
005030     READ I885-FILE
005040       AT END
005050         MOVE 'E' TO WS-I885-FILE-FLAG
005060       NOT AT END
005070         IF I885-PRICING-INSTITUTION (1:13) LESS WS-I885-SEQ
005080           DISPLAY '* I885 SEQUENCE ERROR "' WS-I885-SEQ '" "'
005090                 I885-PRICING-INSTITUTION (1:13) '" *' UPON SYSOUT
005100           GO TO 9900-DISPLAY-ABORT
005110         ELSE
005120           MOVE I885-PRICING-INSTITUTION (1:13) TO WS-I885-SEQ
005130         END-IF
005140     END-READ.
005150*
005160   1500-EACH-I885-REC.
005170     SEARCH ALL WS-58-551-TBL-ENTRY
005180       WHEN WS-58-551-TBL-PROD-DQ (IX-WS-58-551-TBL) EQUAL
005190             I885-PRODUCT-ID (3:16)
005200         MOVE WS-58-551-TBL-ENTRY (IX-WS-58-551-TBL) TO
005210               WS-58-551-REC
005220         PERFORM 1600-I885-EXCLUSIVE-REC.
005230     PERFORM 1300-READ-I885-FILE.
005240*
005250   1600-I885-EXCLUSIVE-REC.
005260     MOVE WS-58-551-PROD-DQ TO WS-BC-EXCL-PROD-DQ.
005270     MOVE WS-BR-INF-PR-DESIG-PAIR TO WS-BC-EXCL-PR-DESIG-PAIR.
005280     IF WS-BR-INF-SPEC-PR-DESIG EQUAL SPACE
005290       MOVE WS-INIT-58-551-ENTRY (44:16) TO WS-BC-EXCL-PRICE-INFO
005300     ELSE
005310       MOVE WS-551-PR-DESIG-PRICES (IX-WS-551-PR-DESIG) TO
005320             WS-BC-EXCL-PRICE-INFO.
005330     MOVE WS-58-MISC-INFO TO WS-BC-EXCL-MISC-INFO.
005340     SET IX-WS-BC-EXCL-TBL TO +1.
005350     PERFORM
005360         WITH TEST BEFORE
005370         UNTIL (IX-WS-BC-EXCL-TBL NOT LESS
005380               IX-WS-BC-EXCL-TBL-EMPTY)
005390             OR (WS-BC-EXCL-TBL-SEQ (IX-WS-BC-EXCL-TBL) NOT LESS
005400                   WS-BC-EXCL-SEQ)
005410       SET IX-WS-BC-EXCL-TBL UP BY 1
005420     END-PERFORM.
005430     IF (IX-WS-BC-EXCL-TBL NOT LESS IX-WS-BC-EXCL-TBL-EMPTY)
005440         OR (WS-BC-EXCL-TBL-SEQ (IX-WS-BC-EXCL-TBL) NOT EQUAL
005450               WS-BC-EXCL-SEQ)
005460*                * FIRST TIME FOR THIS BC/PROD/DQ/PD - INSTALL *
005470       IF IX-WS-BC-EXCL-TBL-EMPTY GREATER +500
005480         DISPLAY '* BC EXCLUSIVE TABLE OVERFLOWED FOR BC '
005490               WS-BC-INF-BC ' *' UPON SYSOUT
005500         GO TO 9900-DISPLAY-ABORT
005510       END-IF
005520       IF IX-WS-BC-EXCL-TBL LESS IX-WS-BC-EXCL-TBL-EMPTY
005530*                * BELONGS OTHER THAN AT END - SHIFT ENTRIES *
005540         SET IX-WS-BC-EXCL-TBL-FROM TO IX-WS-BC-EXCL-TBL-EMPTY
005550         PERFORM
005560             WITH TEST AFTER
005570             UNTIL IX-WS-BC-EXCL-TBL-FROM NOT GREATER
005580                   IX-WS-BC-EXCL-TBL
005590           SET IX-WS-BC-EXCL-TBL-TO TO IX-WS-BC-EXCL-TBL-FROM
005600           SET IX-WS-BC-EXCL-TBL-FROM DOWN BY 1
005610           MOVE WS-BC-EXCL-TBL-ENTRY (IX-WS-BC-EXCL-TBL-FROM) TO
005620                 WS-BC-EXCL-TBL-ENTRY (IX-WS-BC-EXCL-TBL-TO)
005630         END-PERFORM
005640       END-IF
005650       MOVE WS-BC-EXCL-REC TO
005660             WS-BC-EXCL-TBL-ENTRY (IX-WS-BC-EXCL-TBL)
005670       SET IX-WS-BC-EXCL-TBL-EMPTY UP BY 1.
005680*
005690*
005700   2000-FINISH-BC-W-EXCL.
005710     PERFORM 3500-INIT-BC-REPORT.
005720     SET IX-WS-BC-EXCL-TBL TO +1.
005730     PERFORM
005740         WITH TEST AFTER
005750         UNTIL IX-WS-BC-EXCL-TBL NOT LESS IX-WS-BC-EXCL-TBL-EMPTY
005760       MOVE WS-BC-INF-BC TO O1475-BC
005770       MOVE WS-BC-EXCL-TBL-ENTRY (IX-WS-BC-EXCL-TBL) TO
005780             O1475-REC (9:63)
005790       MOVE SPACE TO O1475-REC (72:4)
005800       PERFORM 3000-PRINT-DETAIL-LINE
005810       WRITE O1475-REC
005820       SET IX-WS-BC-EXCL-TBL UP BY 1
005830     END-PERFORM.
005840*
005850*
005860   3000-PRINT-DETAIL-LINE.
005870     IF WS-LINE-COUNT GREATER +59
005880       MOVE '(CONTINUED)' TO WS-FI-HDG-CONTINUED
005890       PERFORM 3300-HEAD-PR1-PAGE
005900       PERFORM 3200-PRINT-FI-HEADING.
005910     MOVE O1475-PROD-MAJOR TO WS-DTL-PROD-MAJOR.
005920     MOVE O1475-PROD-MINOR TO WS-DTL-PROD-MINOR.
005930     MOVE O1475-PROD-NUMBER TO WS-DTL-PROD-NUMBER.
005940     MOVE O1475-PROD-SUFFIX TO WS-DTL-PROD-SUFFIX.
005950     MOVE O1475-DESCRIPTIVE-QTY TO WS-DTL-DESCRIPTIVE-QTY.
005960     MOVE O1475-GENERIC-PR-DESIG TO WS-DTL-GENERIC-PR-DESIG.
005970     MOVE O1475-SPECIFIC-PR-DESIG TO WS-DTL-SPECIFIC-PR-DESIG.
005980     MOVE O1475-UNIT-BASE TO WS-DTL-UNIT-BASE.
005990     MOVE O1475-UNIT-DELIVERY TO WS-DTL-UNIT-DELIVERY.
006000     IF O1475-ADDITIONAL-QUANTITY NOT EQUAL +0
006010       MOVE O1475-ADDITIONAL-QUANTITY TO WS-DTL-ADDL-QUANTITY
006020       MOVE O1475-ADDL-QTY-BASE TO WS-DTL-ADDL-QTY-BASE
006030       MOVE O1475-ADDL-QTY-DELIVERY TO WS-DTL-ADDL-QTY-DELIV
006040     ELSE
006050       MOVE SPACE TO WS-DTL-ADDL-QTY-PRICES.
006060     MOVE O1475-58-ABBREV-DESCR TO WS-DTL-58-ABBREV-DESCR.
006070     MOVE O1475-58-STYLE-CODE TO WS-DTL-58-STYLE-CODE.
006080     MOVE WS-DETAIL-LINE TO PR1-REC.
006090     PERFORM 3100-WRITE-PR1-REC.
006100*
006110   3100-WRITE-PR1-REC.
006120     WRITE PR1-REC AFTER WS-LINE-SPACER.
006130     ADD WS-LINE-SPACER TO WS-LINE-COUNT.
006140*
006150   3200-PRINT-FI-HEADING.
006160     MOVE WS-FI-HEADING TO PR1-REC.
006170     MOVE +2 TO WS-LINE-SPACER.
006180     PERFORM 3100-WRITE-PR1-REC.
006190     MOVE +1 TO WS-LINE-SPACER.
006200*
006210   3300-HEAD-PR1-PAGE.
006220     IF WS-PAGE-NBR NOT EQUAL +0
006230       PERFORM 3400-FOOT-PR1-PAGE.
006240     ADD 1 TO WS-PAGE-NBR.
006250     MOVE WS-PAGE-NBR TO WS-HD1-PAGE-NBR.
006260     WRITE PR1-REC FROM WS-HEADING-1 AFTER PAGE.
006270     MOVE +1 TO WS-LINE-COUNT.
006280     MOVE +2 TO WS-LINE-SPACER.
006290     MOVE WS-HEADING-2 TO PR1-REC.
006300     PERFORM 3100-WRITE-PR1-REC.
006310     MOVE +1 TO WS-LINE-SPACER.
006320     MOVE WS-HEADING-3 TO PR1-REC.
006330     PERFORM 3100-WRITE-PR1-REC.
006340*
006350   3400-FOOT-PR1-PAGE.
DLXMIG*    WRITE PR1-REC FROM WS-PAGE-FOOTER AFTER C12-LINE-61.
DLXMIG     WRITE PR1-REC FROM WS-PAGE-FOOTER AFTER 12 LINES. 
006380     MOVE +61 TO WS-LINE-COUNT.
006390*
006400   3500-INIT-BC-REPORT.
006410     MOVE WS-BC-INF-BC TO WS-FI-HDG-BC.
006420     MOVE WS-BC-INF-FI-NAME TO WS-FI-HDG-FI-NAME.
006430     MOVE SPACE TO WS-FI-HDG-CONTINUED.
006440     SET WS-FI-LINES TO IX-WS-BC-EXCL-TBL-EMPTY.
006450*                * ONE MORE THAN COUNT OF PRODUCT LINES *
006460     ADD 1 TO WS-FI-LINES.
006470*                * TWO MORE, ALLOWING FOR DOUBLE-SPACED BC LINE *
006480     ADD WS-LINE-COUNT WS-FI-LINES GIVING WS-LINES-NEEDED.
006490     IF (WS-LINES-NEEDED GREATER +60)
006500*                * WON'T ALL FIT ON CURRENT PAGE *
006510         AND (((WS-FI-LINES LESS +56)
006520*                * WOULD ALL FIT ON A SEPARATE PAGE *
006530                 AND (WS-LINE-COUNT GREATER +49))
006540*                * CURRENT PAGE FITS LESS THAN TEN PRODUCTS *
006550             OR (WS-LINE-COUNT GREATER +56))
006560*                * CURRENT PAGE FITS LESS THAN THREE PRODUCTS *
006570       PERFORM 3300-HEAD-PR1-PAGE.
006580     PERFORM 3200-PRINT-FI-HEADING.
006590*
006600*
006610   5000-LOAD-58-551-TABLE.
006620     MOVE SPACE TO WS-58-551-REC.
006630     MOVE +0 TO WS-551-UNIT-BASE (1) WS-551-UNIT-DELIV (1)
006640           WS-551-ADDL-QTY (1) WS-551-ADDL-BASE (1)
006650           WS-551-ADDL-DELIV (1).
006660     MOVE WS-551-PR-DESIG-PRICES (1) TO WS-551-PR-DESIG-PRICES (2)
006670           WS-551-PR-DESIG-PRICES (3) WS-551-PR-DESIG-PRICES (4)
006680           WS-551-PR-DESIG-PRICES (5) WS-551-PR-DESIG-PRICES (6)
006690           WS-551-PR-DESIG-PRICES (7) WS-551-PR-DESIG-PRICES (8)
006700           WS-551-PR-DESIG-PRICES (9) WS-551-PR-DESIG-PRICES (10).
006710     MOVE WS-58-551-REC TO WS-INIT-58-551-ENTRY.
006720     MOVE +0 TO WS-58-551-ENTRY-COUNT.
006730     PERFORM
006740         WITH TEST AFTER
006750         UNTIL END-I58-FILE
006760       IF I58-EXCLUSIVE-PRODUCT
006770           AND (NOT I58-OBSOLETE-PRODUCT)
006780           AND (WS-I58-SEQ-1ST-10 IS NUMERIC)
006790           AND (WS-I58-SEQ-SUFFIX NOT EQUAL '*' AND SPACE)
006800         PERFORM 5200-BUILD-58-551-ENTRY
006810       END-IF
006820       PERFORM 5100-READ-I58-FILE
006830     END-PERFORM.
006840     IF WS-58-551-ENTRY-COUNT EQUAL +0
006850       DISPLAY '* NO NON-OBSOLETE EXCLUSIVES ON I58 FILE *'
006860             UPON SYSOUT
006870       GO TO 9900-DISPLAY-ABORT.
006880     IF WS-58-551-ENTRY-COUNT GREATER +1600
006890       DISPLAY 'I58 EXCLUSIVE TABLE OVER 80% FULL.' UPON SYSOUT
006900       MOVE 'Y' TO WS-RETURN-04-FLAG.
006910*
006920   5100-READ-I58-FILE.
006930     READ I58-FILE
006940       AT END
006950         MOVE 'E' TO WS-I58-FILE-FLAG
006960       NOT AT END
006970         IF I58-PRODUCT-CODE GREATER WS-I58-SEQ
006980           MOVE I58-PRODUCT-CODE TO WS-I58-SEQ
006990         ELSE
007000           DISPLAY '* I58 SEQUENCE ERROR "' WS-I58-SEQ '" "'
007010                 I58-PRODUCT-CODE '" *' UPON SYSOUT
007020           GO TO 9900-DISPLAY-ABORT
007030         END-IF
007040     END-READ.
007050*
007060   5200-BUILD-58-551-ENTRY.
007070     IF WS-58-551-ENTRY-COUNT NOT LESS +2000
007080       DISPLAY '* I58 EXCLUSIVE TABLE OVERFLOWED *' UPON SYSOUT
007090       GO TO 9900-DISPLAY-ABORT.
007100     MOVE WS-INIT-58-551-ENTRY TO WS-58-551-REC.
007110     MOVE I58-PRODUCT-CODE TO WS-58-551-PROD-DQ
007120           WS-TGT-551-PROD-DQ.
007130     MOVE I58-ABBREVIATED-PROD-CODE-DESC TO WS-58-ABBREV-DESCR.
007140     MOVE I58-STYLES-FILE-CODE TO WS-58-STYLE-CODE.
007150     SET IX-WS-551-PR-DESIG IX-WS-C1220 TO +1.
007160     PERFORM
007170         WITH TEST AFTER
007180         UNTIL IX-WS-C1220 GREATER IX-WS-C1220-MAX
007190       MOVE WS-C1220-SPEC-PR-DESIG (IX-WS-C1220) TO
007200             WS-TGT-551-PRICE-DESIG
007210       MOVE WS-TARGET-551-KEY TO I551-MP-KEY
007220       READ I551-FILE
007230       IF WS-I551-SUCCESSFUL
007240         MOVE I551-MP-FIXED-DATA-FIELDS (1:16) TO
007250               WS-551-PR-DESIG-PRICES (IX-WS-551-PR-DESIG)
007260       ELSE
007270         IF WS-I551-NO-SUCH-RECORD
007280           DISPLAY 'MISSING I551 RECORD FOR KEY "'
007290                 WS-TARGET-551-KEY '".' UPON SYSOUT
007300           MOVE 'Y' TO WS-RETURN-08-FLAG
007310         ELSE
007320           DISPLAY '* VSAM ERROR ON I551 READ.  FILE STATUS = '
007330                 WS-I551-FILE-STATUS '.  KEY = "'
007340                 WS-TARGET-551-KEY '" *' UPON SYSOUT
007350           GO TO 9900-DISPLAY-ABORT
007360         END-IF
007370       END-IF
007380       SET IX-WS-551-PR-DESIG IX-WS-C1220 UP BY 1
007390     END-PERFORM.
007400     ADD 1 TO WS-58-551-ENTRY-COUNT.
007410     MOVE WS-58-551-REC TO
007420           WS-58-551-TBL-ENTRY (WS-58-551-ENTRY-COUNT).
007430*
007440*
007450   8000-INITIALIZE.
007460*
007470 COPY CPY005.
007480*
007490     OPEN OUTPUT O1475-FILE PRINTER1-FILE
007500           INPUT I1220P-FILE I885-FILE I58-FILE I551-FILE.
007510     IF NOT WS-I551-SUCCESSFUL-OPEN
007520       DISPLAY '* VSAM ERROR ON I551 OPEN.  FILE STATUS = '
007530             WS-I551-FILE-STATUS '. *' UPON SYSOUT
007540       GO TO 9900-DISPLAY-ABORT.
007550     MOVE 'Y' TO WS-I551-FILE-FLAG.
007560     OPEN INPUT I550-FILE.
007570     IF NOT WS-I550-SUCCESSFUL-OPEN
007580       DISPLAY '* VSAM ERROR ON I550 OPEN.  FILE STATUS = '
007590             WS-I550-FILE-STATUS '. *' UPON SYSOUT
007600       GO TO 9900-DISPLAY-ABORT.
007610     MOVE 'Y' TO WS-I550-FILE-FLAG.
007620     PERFORM 1200-READ-I550-FILE-NEXT.
007630     IF WS-I550-END-OF-FILE
007640       DISPLAY '* I550 IS AN EMPTY FILE *' UPON SYSOUT
007650       GO TO 9900-DISPLAY-ABORT.
007660     PERFORM 1300-READ-I885-FILE.
007670     IF END-I885-FILE
007680       DISPLAY '* I885 IS AN EMPTY FILE *' UPON SYSOUT
007690       GO TO 9900-DISPLAY-ABORT.
007700     PERFORM 5100-READ-I58-FILE.
007710     IF END-I58-FILE
007720       DISPLAY '* I58 IS AN EMPTY FILE *' UPON SYSOUT
007730       GO TO 9900-DISPLAY-ABORT.
007740     READ I1220P-FILE
007750       AT END
007760         DISPLAY '* I1220P IS AN EMPTY FILE *' UPON SYSOUT
007770         GO TO 9900-DISPLAY-ABORT.
007780     PERFORM 8100-EDIT-LOAD-C1220-DATA.
007790     MOVE WS-PROGRAM-NUMBER TO WS-HD1-PROGRAM-NUMBER.
007800     MOVE CPY4-CURRENT-DATE (5:2) TO WS-HD1-RUN-MONTH.
007810     MOVE CPY4-CURRENT-DATE (7:2) TO WS-HD1-RUN-DAY.
007820     MOVE CPY4-CURRENT-DATE (1:4) TO WS-HD1-RUN-YEAR.
007830*
007840   8100-EDIT-LOAD-C1220-DATA.
007850     IF I1220P-PR-DESIG-PAIR (01) EQUAL SPACE
007860       DISPLAY '* FIRST I1220P PAIR IS BLANK *' UPON SYSOUT
007870       GO TO 9900-DISPLAY-ABORT.
007880     MOVE I1220P-DATA TO WS-C1220-TABLE.
007890     SET IX-WS-C1220-MAX TO +10.
007900     PERFORM
007910         WITH TEST BEFORE
007920         UNTIL WS-C1220-ENTRY (IX-WS-C1220-MAX) NOT EQUAL SPACE
007930       SET IX-WS-C1220-MAX DOWN BY 1
007940     END-PERFORM.
007950     SET IX-WS-C1220 TO +1.
007960     PERFORM
007970         WITH TEST AFTER
007980         UNTIL IX-WS-C1220 GREATER IX-WS-C1220-MAX
007990       IF WS-C1220-ENTRY (IX-WS-C1220) EQUAL SPACE
008000         DISPLAY '* I1220P CONTAINS IMBEDDED BLANK PAIR *'
008010               UPON SYSOUT
008020         GO TO 9900-DISPLAY-ABORT
008030       ELSE
008040         IF SPACE EQUAL WS-C1220-GEN-PR-D-1 (IX-WS-C1220)
008050               OR WS-C1220-GEN-PR-D-2 (IX-WS-C1220)
008060               OR WS-C1220-SPEC-PR-D-1 (IX-WS-C1220)
008070               OR WS-C1220-SPEC-PR-D-2 (IX-WS-C1220)
008080           DISPLAY '* INVALID I1220P PAIR "'
008090                 WS-C1220-ENTRY (IX-WS-C1220) '" *' UPON SYSOUT
008100           GO TO 9900-DISPLAY-ABORT
008110         END-IF
008120       END-IF
008130       SET IX-WS-C1220 UP BY 1
008140     END-PERFORM.
008150*
008160*
008170   9000-CLOSE.
008180     PERFORM 1300-READ-I885-FILE
008190         WITH TEST BEFORE
008200         UNTIL END-I885-FILE.
008210     IF WS-PAGE-NBR NOT EQUAL +0
008220       PERFORM 3400-FOOT-PR1-PAGE
008230       MOVE WS-END-OF-REPORT TO PR1-REC
008240       MOVE +2 TO WS-LINE-SPACER
008250       PERFORM 3100-WRITE-PR1-REC.
008260     CLOSE PRINTER1-FILE O1475-FILE I885-FILE I58-FILE
008270           I1220P-FILE.
008280     PERFORM 9100-CLOSE-I551-FILE.
008290     IF NOT WS-I551-SUCCESSFUL
008300       DISPLAY 'VSAM ERROR ON I551 CLOSE.  FILE STATUS = '
008310             WS-I551-FILE-STATUS '.' UPON SYSOUT
008320       MOVE 'Y' TO WS-RETURN-04-FLAG.
008330     PERFORM 9200-CLOSE-I550-FILE.
008340     IF NOT WS-I550-SUCCESSFUL
008350       DISPLAY 'VSAM ERROR ON I550 CLOSE.  FILE STATUS = '
008360             WS-I550-FILE-STATUS '.' UPON SYSOUT
008370       MOVE 'Y' TO WS-RETURN-04-FLAG.
008380     IF RETURN-08
008390       MOVE 08 TO RETURN-CODE WS-RETURN-CODE
008400     ELSE
008410       IF RETURN-04
008420         MOVE 04 TO RETURN-CODE WS-RETURN-CODE.
008430     DISPLAY WS-RETURN-CODE-LINE UPON SYSOUT.
008440*
008450   9100-CLOSE-I551-FILE.
008460     MOVE SPACE TO WS-I551-FILE-FLAG.
008470     CLOSE I551-FILE.
008480*
008490   9200-CLOSE-I550-FILE.
008500     MOVE SPACE TO WS-I550-FILE-FLAG.
008510     CLOSE I550-FILE.
008520*
008530   9900-DISPLAY-ABORT.
008540     DISPLAY '* ' WS-PROGRAM-NUMBER ' * RUN ABORTED *'
008550           UPON SYSOUT.
008560     IF OPENED-I550-FILE
008570       PERFORM 9200-CLOSE-I550-FILE.
008580     IF OPENED-I551-FILE
008590       PERFORM 9100-CLOSE-I551-FILE.
008600     MOVE 16 TO RETURN-CODE WS-RETURN-CODE.
008610     DISPLAY WS-RETURN-CODE-LINE UPON SYSOUT.
008620     STOP RUN.
008630*
008640*
