       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.
       AUTHOR. Athos Exarchou.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           *> each record corresponds to a line of text (CSV)
           SELECT EMP-FILE ASSIGN TO WS-INPUT-FILE
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS WS-EMP-FILE-STATUS.

           SELECT RPT-FILE ASSIGN TO WS-OUTPUT-FILE
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS WS-RPT-FILE-STATUS.

           SELECT ERR-FILE ASSIGN TO WS-ERROR-FILE
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS WS-ERR-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  EMP-FILE.
       01  EMP-REC PIC X(132). *> reads whole line (CSV)

       FD  RPT-FILE.
       01  RPT-REC PIC X(132).

       FD  ERR-FILE.
       01  ERR-REC PIC X(132).

       WORKING-STORAGE SECTION.
       
       *> Fixed-width text fields from input (used after UNSTRING)
       01 WS-FIELDS.
           05 WS-EMP-ID-TEXT    PIC X(4).
           05 WS-EMP-NAME-TEXT  PIC X(20).
           05 WS-EMP-HOURS-TEXT PIC X(3).
           *> expects numeric values as text, e.g., "15.50"
           05 WS-EMP-RATE-TEXT PIC X(6).

       *> file status variables
       77 WS-EMP-FILE-STATUS PIC XX.
       77 WS-RPT-FILE-STATUS PIC XX.
       77 WS-ERR-FILE-STATUS PIC XX.

       77 WS-INPUT-FILE  PIC X(50) VALUE SPACES.
       77 WS-OUTPUT-FILE PIC X(50) VALUE SPACES.
       77 WS-ERROR-FILE  PIC X(50) VALUE "error.log".

       *> error logging
       77 WS-ERROR-MSG       PIC X(80).
       77 WS-SKIP-REC        PIC X VALUE 'N'. *> 'Y' = skip this record
       77 WS-I               PIC 9(2).
       77 WS-CHAR            PIC X(1).
       77 WS-DIGITS          PIC 9(2) VALUE 0.
       77 WS-DOT             PIC 9(2) VALUE 0.

       *> counters
       77 WS-REC-COUNT       PIC 9(6) VALUE 0.

       *> Numeric fields
       77 WS-EMP-ID    PIC 9(4). *> integer
       77 WS-EMP-HOURS PIC 9(3). *> integer
       77 WS-RATE      PIC 9(3)V99. *> 3 digits before decimal, 2 after
       77 WS-PAY       PIC 9(6)V99 COMP-3 VALUE 0. *> Binary-coded decimal
       77 WS-TOTAL-PAY PIC 9(8)V99 COMP-3 VALUE 0. *> Binary-coded decimal

       *> Edited fields for display
       77 WS-HOURS-STR PIC ZZ9.
       77 WS-RATE-STR  PIC ZZ9.99. *> 3 digits before decimal, 2 after
       77 WS-PAY-STR   PIC ZZZZZ9.99. *> 6 digits before decimal
       77 WS-TOTAL-STR PIC ZZZZZZZ9.99. *> 8 digits before decimal
       77 WS-LINE      PIC X(132).
       77 WS-EOF       PIC X(3) VALUE "NO".

       *> date fields
       77 WS-DATE-YYYYMMDD PIC 9(8).
       77 WS-DATE-YYYY     PIC 9(4).
       77 WS-DATE-MM       PIC 99.
       77 WS-DATE-DD       PIC 99.
       77 WS-DATE-FORMAT   PIC X(10). *> "YYYY-MM-DD"

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           *> Get filenames from the user interactively
           DISPLAY "Enter input file name (default: employees.csv): "
              WITH NO ADVANCING
           ACCEPT WS-INPUT-FILE

           DISPLAY
              "Enter output report file name (default: report.txt): "
              WITH NO ADVANCING
           ACCEPT WS-OUTPUT-FILE

           *> Set defaults if user entered nothing
           IF WS-INPUT-FILE = SPACES
              MOVE "employees.csv" TO WS-INPUT-FILE
           END-IF
           IF WS-OUTPUT-FILE = SPACES
              MOVE "report.txt" TO WS-OUTPUT-FILE
           END-IF

           *> Open files
           OPEN INPUT EMP-FILE
              OUTPUT RPT-FILE
              OUTPUT ERR-FILE

           *> check file open status
           IF WS-EMP-FILE-STATUS NOT = "00"
               DISPLAY "ERROR: cannot open input file " WS-INPUT-FILE
               MOVE "Cannot open input file" TO WS-ERROR-MSG
               PERFORM LOG-ERROR
               STOP RUN
           END-IF

           IF WS-RPT-FILE-STATUS NOT = "00"
                                       OR WS-ERR-FILE-STATUS NOT = "00"
               DISPLAY "ERROR: cannot open output files" 
               MOVE "Cannot open output or error file" TO WS-ERROR-MSG
               PERFORM LOG-ERROR
               STOP RUN
           END-IF

           *> get system date and format YYYY-MM-DD
           ACCEPT WS-DATE-YYYYMMDD FROM DATE YYYYMMDD

           MOVE WS-DATE-YYYYMMDD(1:4) TO WS-DATE-YYYY
           MOVE WS-DATE-YYYYMMDD(5:2) TO WS-DATE-MM
           MOVE WS-DATE-YYYYMMDD(7:2) TO WS-DATE-DD

           STRING
              WS-DATE-YYYY DELIMITED BY SIZE
              "-"          DELIMITED BY SIZE
              WS-DATE-MM   DELIMITED BY SIZE
              "-"          DELIMITED BY SIZE
              WS-DATE-DD   DELIMITED BY SIZE
              INTO WS-DATE-FORMAT
           END-STRING

           *> Write report header
           MOVE SPACES TO WS-LINE
           MOVE "================================================"
              TO WS-LINE(1:48)
           MOVE WS-LINE TO RPT-REC
           WRITE RPT-REC

           MOVE SPACES TO WS-LINE
           MOVE "  Payroll Report - Generated by Athos Exarchou"
              TO WS-LINE(1:60)
           MOVE WS-LINE TO RPT-REC
           WRITE RPT-REC

           MOVE SPACES TO WS-LINE
           STRING "                Date: " DELIMITED BY SIZE
                  WS-DATE-FORMAT DELIMITED BY SIZE
                  INTO WS-LINE(1:46)
           END-STRING
           
           MOVE WS-LINE TO RPT-REC
           WRITE RPT-REC

           MOVE SPACES TO WS-LINE
           MOVE "================================================"
              TO WS-LINE(1:48)
           MOVE WS-LINE TO RPT-REC
           WRITE RPT-REC

           *> Column header (fixed positions)
           MOVE SPACES TO WS-LINE
           MOVE "ID"        TO WS-LINE(1:2)
           MOVE "NAME"      TO WS-LINE(6:4)
           MOVE "HRS"       TO WS-LINE(28:5)
           MOVE "RATE"      TO WS-LINE(34:4)
           MOVE "PAY"       TO WS-LINE(43:3)
           MOVE WS-LINE TO RPT-REC
           WRITE RPT-REC

           MOVE SPACES TO WS-LINE
           MOVE "------------------------------------------------"
              TO WS-LINE(1:48)
           MOVE WS-LINE TO RPT-REC
           WRITE RPT-REC

           *> Process each employee
           PERFORM UNTIL WS-EOF = "YES"
               READ EMP-FILE
                  AT END
                     MOVE "YES" TO WS-EOF
                  NOT AT END
                  ADD 1 TO WS-REC-COUNT
                  MOVE SPACES TO WS-ERROR-MSG
                  MOVE 'N' TO WS-SKIP-REC

                  *> parse CSV line
                  UNSTRING EMP-REC
                    DELIMITED BY ","
                    INTO WS-EMP-ID-TEXT
                       WS-EMP-NAME-TEXT
                       WS-EMP-HOURS-TEXT
                       WS-EMP-RATE-TEXT
                       
                  *> basic missing-field check
                  IF WS-EMP-ID-TEXT = SPACES
                    OR WS-EMP-NAME-TEXT = SPACES
                    OR WS-EMP-HOURS-TEXT = SPACES
                    OR WS-EMP-RATE-TEXT = SPACES
                       MOVE "Malformed record - missing field"
                             TO WS-ERROR-MSG
                       MOVE 'Y' TO WS-SKIP-REC
                  END-IF

                  *> convert textual pieces into numeric variables
                  MOVE FUNCTION NUMVAL(WS-EMP-ID-TEXT) TO WS-EMP-ID

                  *> validate HOURS
                  *> only digits and spaces allowed, at least one digit
                  IF WS-SKIP-REC = 'N'
                     MOVE 0 TO WS-DIGITS
                     PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >
                                                  3 OR WS-SKIP-REC = 'Y'
                        MOVE WS-EMP-HOURS-TEXT(WS-I:1) TO WS-CHAR
                        IF WS-CHAR IS NUMERIC
                           ADD 1 TO WS-DIGITS
                        ELSE IF WS-CHAR = SPACE
                           CONTINUE
                        ELSE
                           MOVE "Invalid character in HOURS" 
                                TO WS-ERROR-MSG
                           MOVE 'Y' TO WS-SKIP-REC
                        END-IF
                     END-PERFORM

                     IF WS-SKIP-REC = 'N' AND WS-DIGITS = 0
                        MOVE "HOURS field has no digits"
                           TO WS-ERROR-MSG
                        MOVE 'Y' TO WS-SKIP-REC
                     END-IF

                     IF WS-SKIP-REC = 'N'
                        MOVE FUNCTION NUMVAL(WS-EMP-HOURS-TEXT)
                           TO WS-EMP-HOURS
                     END-IF
                  END-IF

                  *> validate RATE (allow digits, one dot, spaces)
                  IF WS-SKIP-REC = 'N'
                     MOVE 0 TO WS-DOT
                     MOVE 0 TO WS-DIGITS
                     PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >
                                                  6 OR WS-SKIP-REC = 'Y'
                        MOVE WS-EMP-RATE-TEXT(WS-I:1) TO WS-CHAR
                        IF WS-CHAR IS NUMERIC
                           ADD 1 TO WS-DIGITS
                        ELSE IF WS-CHAR = '.'
                           ADD 1 TO WS-DOT
                        
                           IF WS-DOT > 1
                              MOVE "RATE has multiple decimal points"
                                   TO WS-ERROR-MSG
                              MOVE 'Y' TO WS-SKIP-REC
                           END-IF
                        ELSE IF WS-CHAR = SPACE
                           CONTINUE
                        ELSE
                           MOVE "Invalid character in RATE"
                                 TO WS-ERROR-MSG
                           MOVE 'Y' TO WS-SKIP-REC
                        END-IF
                     END-PERFORM

                     IF WS-SKIP-REC = 'N' AND WS-DIGITS = 0
                        MOVE "RATE field has no digits"
                           TO WS-ERROR-MSG
                        MOVE 'Y' TO WS-SKIP-REC
                     END-IF

                     IF WS-SKIP-REC = 'N'
                        MOVE FUNCTION NUMVAL(WS-EMP-RATE-TEXT)
                           TO WS-RATE
                     END-IF
                  END-IF

                  *> if invalid, log and skip writing this record
                  IF WS-SKIP-REC = 'Y'
                     PERFORM LOG-ERROR
                  ELSE
                     *> calculate pay and keep running total
                     COMPUTE WS-PAY = WS-EMP-HOURS * WS-RATE
                     ADD WS-PAY TO WS-TOTAL-PAY

                     *> overflow check for total pay
                     IF WS-TOTAL-PAY > 99999999.99
                        MOVE "TOTAL PAY exceeds capacity"
                           TO WS-ERROR-MSG
                        PERFORM LOG-ERROR
                     END-IF

                     *> prepare edited display fields
                     MOVE WS-EMP-HOURS TO WS-HOURS-STR
                     MOVE WS-RATE TO WS-RATE-STR
                     MOVE WS-PAY TO WS-PAY-STR

                     *> Build the line (fixed columns)
                     MOVE SPACES TO WS-LINE
                     MOVE WS-EMP-ID-TEXT(1:4) TO WS-LINE(1:4)
                     MOVE WS-EMP-NAME-TEXT(1:20) TO WS-LINE(6:20)
                     MOVE WS-HOURS-STR TO WS-LINE(27:3)
                     MOVE WS-RATE-STR TO WS-LINE(33:6)
                     MOVE WS-PAY-STR TO WS-LINE(41:8)

                     MOVE WS-LINE TO RPT-REC
                     WRITE RPT-REC
                  END-IF

               END-READ
           END-PERFORM

           *> Report Footer
           MOVE SPACES TO WS-LINE
           MOVE "------------------------------------------------"
              TO WS-LINE(1:48)
           MOVE WS-LINE TO RPT-REC
           WRITE RPT-REC

           *> Write total line
           MOVE WS-TOTAL-PAY TO WS-TOTAL-STR
           MOVE SPACES TO WS-LINE
           MOVE "TOTAL PAY:" TO WS-LINE(1:10)
           MOVE WS-TOTAL-STR TO WS-LINE(39:11)

           MOVE WS-LINE TO RPT-REC
           WRITE RPT-REC

           MOVE "================================================"
              TO RPT-REC
           WRITE RPT-REC

           CLOSE EMP-FILE RPT-FILE ERR-FILE
           STOP RUN.

         LOG-ERROR.
           *> Build an error record: date - message - raw record
           MOVE SPACES TO ERR-REC
           STRING WS-DATE-FORMAT DELIMITED BY SIZE
               " - "          DELIMITED BY SIZE
               WS-ERROR-MSG   DELIMITED BY SIZE
               " - RAW: "     DELIMITED BY SIZE
               EMP-REC        DELIMITED BY SIZE
            INTO ERR-REC
           END-STRING

           WRITE ERR-REC

           *> reset skip flag
           MOVE "Y" TO WS-SKIP-REC
           EXIT.
