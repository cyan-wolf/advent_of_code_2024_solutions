      ******************************************************************
      * Author: cyan-wolf
      * Date: 03/03/2025
      * Purpose: To determine the number of safe records in the report.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADVENT-OF-CODE-2024-DAY-02-01.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "../input_test.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO "../output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
               
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-LINE.
           03 FILLER OCCURS 132 TIMES.
               05 INPUT-CHARS PIC X.
       
       FD  OUT-FILE.
       01  PRINT-LINE                  PIC X(132) VALUE SPACES.
           
       WORKING-STORAGE SECTION.
       01  WS-DATA-REMAINS             PIC X VALUE 'Y'.
       
       01  WS-PARSED-RECORD            OCCURS 10 TIMES 
                                       INDEXED BY WS-IDX.
           05 WS-RECORD-DATA           PIC 999.
           
       01  WS-PREV-NUM                 PIC 999 VALUE ZEROES.
       01  WS-CURR-NUM                 PIC 999 VALUE ZEROES.
       
       01  WS-CURR-STEP                PIC 999 VALUE ZEROES.
       01  WS-CURR-REPORT-IS-SAFE      PIC X VALUE 'Y'.
       
       01  WS-SAFE-REPORT-AMOUNT       PIC 9(20) VALUE ZEROES.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN
               INPUT INPUT-FILE
               OUTPUT OUT-FILE.

           PERFORM PROCESS-ALL-RECORDS.
           
           DISPLAY "SAFE REPORTS: " WS-SAFE-REPORT-AMOUNT.

           CLOSE
               INPUT-FILE
               OUT-FILE.
                   
           STOP RUN.
            
       PROCESS-ALL-RECORDS.
           PERFORM READ-RECORD
               UNTIL WS-DATA-REMAINS = 'N'.
            
       READ-RECORD.
           READ INPUT-FILE
               AT END MOVE 'N' TO WS-DATA-REMAINS.
               
           IF WS-DATA-REMAINS <> 'N' 
               PERFORM PROCESS-RECORD
           END-IF.
       
       PROCESS-RECORD.
           PERFORM PARSE-RECORD-TO-WS.

           PERFORM CHECK-ALL-NUMS-IN-RECORD
               UNTIL WS-RECORD-DATA (WS-IDX) = 0.
               
           IF WS-CURR-REPORT-IS-SAFE = 'Y'
               ADD 1 TO WS-SAFE-REPORT-AMOUNT
           END-IF.
               
           PERFORM RESET-ITERATION-FIELDS.
           
           DISPLAY "---------------".
           
           MOVE INPUT-LINE TO PRINT-LINE.
           WRITE PRINT-LINE.
           
       RESET-ITERATION-FIELDS.
           MOVE 1 TO WS-IDX.
           MOVE ZEROES TO WS-PREV-NUM.
           MOVE ZEROES TO WS-CURR-NUM.
           
           MOVE ZEROES TO WS-CURR-STEP.
           MOVE 'Y' TO WS-CURR-REPORT-IS-SAFE.
           
       PARSE-RECORD-TO-WS.
           UNSTRING INPUT-LINE DELIMITED BY " "
               INTO 
                   WS-RECORD-DATA (1), 
                   WS-RECORD-DATA (2),
                   WS-RECORD-DATA (3), 
                   WS-RECORD-DATA (4), 
                   WS-RECORD-DATA (5), 
                   WS-RECORD-DATA (6), 
                   WS-RECORD-DATA (7), 
                   WS-RECORD-DATA (8), 
                   WS-RECORD-DATA (9), 
                   WS-RECORD-DATA (10).
           
       CHECK-ALL-NUMS-IN-RECORD.
           MOVE WS-RECORD-DATA (WS-IDX) TO WS-CURR-NUM.
           
           IF WS-PREV-NUM <> 0
               DISPLAY WS-PREV-NUM " " WS-CURR-NUM
               
      *>          TODO: CHECK IF RECORD IS SAFE HERE
               
           END-IF.
           
           MOVE WS-CURR-NUM TO WS-PREV-NUM.
           
           ADD 1 TO WS-IDX.

       END PROGRAM ADVENT-OF-CODE-2024-DAY-02-01.

