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
           SELECT INPUT-FILE ASSIGN TO "../input.txt"
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
       01  HEADING-LINE.
           05 FILLER                   PIC X(6)          VALUE "RECORD".
           05 FILLER                   PIC X(4)          VALUE SPACES.
           05 FILLER                   PIC X(2)          VALUE "1".
           05 FILLER                   PIC XX            VALUE SPACES.
           05 FILLER                   PIC X(2)          VALUE "2".
           05 FILLER                   PIC XX            VALUE SPACES.
           05 FILLER                   PIC X(2)          VALUE "3".
           05 FILLER                   PIC XX            VALUE SPACES.
           05 FILLER                   PIC X(2)          VALUE "4".
           05 FILLER                   PIC XX            VALUE SPACES.
           05 FILLER                   PIC X(2)          VALUE "5".
           05 FILLER                   PIC XX            VALUE SPACES.
           05 FILLER                   PIC X(2)          VALUE "6".
           05 FILLER                   PIC XX            VALUE SPACES.
           05 FILLER                   PIC X(2)          VALUE "7".
           05 FILLER                   PIC XX            VALUE SPACES.
           05 FILLER                   PIC X(2)          VALUE "8".
           05 FILLER                   PIC X(5)          VALUE SPACES.
           05 FILLER                   PIC X(6)          VALUE "STATUS".
           
       01  DETAIL-LINE.
           03 FILLER                   PIC X(8)          VALUE SPACES.
           03 DET-FIELDS               OCCURS 8 TIMES.
               05 DET-FIELD-VALUE      PIC 999.
               05 FILLER               PIC X             VALUE SPACES.
               
           03 FILLER                   PIC X(5)          VALUE SPACES.
           03 DET-STATUS               PIC X(6).
           
       01  TOTAL-LINE-DASHES.
           03 FILLER                   OCCURS 51 TIMES.
               05 FILLER               PIC X             VALUE "-".
           
       01  TOTAL-LINE.
               05 FILLER               PIC X(5)          VALUE "TOTAL".
               05 FILLER               PIC X(40)         VALUE SPACES.
               05 TOTAL-SAFE-REPORTS   PIC 9(6).
           
       01  WS-PARSED-RECORD            OCCURS 8 TIMES
                                       INDEXED BY WS-IDX.
               05 WS-RECORD-DATA           PIC 999.
       
       77  WS-DATA-REMAINS             PIC X VALUE 'Y'.

       77  WS-PREV-NUM                 PIC 999 VALUE ZEROES.
       77  WS-CURR-NUM                 PIC 999 VALUE ZEROES.

      *>   Always positve.
       77  WS-CURR-STEP                PIC 999 VALUE ZEROES.
       
       77  WS-RECORD-INCREASING        PIC X VALUE 'N'.
       77  WS-RECORD-DECREASING        PIC X VALUE 'N'.

       77  WS-CURR-REPORT-IS-SAFE      PIC X VALUE 'Y'.

       77  WS-SAFE-REPORT-AMOUNT       PIC 9(6) VALUE ZEROES.

       PROCEDURE DIVISION.
       0000-MAIN-PROCEDURE.
           OPEN
               INPUT INPUT-FILE
               OUTPUT OUT-FILE.

           PERFORM 0100-WRITE-HEADING-LINE.
       
           PERFORM 0200-PROCESS-ALL-RECORDS.

           PERFORM 0300-WRITE-TOTAL-LINE.

           CLOSE
               INPUT-FILE
               OUT-FILE.

           STOP RUN.
           
       0100-WRITE-HEADING-LINE.
           MOVE HEADING-LINE TO PRINT-LINE.
           WRITE PRINT-LINE.

       0200-PROCESS-ALL-RECORDS.
           PERFORM 0210-READ-RECORD
               UNTIL WS-DATA-REMAINS = 'N'.

       0210-READ-RECORD.
           READ INPUT-FILE
               AT END MOVE 'N' TO WS-DATA-REMAINS.

           IF WS-DATA-REMAINS <> 'N'
               PERFORM 0230-PROCESS-RECORD
           END-IF.

       0230-PROCESS-RECORD.
           PERFORM 0240-PARSE-RECORD-TO-WS.

           PERFORM 0250-CHECK-ALL-NUMS-IN-RECORD
               UNTIL WS-RECORD-DATA (WS-IDX) = 0 
                     OR WS-CURR-REPORT-IS-SAFE = 'N'.

           IF WS-CURR-REPORT-IS-SAFE = 'Y' THEN
               ADD 1 TO WS-SAFE-REPORT-AMOUNT
           END-IF.
               
           PERFORM 0260-WRITE-DETAIL-LINE.

           PERFORM 0270-RESET-ITERATION-FIELDS.


       0240-PARSE-RECORD-TO-WS.
           UNSTRING INPUT-LINE DELIMITED BY " "
               INTO
                   WS-RECORD-DATA (1),
                   WS-RECORD-DATA (2),
                   WS-RECORD-DATA (3),
                   WS-RECORD-DATA (4),
                   WS-RECORD-DATA (5),
                   WS-RECORD-DATA (6),
                   WS-RECORD-DATA (7),
                   WS-RECORD-DATA (8).  

       0250-CHECK-ALL-NUMS-IN-RECORD.
           MOVE WS-RECORD-DATA (WS-IDX) TO WS-CURR-NUM.

           IF WS-PREV-NUM <> 0 THEN
               MOVE WS-CURR-NUM TO WS-CURR-STEP
               SUBTRACT WS-PREV-NUM FROM WS-CURR-STEP
               
               PERFORM 0251-CHECK-STEP-VALIDITY
               PERFORM 0252-CHECK-ORDERING
           END-IF.

           MOVE WS-CURR-NUM TO WS-PREV-NUM.

           ADD 1 TO WS-IDX.

       0251-CHECK-STEP-VALIDITY.
           IF WS-CURR-STEP < 1 OR WS-CURR-STEP > 3 THEN
               MOVE 'N' TO WS-CURR-REPORT-IS-SAFE
           END-IF.
               
       0252-CHECK-ORDERING.
           IF WS-PREV-NUM < WS-CURR-NUM
           THEN
               MOVE 'Y' TO WS-RECORD-INCREASING
               
           ELSE IF WS-PREV-NUM > WS-CURR-NUM
           THEN
               MOVE 'Y' TO WS-RECORD-DECREASING
           END-IF.
               
           IF WS-RECORD-INCREASING = 'Y' AND WS-RECORD-DECREASING = 'Y' 
           THEN
               MOVE 'N' TO WS-CURR-REPORT-IS-SAFE
           END-IF.       
       
       0260-WRITE-DETAIL-LINE.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 8
               MOVE WS-RECORD-DATA (WS-IDX) TO DET-FIELD-VALUE (WS-IDX)
           END-PERFORM.
           
           IF WS-CURR-REPORT-IS-SAFE = 'Y' THEN 
               MOVE "SAFE"     TO DET-STATUS
           ELSE 
               MOVE "UNSAFE"   TO DET-STATUS
           END-IF.
               
           MOVE DETAIL-LINE    TO PRINT-LINE.
           WRITE PRINT-LINE.

       0270-RESET-ITERATION-FIELDS.
           MOVE 1          TO WS-IDX.
           MOVE ZEROES     TO WS-PREV-NUM.
           MOVE ZEROES     TO WS-CURR-NUM.

           MOVE ZEROES     TO WS-CURR-STEP.
           MOVE 'N'        TO WS-RECORD-INCREASING.
           MOVE 'N'        TO WS-RECORD-DECREASING.
           
           MOVE 'Y'        TO WS-CURR-REPORT-IS-SAFE.
           
       0300-WRITE-TOTAL-LINE.
           MOVE TOTAL-LINE-DASHES      TO PRINT-LINE.
           WRITE PRINT-LINE.
           
           MOVE WS-SAFE-REPORT-AMOUNT  TO TOTAL-SAFE-REPORTS.
           MOVE TOTAL-LINE             TO PRINT-LINE.
           WRITE PRINT-LINE.

       END PROGRAM ADVENT-OF-CODE-2024-DAY-02-01.
