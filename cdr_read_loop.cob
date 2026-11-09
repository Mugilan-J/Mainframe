       IDENTIFICATION DIVISION.
       PROGRAM-ID. CDR-BILLING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CDR-FILE ASSIGN TO "cdr.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CDR-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CDR-FILE.
       01  CDR-FILE-REC           PIC X(60).

       WORKING-STORAGE SECTION.
       01  WS-CDR-STATUS          PIC X(2) VALUE SPACES.
           88  CDR-OK             VALUE "00".
           88  CDR-EOF            VALUE "10".
       01  WS-EOF-SWITCH          PIC X(1) VALUE "N".
           88  EOF-REACHED        VALUE "Y".
       01  WS-CDR-REC.
           05  WS-PHONE           PIC X(12).
           05  WS-DURATION-TEXT   PIC X(4).
           05  WS-CALL-DATE       PIC X(8).
       01  WS-DURATION            PIC 9(4).
       01  WS-RECORDS-READ        PIC 9(9) VALUE 0.
       01  WS-TOTAL-SECONDS       PIC 9(12) VALUE 0.
       01  WS-SHOW-COUNT          PIC Z(8)9.
       01  WS-SHOW-SECONDS        PIC Z(11)9.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT CDR-FILE
           IF NOT CDR-OK
               DISPLAY "[FATAL] CANNOT OPEN CDR FILE, STATUS="
                       WS-CDR-STATUS
               MOVE 16 TO RETURN-CODE
               STOP RUN
           END-IF

           PERFORM 2000-READ-NEXT-CDR UNTIL EOF-REACHED

           CLOSE CDR-FILE
           PERFORM 3000-PRINT-SUMMARY
           STOP RUN.

      * One READ, then act on what the status code tells us.
       2000-READ-NEXT-CDR.
           READ CDR-FILE
           EVALUATE TRUE
               WHEN CDR-OK
                   PERFORM 2100-RATE-CALL
               WHEN CDR-EOF
                   SET EOF-REACHED TO TRUE
               WHEN OTHER
                   PERFORM 9000-FILE-ERROR
           END-EVALUATE.

       2100-RATE-CALL.
           MOVE SPACES TO WS-CDR-REC
           UNSTRING CDR-FILE-REC DELIMITED BY "|"
               INTO WS-PHONE WS-DURATION-TEXT WS-CALL-DATE
           END-UNSTRING
           COMPUTE WS-DURATION = FUNCTION NUMVAL(WS-DURATION-TEXT)
           ADD WS-DURATION TO WS-TOTAL-SECONDS
           ADD 1 TO WS-RECORDS-READ.

       3000-PRINT-SUMMARY.
           MOVE WS-RECORDS-READ TO WS-SHOW-COUNT
           MOVE WS-TOTAL-SECONDS TO WS-SHOW-SECONDS
           DISPLAY "Clean loop exit, FILE STATUS = 10"
           DISPLAY "Records processed : " FUNCTION TRIM(WS-SHOW-COUNT)
           DISPLAY "Total call seconds: "
                   FUNCTION TRIM(WS-SHOW-SECONDS).

       9000-FILE-ERROR.
           DISPLAY "[FATAL] READ FAILED, STATUS=" WS-CDR-STATUS
           MOVE 16 TO RETURN-CODE
           STOP RUN.
