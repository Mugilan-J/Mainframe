       IDENTIFICATION DIVISION.
       PROGRAM-ID. BKSETTLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DAILY-TXN-FILE ASSIGN TO "daily_txn.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TXN-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  DAILY-TXN-FILE.
       01  TXN-REC                PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-TXN-STATUS          PIC X(2) VALUE SPACES.
       01  WS-EOF-FLAG            PIC X(1) VALUE "N".
           88  END-OF-TXNS        VALUE "Y".
       01  WS-RECORD-COUNT        PIC 9(7) VALUE 0.
       01  WS-COUNT-SHOW          PIC Z(6)9.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-OPEN-INPUT
           PERFORM 2000-READ-ALL-TRANSACTIONS
           PERFORM 3000-FINISH
           GOBACK.

      * Open the file and decide what to do based on the status code.
       1000-OPEN-INPUT.
           OPEN INPUT DAILY-TXN-FILE
           EVALUATE WS-TXN-STATUS
               WHEN "00"
                   CONTINUE
               WHEN "35"
                   DISPLAY "[WARN] TXN FILE MISSING (STATUS=35)."
                   DISPLAY "ZERO-RECORD RUN COMPLETED. RC=04"
                   MOVE 4 TO RETURN-CODE
                   GOBACK
               WHEN OTHER
                   PERFORM 9999-ABEND-ROUTINE
           END-EVALUATE.

       2000-READ-ALL-TRANSACTIONS.
           PERFORM UNTIL END-OF-TXNS
               READ DAILY-TXN-FILE
                   AT END
                       SET END-OF-TXNS TO TRUE
                   NOT AT END
                       ADD 1 TO WS-RECORD-COUNT
               END-READ
           END-PERFORM.

       3000-FINISH.
           CLOSE DAILY-TXN-FILE
           MOVE WS-RECORD-COUNT TO WS-COUNT-SHOW
           DISPLAY "SETTLEMENT RUN COMPLETED. RECORDS READ: "
                   FUNCTION TRIM(WS-COUNT-SHOW)
           MOVE 0 TO RETURN-CODE.

      * Anything unexpected is a real problem: report it loudly.
       9999-ABEND-ROUTINE.
           DISPLAY "[FATAL] UNEXPECTED OPEN STATUS=" WS-TXN-STATUS
           MOVE 16 TO RETURN-CODE
           GOBACK.
