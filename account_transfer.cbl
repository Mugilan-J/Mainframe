
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCOUNT-TRANSFER.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SOURCE-BALANCE     PIC 9(7).
       01  WS-DEST-BALANCE       PIC 9(7).
       01  WS-TRANSFER-AMOUNT    PIC 9(7).
       01  WS-DEST-IF-CREDITED   PIC 9(8).
       01  WS-MAX-BALANCE        PIC 9(7) VALUE 9999999.
       01  WS-SHOW               PIC Z(6)9.

       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-SOURCE-BALANCE
           ACCEPT WS-DEST-BALANCE
           ACCEPT WS-TRANSFER-AMOUNT

           IF WS-TRANSFER-AMOUNT = 0
               DISPLAY "Error: transfer amount must be positive"
           ELSE
               IF WS-TRANSFER-AMOUNT > WS-SOURCE-BALANCE
                   DISPLAY "Error: insufficient source funds"
               ELSE
                   COMPUTE WS-DEST-IF-CREDITED =
                           WS-DEST-BALANCE + WS-TRANSFER-AMOUNT
                   IF WS-DEST-IF-CREDITED > WS-MAX-BALANCE
                       DISPLAY "Error: destination would overflow"
                   ELSE
                       PERFORM APPLY-TRANSFER
                   END-IF
               END-IF
           END-IF
           STOP RUN.

       APPLY-TRANSFER.
           SUBTRACT WS-TRANSFER-AMOUNT FROM WS-SOURCE-BALANCE
           ADD WS-TRANSFER-AMOUNT TO WS-DEST-BALANCE
           MOVE WS-SOURCE-BALANCE TO WS-SHOW
           DISPLAY "Source balance: " FUNCTION TRIM(WS-SHOW)
           MOVE WS-DEST-BALANCE TO WS-SHOW
           DISPLAY "Destination balance: " FUNCTION TRIM(WS-SHOW).
