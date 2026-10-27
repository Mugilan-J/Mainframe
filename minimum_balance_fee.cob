       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIN-BALANCE-FEE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-BALANCE        PIC 9(7).
       01  WS-MINIMUM        PIC 9(7).
       01  WS-FEE            PIC 9(7).
       01  WS-SHOW           PIC Z(6)9.

       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-BALANCE
           ACCEPT WS-MINIMUM
           ACCEPT WS-FEE

           IF WS-BALANCE < WS-MINIMUM
               IF WS-BALANCE >= WS-FEE
                   SUBTRACT WS-FEE FROM WS-BALANCE
                   PERFORM SHOW-BALANCE
               ELSE
                   DISPLAY "Fee cannot be collected: insufficient funds"
               END-IF
           ELSE
               PERFORM SHOW-BALANCE
           END-IF
           STOP RUN.

       SHOW-BALANCE.
           MOVE WS-BALANCE TO WS-SHOW
           DISPLAY FUNCTION TRIM(WS-SHOW).
