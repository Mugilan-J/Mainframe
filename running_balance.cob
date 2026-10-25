      IDENTIFICATION DIVISION.
       PROGRAM-ID. RUNNING-BALANCE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-BALANCE        PIC S9(12) VALUE 0.
       01  WS-COUNT          PIC 9(4).
       01  WS-INDEX          PIC 9(4).
       01  WS-LINE           PIC X(40).
       01  WS-TYPE           PIC X(10).
       01  WS-AMOUNT-TEXT    PIC X(20).
       01  WS-AMOUNT         PIC 9(12).
       01  WS-SHOW           PIC Z(11)9.

       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-BALANCE
           ACCEPT WS-COUNT
           PERFORM PROCESS-ONE-TRANSACTION
                   VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-COUNT
           STOP RUN.

       PROCESS-ONE-TRANSACTION.
           MOVE SPACES TO WS-TYPE WS-AMOUNT-TEXT
           ACCEPT WS-LINE
           UNSTRING FUNCTION TRIM(WS-LINE)
               DELIMITED BY ALL SPACE
               INTO WS-TYPE WS-AMOUNT-TEXT
           END-UNSTRING
           MOVE FUNCTION UPPER-CASE(WS-TYPE) TO WS-TYPE

           IF FUNCTION TEST-NUMVAL(WS-AMOUNT-TEXT) NOT = 0
              OR FUNCTION NUMVAL(WS-AMOUNT-TEXT) < 0
               PERFORM SHOW-INVALID
           ELSE
               COMPUTE WS-AMOUNT = FUNCTION NUMVAL(WS-AMOUNT-TEXT)
               EVALUATE WS-TYPE
                   WHEN "C"
                       ADD WS-AMOUNT TO WS-BALANCE
                       PERFORM SHOW-BALANCE
                   WHEN "D"
                       IF WS-AMOUNT > WS-BALANCE
                           PERFORM SHOW-REJECTED
                       ELSE
                           SUBTRACT WS-AMOUNT FROM WS-BALANCE
                           PERFORM SHOW-BALANCE
                       END-IF
                   WHEN OTHER
                       PERFORM SHOW-INVALID
               END-EVALUATE
           END-IF.

       SHOW-BALANCE.
           MOVE WS-BALANCE TO WS-SHOW
           DISPLAY FUNCTION TRIM(WS-SHOW).

       SHOW-REJECTED.
           MOVE WS-BALANCE TO WS-SHOW
           DISPLAY "REJECTED: " FUNCTION TRIM(WS-SHOW).

       SHOW-INVALID.
           MOVE WS-BALANCE TO WS-SHOW
           DISPLAY "INVALID: " FUNCTION TRIM(WS-SHOW).
