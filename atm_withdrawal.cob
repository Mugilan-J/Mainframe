       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATM-WITHDRAWAL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ACCOUNT-DATA.
           05  FILLER PIC X(44) VALUE
               "9000000001Rajesh Kumar        S005000000A".
           05  FILLER PIC X(44) VALUE
               "9000000002Priya Sharma        S001500000A".
           05  FILLER PIC X(44) VALUE
               "9000000003Arun Menon          C020000000A".
       01  ACCOUNT-TABLE REDEFINES ACCOUNT-DATA.
           05  ACCOUNT-ENTRY OCCURS 3 TIMES.
               10  ACC-NUMBER   PIC X(10).
               10  ACC-NAME     PIC X(20).
               10  ACC-TYPE     PIC X(1).
               10  ACC-BALANCE  PIC 9(7)V99.
               10  ACC-STATUS   PIC X(1).
               10  FILLER       PIC X(3).

       01  WS-ACCOUNT-INPUT     PIC X(10).
       01  WS-AMOUNT            PIC 9(7).
       01  WS-INDEX             PIC 9.
       01  WS-FOUND-AT          PIC 9 VALUE 0.
       01  WS-BALANCE-AFTER     PIC S9(8)V99.
       01  WS-MAX-WITHDRAWAL    PIC 9(5) VALUE 20000.
       01  WS-MIN-BALANCE       PIC 9(4) VALUE 1000.
       01  WS-SHOW              PIC Z(8)9.

       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-ACCOUNT-INPUT
           ACCEPT WS-AMOUNT
           PERFORM FIND-ACCOUNT

           IF WS-FOUND-AT = 0
               DISPLAY "ACCOUNT NOT FOUND"
           ELSE
               IF ACC-STATUS(WS-FOUND-AT) NOT = "A"
                   DISPLAY "ACCOUNT INACTIVE"
               ELSE
                   PERFORM VALIDATE-AND-WITHDRAW
               END-IF
           END-IF
           STOP RUN.

       FIND-ACCOUNT.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > 3 OR WS-FOUND-AT > 0
               IF ACC-NUMBER(WS-INDEX) = WS-ACCOUNT-INPUT
                   MOVE WS-INDEX TO WS-FOUND-AT
               END-IF
           END-PERFORM.

       VALIDATE-AND-WITHDRAW.
           COMPUTE WS-BALANCE-AFTER =
                   ACC-BALANCE(WS-FOUND-AT) - WS-AMOUNT
           EVALUATE TRUE
               WHEN WS-AMOUNT = 0
                   DISPLAY "INVALID AMOUNT"
               WHEN WS-AMOUNT > WS-MAX-WITHDRAWAL
                   DISPLAY "WITHDRAWAL LIMIT EXCEEDED (MAX 20000)"
               WHEN WS-BALANCE-AFTER < WS-MIN-BALANCE
                   DISPLAY "INSUFFICIENT BALANCE (MIN 1000 MUST REMAIN)"
               WHEN OTHER
                   SUBTRACT WS-AMOUNT FROM ACC-BALANCE(WS-FOUND-AT)
                   PERFORM SHOW-RECEIPT
           END-EVALUATE.

       SHOW-RECEIPT.
           DISPLAY "------- ATM WITHDRAWAL -------"
           DISPLAY "Account Number : " ACC-NUMBER(WS-FOUND-AT)
           MOVE WS-AMOUNT TO WS-SHOW
           DISPLAY "Amount Withdrawn : " FUNCTION TRIM(WS-SHOW)
           MOVE ACC-BALANCE(WS-FOUND-AT) TO WS-SHOW
           DISPLAY "Remaining Balance : " FUNCTION TRIM(WS-SHOW)
           DISPLAY "Transaction Successful"
           DISPLAY "-------------------------------".
