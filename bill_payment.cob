       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILL-PAYMENT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ACCOUNT-DATA.
           05  FILLER PIC X(20) VALUE "9000000001005000000A".
           05  FILLER PIC X(20) VALUE "9000000002001500000A".
           05  FILLER PIC X(20) VALUE "9000000003020000000A".
       01  ACCOUNT-TABLE REDEFINES ACCOUNT-DATA.
           05  ACCOUNT-ENTRY OCCURS 3 TIMES.
               10  ACC-NUMBER   PIC X(10).
               10  ACC-BALANCE  PIC 9(7)V99.
               10  ACC-STATUS   PIC X(1).

       01  WS-ACCOUNT-INPUT     PIC X(10).
       01  WS-BILLER-NAME       PIC X(30).
       01  WS-AMOUNT            PIC 9(7).
       01  WS-INDEX             PIC 9.
       01  WS-FOUND-AT          PIC 9 VALUE 0.
       01  WS-SHOW              PIC Z(6)9.

       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-ACCOUNT-INPUT
           ACCEPT WS-BILLER-NAME
           ACCEPT WS-AMOUNT

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > 3 OR WS-FOUND-AT > 0
               IF ACC-NUMBER(WS-INDEX) = WS-ACCOUNT-INPUT
                   MOVE WS-INDEX TO WS-FOUND-AT
               END-IF
           END-PERFORM

           EVALUATE TRUE
               WHEN WS-FOUND-AT = 0
                   DISPLAY "Payment Failed: ACCOUNT NOT FOUND"
               WHEN ACC-STATUS(WS-FOUND-AT) NOT = "A"
                   DISPLAY "Payment Failed: ACCOUNT INACTIVE"
               WHEN WS-AMOUNT = 0
                   DISPLAY "Payment Failed: AMOUNT MUST BE POSITIVE"
               WHEN WS-AMOUNT > ACC-BALANCE(WS-FOUND-AT)
                   DISPLAY "Payment Failed: INSUFFICIENT BALANCE"
               WHEN OTHER
                   SUBTRACT WS-AMOUNT FROM ACC-BALANCE(WS-FOUND-AT)
                   PERFORM SHOW-RESULT
           END-EVALUATE
           STOP RUN.

       SHOW-RESULT.
           MOVE WS-AMOUNT TO WS-SHOW
           DISPLAY "------- BILL PAYMENT -------"
           DISPLAY "Account Number : " ACC-NUMBER(WS-FOUND-AT)
           DISPLAY "Biller Name    : " FUNCTION TRIM(WS-BILLER-NAME)
           DISPLAY "Amount Paid    : " FUNCTION TRIM(WS-SHOW)
           DISPLAY "Payment Successful"
           DISPLAY "----------------------------".
