       IDENTIFICATION DIVISION.
       PROGRAM-ID. CARD-PAYMENT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CARD-RECORD.
           05  CARD-NUMBER       PIC X(16) VALUE "4321123456781111".
           05  CARD-OUTSTANDING  PIC 9(7)V99 VALUE 20000.00.
           05  CARD-STATUS       PIC X(1) VALUE "A".

       01  WS-CARD-INPUT         PIC X(16).
       01  WS-PAYMENT            PIC 9(7)V99.
       01  WS-SHOW               PIC Z(6)9.99.

       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-CARD-INPUT
           ACCEPT WS-PAYMENT

           EVALUATE TRUE
               WHEN WS-CARD-INPUT NOT = CARD-NUMBER
                   DISPLAY "CARD NOT FOUND"
               WHEN CARD-STATUS NOT = "A"
                   DISPLAY "CARD INACTIVE"
               WHEN WS-PAYMENT = 0
                   DISPLAY "PAYMENT AMOUNT MUST BE POSITIVE"
               WHEN WS-PAYMENT > CARD-OUTSTANDING
                   DISPLAY "PAYMENT EXCEEDS OUTSTANDING BALANCE"
               WHEN OTHER
                   SUBTRACT WS-PAYMENT FROM CARD-OUTSTANDING
                   PERFORM SHOW-RECEIPT
           END-EVALUATE
           STOP RUN.

       SHOW-RECEIPT.
           DISPLAY "------ CREDIT CARD PAYMENT ------"
           DISPLAY "Card Number    : "
                   CARD-NUMBER(1:4) " " CARD-NUMBER(5:4) " "
                   CARD-NUMBER(9:4) " " CARD-NUMBER(13:4)
           MOVE WS-PAYMENT TO WS-SHOW
           DISPLAY "Payment Amount : " FUNCTION TRIM(WS-SHOW)
           MOVE CARD-OUTSTANDING TO WS-SHOW
           DISPLAY "New Balance    : " FUNCTION TRIM(WS-SHOW)
           DISPLAY "---------------------------------".
