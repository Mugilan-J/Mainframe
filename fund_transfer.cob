       IDENTIFICATION DIVISION.
       PROGRAM-ID. FUND-TRANSFER.

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

       01  WS-SOURCE-INPUT      PIC X(10).
       01  WS-DEST-INPUT        PIC X(10).
       01  WS-AMOUNT            PIC 9(7)V99.
       01  WS-INDEX             PIC 9.
       01  WS-SRC-AT            PIC 9 VALUE 0.
       01  WS-DEST-AT           PIC 9 VALUE 0.
       01  WS-SHOW              PIC Z(6)9.99.

       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-SOURCE-INPUT
           ACCEPT WS-DEST-INPUT
           ACCEPT WS-AMOUNT

           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 3
               IF ACC-NUMBER(WS-INDEX) = WS-SOURCE-INPUT
                   MOVE WS-INDEX TO WS-SRC-AT
               END-IF
               IF ACC-NUMBER(WS-INDEX) = WS-DEST-INPUT
                   MOVE WS-INDEX TO WS-DEST-AT
               END-IF
           END-PERFORM

           EVALUATE TRUE
               WHEN WS-SRC-AT = 0 OR WS-DEST-AT = 0
                   DISPLAY "Transfer Failed: ACCOUNT NOT FOUND"
               WHEN WS-SRC-AT = WS-DEST-AT
                   DISPLAY "Transfer Failed: SAME ACCOUNT"
               WHEN ACC-STATUS(WS-SRC-AT) NOT = "A"
                   OR ACC-STATUS(WS-DEST-AT) NOT = "A"
                   DISPLAY "Transfer Failed: ACCOUNT INACTIVE"
               WHEN WS-AMOUNT = 0
                   DISPLAY "Transfer Failed: AMOUNT MUST BE POSITIVE"
               WHEN WS-AMOUNT > ACC-BALANCE(WS-SRC-AT)
                   DISPLAY "Transfer Failed: INSUFFICIENT BALANCE"
               WHEN OTHER
                   SUBTRACT WS-AMOUNT FROM ACC-BALANCE(WS-SRC-AT)
                   ADD WS-AMOUNT TO ACC-BALANCE(WS-DEST-AT)
                   PERFORM SHOW-RESULT
           END-EVALUATE
           STOP RUN.

       SHOW-RESULT.
           DISPLAY "------ FUND TRANSFER RESULT ------"
           DISPLAY "Transfer Successful"
           DISPLAY "From Account : " ACC-NUMBER(WS-SRC-AT)
           DISPLAY "To Account   : " ACC-NUMBER(WS-DEST-AT)
           MOVE WS-AMOUNT TO WS-SHOW
           DISPLAY "Amount       : " FUNCTION TRIM(WS-SHOW)
           MOVE ACC-BALANCE(WS-SRC-AT) TO WS-SHOW
           DISPLAY "Source Balance      : " FUNCTION TRIM(WS-SHOW)
           MOVE ACC-BALANCE(WS-DEST-AT) TO WS-SHOW
           DISPLAY "Destination Balance : " FUNCTION TRIM(WS-SHOW)
           DISPLAY "----------------------------------".
