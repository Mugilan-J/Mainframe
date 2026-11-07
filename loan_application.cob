       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOAN-APPLICATION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CUSTOMER-DATA.
           05  FILLER PIC X(30) VALUE "9000000001Rajesh Kumar        ".
           05  FILLER PIC X(30) VALUE "9000000002Priya Sharma        ".
           05  FILLER PIC X(30) VALUE "9000000003Arun Menon          ".
       01  CUSTOMER-TABLE REDEFINES CUSTOMER-DATA.
           05  CUSTOMER-ENTRY OCCURS 3 TIMES.
               10  CUST-ID      PIC X(10).
               10  CUST-NAME    PIC X(20).

       01  WS-CUSTOMER-INPUT    PIC X(10).
       01  WS-LOAN-AMOUNT       PIC 9(8).
       01  WS-LOAN-TYPE         PIC X(15).
       01  WS-INDEX             PIC 9.
       01  WS-FOUND-AT          PIC 9 VALUE 0.
       01  WS-LOAN-LIMIT        PIC 9(8) VALUE 5000000.
       01  WS-LAST-APP-ID       PIC 9(6) VALUE 120044.
       01  WS-APP-STATUS        PIC X(1).
       01  WS-SHOW              PIC Z(7)9.

       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-CUSTOMER-INPUT
           ACCEPT WS-LOAN-AMOUNT
           ACCEPT WS-LOAN-TYPE

           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > 3 OR WS-FOUND-AT > 0
               IF CUST-ID(WS-INDEX) = WS-CUSTOMER-INPUT
                   MOVE WS-INDEX TO WS-FOUND-AT
               END-IF
           END-PERFORM

           EVALUATE TRUE
               WHEN WS-FOUND-AT = 0
                   DISPLAY "CUSTOMER NOT FOUND"
               WHEN WS-LOAN-AMOUNT = 0
                   DISPLAY "LOAN AMOUNT MUST BE POSITIVE"
               WHEN WS-LOAN-AMOUNT > WS-LOAN-LIMIT
                   DISPLAY "LOAN AMOUNT EXCEEDS LIMIT OF 5000000"
               WHEN WS-LOAN-TYPE = SPACES
                   DISPLAY "LOAN TYPE IS REQUIRED"
               WHEN OTHER
                   ADD 1 TO WS-LAST-APP-ID
                   MOVE "P" TO WS-APP-STATUS
                   PERFORM SHOW-CONFIRMATION
           END-EVALUATE
           STOP RUN.

       SHOW-CONFIRMATION.
           MOVE WS-LOAN-AMOUNT TO WS-SHOW
           DISPLAY "------ LOAN APPLICATION ------"
           DISPLAY "Application ID : " WS-LAST-APP-ID
           DISPLAY "Customer ID    : " CUST-ID(WS-FOUND-AT)
           DISPLAY "Loan Type      : " FUNCTION TRIM(WS-LOAN-TYPE)
           DISPLAY "Loan Amount    : " FUNCTION TRIM(WS-SHOW)
           IF WS-APP-STATUS = "P"
               DISPLAY "Status         : PENDING"
           END-IF
           DISPLAY "------------------------------".
