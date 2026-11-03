       IDENTIFICATION DIVISION.
       PROGRAM-ID. BALANCE-INQUIRY.

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
       01  WS-INDEX             PIC 9.
       01  WS-FOUND-AT          PIC 9 VALUE 0.
       01  WS-SHOW              PIC Z(6)9.99.

       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT WS-ACCOUNT-INPUT
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > 3 OR WS-FOUND-AT > 0
               IF ACC-NUMBER(WS-INDEX) = WS-ACCOUNT-INPUT
                   MOVE WS-INDEX TO WS-FOUND-AT
               END-IF
           END-PERFORM

           IF WS-FOUND-AT = 0
               DISPLAY "ACCOUNT NOT FOUND"
           ELSE
               IF ACC-STATUS(WS-FOUND-AT) NOT = "A"
                   DISPLAY "ACCOUNT INACTIVE"
               ELSE
                   MOVE ACC-BALANCE(WS-FOUND-AT) TO WS-SHOW
                   DISPLAY "------ ATM BALANCE INQUIRY ------"
                   DISPLAY "Account Number : " ACC-NUMBER(WS-FOUND-AT)
                   DISPLAY "Customer Name : "
                           FUNCTION TRIM(ACC-NAME(WS-FOUND-AT))
                   DISPLAY "Balance        : " FUNCTION TRIM(WS-SHOW)
                   DISPLAY "---------------------------------"
               END-IF
           END-IF
           STOP RUN.
