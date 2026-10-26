IDENTIFICATION DIVISION.
PROGRAM-ID. STANDING-ORDER.

DATA DIVISION.  
   WORKING-STORAGE SECTION.  

   01 ACCOUNT-COUNT PIC 9(3).  
   01 ORDER-COUNT PIC 9(4).  
   01 I PIC 9(4) VALUE 0.  
   01 J PIC 9(4) VALUE 0.  
   01 ACCOUNT-TABLE.  
   02 ACCOUNT-ENTRY OCCURS 100 TIMES.  
   03 ACCOUNT-ID PIC X(2).  
   03 BALANCE PIC 9(9) VALUE 0.  
   01 ORDER-TABLE.  
   02 ORDER-ENTRY OCCURS 5000 TIMES.  
   03 ORDER-ID PIC X(2).  
   03 ORDER-ACCOUNT PIC X(10).  
   03 ORDER-AMOUNT  PIC S9(9).  
   01 CURRENT-ACCOUNT PIC X(10).  
   01 CURRENT-AMOUNT PIC 9(9).  
   01 FOUND-FLAG PIC X VALUE 'N'.  
   01 TOTAL-SUCCESS  PIC 9(5) VALUE 0.  

   PROCEDURE DIVISION.  
       ACCEPT ACCOUNT-COUNT  
       PERFORM VARYING I FROM 1 BY 1  
           UNTIL I > ACCOUNT-COUNT  
           ACCEPT ACCOUNT-ID(I)  
           ACCEPT BALANCE(I)  
       END-PERFORM  
       ACCEPT ORDER-COUNT  
       PERFORM VARYING I FROM 1 BY 1  
           UNTIL I > ORDER-COUNT  
           ACCEPT ORDER-ID(I)  
           ACCEPT ORDER-ACCOUNT(I)  
           ACCEPT ORDER-AMOUNT(I)  
       END-PERFORM  
       PERFORM VARYING I FROM 1 BY 1  
           UNTIL I > ORDER-COUNT  
           MOVE 'N' TO FOUND-FLAG  
           PERFORM VARYING J FROM 1 BY 1  
               UNTIL J > ACCOUNT-COUNT  
               IF ORDER-ACCOUNT(I) = ACCOUNT-ID(J)  
                   MOVE 'Y' TO FOUND-FLAG  
                   EXIT PERFORM  
               END-IF  
           END-PERFORM  
           IF FOUND-FLAG = 'N'  
               DISPLAY ORDER-ID(I)  
                   ' REJECTED: UNKNOWN ACCOUNT'  
           ELSE  
               IF ORDER-AMOUNT(I) > 0  
                   AND BALANCE(J) >= ORDER-AMOUNT(I)  
                   SUBTRACT ORDER-AMOUNT(I)  
                       FROM BALANCE(J)  
                   ADD ORDER-AMOUNT(I)  
                       TO TOTAL-SUCCESS  
                   DISPLAY ORDER-ID(I)  
                       ' SUCCESS'  
               ELSE  
                   IF ORDER-AMOUNT(I) <= 0  
                       DISPLAY ORDER-ID(I)  
                           ' REJECTED: INVALID AMOUNT'  
                   ELSE  
                       DISPLAY ORDER-ID(I)  
                           ' REJECTED: INSUFFICIENT FUNDS'  
                   END-IF  
               END-IF  
           END-IF  
       END-PERFORM  
       DISPLAY 'Successful total: '  
            TOTAL-SUCCESS  
       STOP RUN.

see the code...
