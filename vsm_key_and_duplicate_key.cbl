       IDENTIFICATION DIVISION.
       PROGRAM-ID. VSAM-KEY-TEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 ACCOUNT-TABLE.
          05 ACCOUNT-ID OCCURS 10 TIMES PIC X(5).

       01 ACCOUNT-COUNT PIC 99 VALUE 3.
       01 I PIC 99.
       01 J PIC 99.
       01 FOUND-FLAG PIC X VALUE 'N'.

       01 NEW-ACCOUNT PIC X(5).

       01 BACKUP-TABLE.
          05 BACKUP-ID OCCURS 10 TIMES PIC X(5).

       01 BACKUP-COUNT PIC 99.

       PROCEDURE DIVISION.

           MOVE '00001' TO ACCOUNT-ID(1)
           MOVE '00002' TO ACCOUNT-ID(2)
           MOVE '00003' TO ACCOUNT-ID(3)

           DISPLAY 'INITIAL RECORD COUNT: '
                   ACCOUNT-COUNT

           DISPLAY ' '
           DISPLAY 'TEST 1: INSERT NEW ACCOUNT'

           MOVE '00004' TO NEW-ACCOUNT
           PERFORM INSERT-ACCOUNT

           DISPLAY ' '
           DISPLAY 'TEST 2: INSERT DUPLICATE KEY'

           MOVE '00002' TO NEW-ACCOUNT
           PERFORM INSERT-ACCOUNT

           DISPLAY ' '
           DISPLAY 'TEST 3: BATCH ROLLBACK'

           PERFORM BATCH-TEST

           DISPLAY ' '
           DISPLAY 'FINAL RECORD COUNT: '
                   ACCOUNT-COUNT

           STOP RUN.

       INSERT-ACCOUNT.

           MOVE 'N' TO FOUND-FLAG

           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > ACCOUNT-COUNT

               IF ACCOUNT-ID(I) = NEW-ACCOUNT
                   MOVE 'Y' TO FOUND-FLAG
               END-IF

           END-PERFORM

           IF FOUND-FLAG = 'Y'
               DISPLAY 'DUPLICATE KEY: '
                       NEW-ACCOUNT
           ELSE
               IF ACCOUNT-COUNT < 10
                   ADD 1 TO ACCOUNT-COUNT

                   MOVE NEW-ACCOUNT
                       TO ACCOUNT-ID(ACCOUNT-COUNT)

                   DISPLAY 'INSERT SUCCESS: '
                           NEW-ACCOUNT
               ELSE
                   DISPLAY 'TABLE FULL'
               END-IF
           END-IF.

       BATCH-TEST.

           MOVE ACCOUNT-TABLE TO BACKUP-TABLE
           MOVE ACCOUNT-COUNT TO BACKUP-COUNT

           MOVE '00005' TO NEW-ACCOUNT
           PERFORM INSERT-ACCOUNT

           MOVE '00003' TO NEW-ACCOUNT
           PERFORM INSERT-ACCOUNT

           IF FOUND-FLAG = 'Y'
               MOVE BACKUP-TABLE TO ACCOUNT-TABLE
               MOVE BACKUP-COUNT TO ACCOUNT-COUNT

               DISPLAY 'BATCH ROLLBACK SUCCESS'
           ELSE
               DISPLAY 'BATCH COMMITTED'
           END-IF.
