       >>source fixed


*>
*>  Read purchase ledger manual for details regarding all three send-mail prcesses.
*>  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*>
*>

      *****************************************************
      *  VARIABLE COPYLIB FOR SENDMAIL-PROCEDURES.CPY     *
      *                                                   *
      *  VERSION 001--ORIGINAL VERSION                    *
      *               1266048--JIM CURREY                 *
      *               06/03/2009--MARC RODRIGUEZ          *
      *  VERSION 002--CHANGED TO CORRECTLY COMPARE NULL   *
      *               POINTERS.                           *
      *               1266048--JIM CURREY                 *
      *               08/05/2009-MARC RODRIGUEZ           *
      *****************************************************
      * THE FOLLOWING VARIABLES MUST BE NULL TERMINATED!
       01  MAIL-TO-ADDRESS                     PIC X(64).
       01  MAIL-SUBJECT                        PIC X(50).
       01  MAIL-FROM-ADDRESS                   PIC X(64).
       01  MAIL-FROM-FULL-NAME                 PIC X(50).
       01  MAIL-BODY-LINE                      PIC X(256).
      * END OF NULL TERMINATED VARS
      *
      * MAIL-OUTPUT-RECORD MUST BE 2 CHARS LARGER THAN
      *  MAIL-BODY-LINE!
       01  MAIL-OUTPUT-RECORD                  PIC X(258).
       01  MAIL-RETURN                         USAGE BINARY-LONG.
      * MAIL-ERROR-TEXT WILL BE POPULATED IF AND WHEN AN ERROR OCCURS
       01  MAIL-ERROR-TEXT                     PIC X(80).
       01  MAIL-COMMAND-LINE                   PIC X(256).
       01  MAIL-TEMP-FILE-POINTER              USAGE POINTER.
080509 01  MAIL-DUMMY-POINTER                  USAGE POINTER.
       01  MAIL-TEMP-FILE-NAME                 PIC X(256).
       01  MAIL-TEMP-FILE-MODE                 PIC X VALUE "w".
      * THESE WILL BE USED AS A GENERIC CHAR * (POINTER) FOR
      *   OUR CALLS TO C
       01  MAIL-POINTER                        USAGE POINTER.
       01  MAIL-POINTER-2                      USAGE POINTER.




Next the procedure division stuff:

      *****************************************************
      *  WILL SEND A PLAIN-TEXT EMAIL VIA SENDMAIL        *
      *                                                   *
      *  VERSION 001--ORIGINAL VERSION                    *
      *               1266048--JIM CURREY                 *
      *               06/03/2009--MARC RODRIGUEZ          *
      *  VERSION 002--ADDED STANDARDS WARNING COMMENT.    *
      *               1343662--JIM CURREY                 *
      *               02/23/2010--JOSE ROSADO             *
      *  VERSION 003--CORRECTS HANDLING OF ERRORS - ON THE*
      *               FIRST ERROR, WE EXIT THE ROUTINE AND*
      *               ALLOW THE CALLING PROGRAM TO HANDLE *
      *               DISPLAYING THE ERROR (SEE /private- *
      *               set-1/progroot/task/source/         *
      *               task_entry.cbl                      *
      *               1352525--SANDY DOSS                 *
      *               07/01/2011--SANDY DOSS              *
      *****************************************************
      * BE ADVISED! THIS CODE IS NOT UP TO STANDARDS! THE *
      * FACT THIS CODE EXISTS AS IT IS DOES NOT AUTHORIZE *
      * THE USE OF SIMILAR CODE IN FUTURE PROGRAMS!       *
      *                                     --J.C. CURREY *
      *****************************************************
      * MAIL-INIT                                         *
      *                                                   *
      *   ROUTINE WILL SETUP EMAIL                        *
      *   REQUIRES: MAIL-TO-ADDRESS                       *
      *             MAIL-SUBJECT                          *
      *             MAIL-FROM-ADDRESS                     *
      *****************************************************
       MAIL-INIT.
           MOVE SPACES TO MAIL-ERROR-TEXT.
080509     INITIALIZE MAIL-DUMMY-POINTER.
           IF MAIL-TO-ADDRESS IS EQUAL TO SPACES
             THEN MOVE "MISSING TO-ADDRESS" TO MAIL-ERROR-TEXT
                  GO TO MAIL-INIT-EXIT.
           IF MAIL-SUBJECT IS EQUAL TO SPACES
             THEN MOVE "MISSING SUBJECT" TO MAIL-ERROR-TEXT
                  GO TO MAIL-INIT-EXIT.
           IF MAIL-FROM-ADDRESS IS EQUAL TO SPACES
             THEN MOVE "MISSING FROM-ADDRESS" TO MAIL-ERROR-TEXT
                  GO TO MAIL-INIT-EXIT.
      * MAIL-POINTER NOW POINTS TO MAIL-TEMP-FILE-NAME
           MOVE ADDRESS OF MAIL-TEMP-FILE-NAME TO MAIL-POINTER.
           CALL "tmpnam" USING BY VALUE MAIL-POINTER.
      * MAIL-POINTER-2 NOW POINTS TO MAIL-TEMP-FILE-MODE
           MOVE ADDRESS OF MAIL-TEMP-FILE-MODE TO MAIL-POINTER-2.
           CALL "fopen" USING     BY VALUE MAIL-POINTER
                                  BY VALUE MAIL-POINTER-2
                        RETURNING MAIL-TEMP-FILE-POINTER.
080509*    IF MAIL-TEMP-FILE-POINTER IS EQUAL TO ZERO
080509     IF MAIL-TEMP-FILE-POINTER IS EQUAL TO MAIL-DUMMY-POINTER
             THEN MOVE "UNABLE TO OPEN TEMP FILE" TO
070111*             MAIL-ERROR-TEXT.
070111              MAIL-ERROR-TEXT
070111            GO TO MAIL-INIT-EXIT.
       MAIL-WRITE-HEADERS.
           MOVE SPACES TO MAIL-BODY-LINE.
           STRING "To: " DELIMITED BY SIZE
             MAIL-TO-ADDRESS DELIMITED BY LOW-VALUE
             X"0A" DELIMITED BY SIZE
             X"00" DELIMITED BY SIZE
             INTO MAIL-BODY-LINE.
           CALL "fputs" USING     MAIL-BODY-LINE
                                  BY VALUE MAIL-TEMP-FILE-POINTER
                        RETURNING MAIL-RETURN.
           IF MAIL-RETURN IS LESS THAN OR EQUAL TO ZERO
             THEN MOVE "ERROR WRITING TO TEMPORARY FILE" TO
070111*             MAIL-ERROR-TEXT.
070111              MAIL-ERROR-TEXT
070111            GO TO MAIL-INIT-EXIT.
      *
           MOVE SPACES TO MAIL-BODY-LINE.
           STRING "Subject: " DELIMITED BY SIZE
             MAIL-SUBJECT DELIMITED BY LOW-VALUE
             X"0A" DELIMITED BY SIZE
             X"00" DELIMITED BY SIZE
             INTO MAIL-BODY-LINE.
           CALL "fputs" USING     MAIL-BODY-LINE
                                  BY VALUE MAIL-TEMP-FILE-POINTER
                        RETURNING MAIL-RETURN.
           IF MAIL-RETURN IS LESS THAN OR EQUAL TO ZERO
             THEN MOVE "ERROR WRITING TO TEMPORARY FILE" TO
070111*             MAIL-ERROR-TEXT.
070111              MAIL-ERROR-TEXT
070111            GO TO MAIL-INIT-EXIT.
      *
           MOVE SPACES TO MAIL-BODY-LINE.
           STRING "Body: " DELIMITED BY SIZE
             X"0A" DELIMITED BY SIZE
             X"00" DELIMITED BY SIZE
             INTO MAIL-BODY-LINE.
           CALL "fputs" USING     MAIL-BODY-LINE
                                  BY VALUE MAIL-TEMP-FILE-POINTER
                        RETURNING MAIL-RETURN.
           IF MAIL-RETURN IS LESS THAN OR EQUAL TO ZERO
             THEN MOVE "ERROR WRITING TO TEMPORARY FILE" TO
                    MAIL-ERROR-TEXT.
       MAIL-INIT-EXIT.
           EXIT.
      *****************************************************
      * MAIL-WRITE-BODY-LINE                              *
      *                                                   *
      * WILL WRITE A SINGLE LINE OF TEXT TO AN EMAIL      *
      * MAIL-INIT MUST BE PERFORMED BEFORE THIS ROUTINE!  *
      *****************************************************
       MAIL-WRITE-BODY-LINE.
           MOVE SPACES TO MAIL-ERROR-TEXT, MAIL-OUTPUT-RECORD.
080509*    IF MAIL-TEMP-FILE-POINTER IS EQUAL TO ZERO
080509     IF MAIL-TEMP-FILE-POINTER IS EQUAL TO MAIL-DUMMY-POINTER
             THEN MOVE "INVALID TEMP FILE POINTER" TO MAIL-ERROR-TEXT
                  GO TO MAIL-WRITE-BODY-LINE-EXIT.
           STRING MAIL-BODY-LINE DELIMITED BY LOW-VALUE
             X"0A" DELIMITED BY SIZE
             X"00" DELIMITED BY SIZE
             INTO MAIL-OUTPUT-RECORD.
           CALL "fputs" USING     MAIL-OUTPUT-RECORD
                                  BY VALUE MAIL-TEMP-FILE-POINTER
                        RETURNING MAIL-RETURN.
           IF MAIL-RETURN IS LESS THAN OR EQUAL TO ZERO
             THEN MOVE "ERROR WRITING TO TEMPORARY FILE" TO
                    MAIL-ERROR-TEXT.
       MAIL-WRITE-BODY-LINE-EXIT.
           EXIT.
      *****************************************************
      * MAIL-SEND                                         *
      *                                                   *
      * WILL ACTUALLY SEND THE EMAIL; ENDING THE PROCESS  *
      *****************************************************
       MAIL-SEND.
           MOVE SPACES TO MAIL-BODY-LINE.
           STRING "." DELIMITED BY SIZE
             X"0A" DELIMITED BY SIZE
             X"00" DELIMITED BY SIZE
             INTO MAIL-BODY-LINE.
           CALL "fputs" USING     MAIL-BODY-LINE
                                  BY VALUE MAIL-TEMP-FILE-POINTER
                        RETURNING MAIL-RETURN.
           IF MAIL-RETURN IS LESS THAN OR EQUAL TO ZERO
             THEN MOVE "ERROR WRITING TO TEMPORARY FILE" TO
070111*             MAIL-ERROR-TEXT.
070111              MAIL-ERROR-TEXT
070111            GO TO MAIL-SEND-EXIT.
      *
           CALL "fclose" USING     BY VALUE MAIL-TEMP-FILE-POINTER
                         RETURNING MAIL-RETURN.
070111     IF MAIL-RETURN IS NOT EQUAL TO ZERO
070111*             MAIL-ERROR-TEXT.
             THEN MOVE "ERROR CLOSING TEMPORARY FILE" TO
070111              MAIL-ERROR-TEXT
070111            GO TO MAIL-SEND-EXIT.
      * MAIL-POINTER NOW POINTS TO MAIL-BODY-LINE!
           MOVE ADDRESS OF MAIL-BODY-LINE TO MAIL-POINTER.
       MAIL-SEND-COMMAND.
           MOVE SPACES TO MAIL-COMMAND-LINE.
           STRING 'cat ' DELIMITED BY SIZE
             MAIL-TEMP-FILE-NAME DELIMITED BY LOW-VALUE
             ' | /usr/sbin/sendmail -t -f "'
               DELIMITED BY SIZE
             MAIL-FROM-ADDRESS DELIMITED BY LOW-VALUE
             '" -F "' DELIMITED BY SIZE
             MAIL-FROM-FULL-NAME DELIMITED BY LOW-VALUE
             '"' DELIMITED BY SIZE
             INTO MAIL-COMMAND-LINE.
           CALL "SYSTEM" USING     MAIL-COMMAND-LINE
                         RETURNING MAIL-RETURN.
070111     IF MAIL-RETURN IS GREATER THAN ZERO
             THEN MOVE "ERROR OCCURRED SENDING EMAIL" TO
070111*             MAIL-ERROR-TEXT.
070111              MAIL-ERROR-TEXT
070111            GO TO MAIL-SEND-EXIT.
       MAIL-SEND-CLEANUP.
      * MAIL-POINTER NOW POINTS TO MAIL-TEMP-FILE-NAME
           MOVE ADDRESS OF MAIL-TEMP-FILE-NAME TO MAIL-POINTER.
           CALL "remove" USING     BY VALUE MAIL-POINTER
                         RETURNING MAIL-RETURN.
           IF MAIL-RETURN IS NOT EQUAL TO ZERO
             THEN MOVE "UNABLE TO DELETE TEMP FILE" TO MAIL-ERROR-TEXT.
       MAIL-SEND-EXIT.
           EXIT.



Then the program we used to test it:

      *****************************************************
      *  WILL SEND A PLAIN-TEXT EMAIL VIA SENDMAIL        *
      *                                                   *
      *  VERSION 001--ORIGINAL VERSION                    *
      *               1266048--JIM CURREY                 *
      *               06/03/2009--MARC RODRIGUEZ          *
      *  VERSION 002--ADDED STANDARDS WARNING COMMENT.    *
      *               1343662--JIM CURREY                 *
      *               02/23/2010--JOSE ROSADO             *
      *  VERSION 003--CORRECTS HANDLING OF ERRORS - ON THE*
      *               FIRST ERROR, WE EXIT THE ROUTINE AND*
      *               ALLOW THE CALLING PROGRAM TO HANDLE *
      *               DISPLAYING THE ERROR (SEE /private- *
      *               set-1/progroot/task/source/         *
      *               task_entry.cbl                      *
      *               1352525--SANDY DOSS                 *
      *               07/01/2011--SANDY DOSS              *
      *****************************************************
      * BE ADVISED! THIS CODE IS NOT UP TO STANDARDS! THE *
      * FACT THIS CODE EXISTS AS IT IS DOES NOT AUTHORIZE *
      * THE USE OF SIMILAR CODE IN FUTURE PROGRAMS!       *
      *                                     --J.C. CURREY *
      *****************************************************
      * MAIL-INIT                                         *
      *                                                   *
      *   ROUTINE WILL SETUP EMAIL                        *
      *   REQUIRES: MAIL-TO-ADDRESS                       *
      *             MAIL-SUBJECT                          *
      *             MAIL-FROM-ADDRESS                     *
      *****************************************************
       MAIL-INIT.
           MOVE SPACES TO MAIL-ERROR-TEXT.
080509     INITIALIZE MAIL-DUMMY-POINTER.
           IF MAIL-TO-ADDRESS IS EQUAL TO SPACES
             THEN MOVE "MISSING TO-ADDRESS" TO MAIL-ERROR-TEXT
                  GO TO MAIL-INIT-EXIT.
           IF MAIL-SUBJECT IS EQUAL TO SPACES
             THEN MOVE "MISSING SUBJECT" TO MAIL-ERROR-TEXT
                  GO TO MAIL-INIT-EXIT.
           IF MAIL-FROM-ADDRESS IS EQUAL TO SPACES
             THEN MOVE "MISSING FROM-ADDRESS" TO MAIL-ERROR-TEXT
                  GO TO MAIL-INIT-EXIT.
      * MAIL-POINTER NOW POINTS TO MAIL-TEMP-FILE-NAME
           MOVE ADDRESS OF MAIL-TEMP-FILE-NAME TO MAIL-POINTER.
           CALL "tmpnam" USING BY VALUE MAIL-POINTER.
      * MAIL-POINTER-2 NOW POINTS TO MAIL-TEMP-FILE-MODE
           MOVE ADDRESS OF MAIL-TEMP-FILE-MODE TO MAIL-POINTER-2.
           CALL "fopen" USING     BY VALUE MAIL-POINTER
                                  BY VALUE MAIL-POINTER-2
                        RETURNING MAIL-TEMP-FILE-POINTER.
080509*    IF MAIL-TEMP-FILE-POINTER IS EQUAL TO ZERO
080509     IF MAIL-TEMP-FILE-POINTER IS EQUAL TO MAIL-DUMMY-POINTER
             THEN MOVE "UNABLE TO OPEN TEMP FILE" TO
070111*             MAIL-ERROR-TEXT.
070111              MAIL-ERROR-TEXT
070111            GO TO MAIL-INIT-EXIT.
       MAIL-WRITE-HEADERS.
           MOVE SPACES TO MAIL-BODY-LINE.
           STRING "To: " DELIMITED BY SIZE
             MAIL-TO-ADDRESS DELIMITED BY LOW-VALUE
             X"0A" DELIMITED BY SIZE
             X"00" DELIMITED BY SIZE
             INTO MAIL-BODY-LINE.
           CALL "fputs" USING     MAIL-BODY-LINE
                                  BY VALUE MAIL-TEMP-FILE-POINTER
                        RETURNING MAIL-RETURN.
           IF MAIL-RETURN IS LESS THAN OR EQUAL TO ZERO
             THEN MOVE "ERROR WRITING TO TEMPORARY FILE" TO
070111*             MAIL-ERROR-TEXT.
070111              MAIL-ERROR-TEXT
070111            GO TO MAIL-INIT-EXIT.
      *
           MOVE SPACES TO MAIL-BODY-LINE.
           STRING "Subject: " DELIMITED BY SIZE
             MAIL-SUBJECT DELIMITED BY LOW-VALUE
             X"0A" DELIMITED BY SIZE
             X"00" DELIMITED BY SIZE
             INTO MAIL-BODY-LINE.
           CALL "fputs" USING     MAIL-BODY-LINE
                                  BY VALUE MAIL-TEMP-FILE-POINTER
                        RETURNING MAIL-RETURN.
           IF MAIL-RETURN IS LESS THAN OR EQUAL TO ZERO
             THEN MOVE "ERROR WRITING TO TEMPORARY FILE" TO
070111*             MAIL-ERROR-TEXT.
070111              MAIL-ERROR-TEXT
070111            GO TO MAIL-INIT-EXIT.
      *
           MOVE SPACES TO MAIL-BODY-LINE.
           STRING "Body: " DELIMITED BY SIZE
             X"0A" DELIMITED BY SIZE
             X"00" DELIMITED BY SIZE
             INTO MAIL-BODY-LINE.
           CALL "fputs" USING     MAIL-BODY-LINE
                                  BY VALUE MAIL-TEMP-FILE-POINTER
                        RETURNING MAIL-RETURN.
           IF MAIL-RETURN IS LESS THAN OR EQUAL TO ZERO
             THEN MOVE "ERROR WRITING TO TEMPORARY FILE" TO
                    MAIL-ERROR-TEXT.
       MAIL-INIT-EXIT.
           EXIT.
      *****************************************************
      * MAIL-WRITE-BODY-LINE                              *
      *                                                   *
      * WILL WRITE A SINGLE LINE OF TEXT TO AN EMAIL      *
      * MAIL-INIT MUST BE PERFORMED BEFORE THIS ROUTINE!  *
      *****************************************************
       MAIL-WRITE-BODY-LINE.
           MOVE SPACES TO MAIL-ERROR-TEXT, MAIL-OUTPUT-RECORD.
080509*    IF MAIL-TEMP-FILE-POINTER IS EQUAL TO ZERO
080509     IF MAIL-TEMP-FILE-POINTER IS EQUAL TO MAIL-DUMMY-POINTER
             THEN MOVE "INVALID TEMP FILE POINTER" TO MAIL-ERROR-TEXT
                  GO TO MAIL-WRITE-BODY-LINE-EXIT.
           STRING MAIL-BODY-LINE DELIMITED BY LOW-VALUE
             X"0A" DELIMITED BY SIZE
             X"00" DELIMITED BY SIZE
             INTO MAIL-OUTPUT-RECORD.
           CALL "fputs" USING     MAIL-OUTPUT-RECORD
                                  BY VALUE MAIL-TEMP-FILE-POINTER
                        RETURNING MAIL-RETURN.
           IF MAIL-RETURN IS LESS THAN OR EQUAL TO ZERO
             THEN MOVE "ERROR WRITING TO TEMPORARY FILE" TO
                    MAIL-ERROR-TEXT.
       MAIL-WRITE-BODY-LINE-EXIT.
           EXIT.
      *****************************************************
      * MAIL-SEND                                         *
      *                                                   *
      * WILL ACTUALLY SEND THE EMAIL; ENDING THE PROCESS  *
      *****************************************************
       MAIL-SEND.
           MOVE SPACES TO MAIL-BODY-LINE.
           STRING "." DELIMITED BY SIZE
             X"0A" DELIMITED BY SIZE
             X"00" DELIMITED BY SIZE
             INTO MAIL-BODY-LINE.
           CALL "fputs" USING     MAIL-BODY-LINE
                                  BY VALUE MAIL-TEMP-FILE-POINTER
                        RETURNING MAIL-RETURN.
           IF MAIL-RETURN IS LESS THAN OR EQUAL TO ZERO
             THEN MOVE "ERROR WRITING TO TEMPORARY FILE" TO
070111*             MAIL-ERROR-TEXT.
070111              MAIL-ERROR-TEXT
070111            GO TO MAIL-SEND-EXIT.
      *
           CALL "fclose" USING     BY VALUE MAIL-TEMP-FILE-POINTER
                         RETURNING MAIL-RETURN.
070111     IF MAIL-RETURN IS NOT EQUAL TO ZERO
070111*             MAIL-ERROR-TEXT.
             THEN MOVE "ERROR CLOSING TEMPORARY FILE" TO
070111              MAIL-ERROR-TEXT
070111            GO TO MAIL-SEND-EXIT.
      * MAIL-POINTER NOW POINTS TO MAIL-BODY-LINE!
           MOVE ADDRESS OF MAIL-BODY-LINE TO MAIL-POINTER.
       MAIL-SEND-COMMAND.
           MOVE SPACES TO MAIL-COMMAND-LINE.
           STRING 'cat ' DELIMITED BY SIZE
             MAIL-TEMP-FILE-NAME DELIMITED BY LOW-VALUE
             ' | /usr/sbin/sendmail -t -f "'
               DELIMITED BY SIZE
             MAIL-FROM-ADDRESS DELIMITED BY LOW-VALUE
             '" -F "' DELIMITED BY SIZE
             MAIL-FROM-FULL-NAME DELIMITED BY LOW-VALUE
             '"' DELIMITED BY SIZE
             INTO MAIL-COMMAND-LINE.
           CALL "SYSTEM" USING     MAIL-COMMAND-LINE
                         RETURNING MAIL-RETURN.
070111     IF MAIL-RETURN IS GREATER THAN ZERO
             THEN MOVE "ERROR OCCURRED SENDING EMAIL" TO
070111*             MAIL-ERROR-TEXT.
070111              MAIL-ERROR-TEXT
070111            GO TO MAIL-SEND-EXIT.
       MAIL-SEND-CLEANUP.
      * MAIL-POINTER NOW POINTS TO MAIL-TEMP-FILE-NAME
           MOVE ADDRESS OF MAIL-TEMP-FILE-NAME TO MAIL-POINTER.
           CALL "remove" USING     BY VALUE MAIL-POINTER
                         RETURNING MAIL-RETURN.
           IF MAIL-RETURN IS NOT EQUAL TO ZERO
             THEN MOVE "UNABLE TO DELETE TEMP FILE" TO MAIL-ERROR-TEXT.
       MAIL-SEND-EXIT.
           EXIT.
