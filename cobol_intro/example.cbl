              *> example program
              IDENTIFICATION DIVISION.
              PROGRAM-ID. HELLO.

              DATA DIVISION.
                WORKING-STORAGE SECTION.
                01 WS-NAME PIC A(30).
                01 WS-ID PIC 9(5) VALUE 12345.

              PROCEDURE DIVISION.
                A000-FIRST-PARA.
                DISPLAY 'HELLO WURLD'.
                MOVE 'TUTORIAL' TO WS-NAME.
                DISPLAY 'MY NAME IS :'WS-NAME.
                DISPLAY 'MY ID IS :'WS-ID.
              STOP RUN.
