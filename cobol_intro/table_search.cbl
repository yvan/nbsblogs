            *> doesnt work
            IDENTIFICATION DIVISION.
            PROGRAM-ID. HELLO.

            DATA DIVISION.
               WORKING-STORAGE SECTION.
               01 WS-TABLE.
                  05 WS-RECORD OCCURS 10 TIMES ASCENDING KEY IS WS-NUM INDEXED BY I.
                  10 WS-NUM PIC 9(2).
                  10 WS-NAME PIC A(3).

            PROCEDURE DIVISION.
               MOVE '12ABC56DEF34GHI78JKL93MNO11PQR' TO WS-TABLE.
               SEARCH ALL WS-RECORD
                 AT END DISPLAY 'RECORD NOT FOUND'
                 WHEN WS-NUM(I) = 93
                 DISPLAY 'RECORD FOUND '
                 DISPLAY WS-NUM(I)
                 DISPLAY WS-NAME(I)

            END-SEARCH.
