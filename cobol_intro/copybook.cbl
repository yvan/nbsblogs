            DATA DIVISION.
            WORKING-STORAGE SECTION.
            COPY ABC.
            01 WS-DESCRIPTION.
              05 WS-NUM.
                10 WS-NUM1 PIC 9(2) VALUE 20.
                10 WS-NUM2 PIC 9(2) VALUE 56.
              05 WS-CHAR.
                10 WS-CHAR1 PIC X(2) VALUE 'AA'.
                10 WS-CHAR2 PIC X(2) VALUE 'BB'.
