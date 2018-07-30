            IDENTIFICATION DIVISION.
            PROGRAM-ID. TABLE.

            DATA DIVISION.
              WORKING-STORAGE SECTION.
              01 WS-TABLE.
                05 WS-A PIC A(10) VALUE 'TUTORIALS' OCCURS 5 TIMES.

            PROCEDURE DIVISION.
              DISPLAY 'ONE-D TABLE : 'WS-TABLE.
            STOP RUN.
