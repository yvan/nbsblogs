            IDENTIFICATION DIVISION.
            PROGRAM-ID. STRINGHANDLE.

            DATA DIVISION.
              WORKING-STORAGE SECTION.
              01 WS-CNT1 PIC 9(2) VALUE 0.
              01 WS-CNT2 PIC 9(2) VALUE 0.
              01 WS-STRING PIC X(25) VALUE 'ABCDADADADABVDFDFFAF'.
              01 WS-STRING-DEST PIC A(30).
              01 WS-STR1 PIC A(15) VALUE 'TUTORIALSPOINT'.
              01 WS-STR2 PIC A(7) VALUE 'WELCOME'.
              01 WS-STR3 PIC A(7) VALUE 'TO AND'.
              01 WS-COUNT PIC 99 VALUE 1.
              01 WS-UNSTR PIC A(30) VALUE 'WELCOME TO TUTORIALSPOINT'.

            PROCEDURE DIVISION.
              *> count the number of chars in string, store in ws-cnt1
              INSPECT WS-STRING TALLYING WS-CNT1 FOR ALL CHARACTERS.
              DISPLAY "WS-CNT1 : "WS-CNT1.
              *> count just the A characters
              INSPECT WS-STRING TALLYING WS-CNT2 FOR ALL 'A'.
              DISPLAY "WS-CNT2 : "WS-CNT2.
              *> replace A chars with X in strings
              DISPLAY "OLD STRING : "WS-STRING.
              INSPECT WS-STRING REPLACING ALL 'A' BY 'X'.
              DISPLAY "NEW STRING : "WS-STRING.
              *> string concatenate
              STRING WS-STR2 DELIMITED BY SIZE
                WS-STR3 DELIMITED BY SPACE
                WS-STR1 DELIMITED BY SIZE
                INTO WS-STRING-DEST
                WITH POINTER WS-COUNT
                ON OVERFLOW DISPLAY 'OVERFLOW!'
              END-STRING.
              DISPLAY 'WS-STRING : 'WS-STRING-DEST.
              DISPLAY 'WS-COUNT : 'WS-COUNT.
              *> string split
              UNSTRING WS-UNSTR DELIMITED BY SPACE
                INTO WS-STR3, WS-STR2, WS-STR1
              END-UNSTRING.
              DISPLAY 'WS-STR1 : 'WS-STR1.
              DISPLAY 'WS-STR2 : 'WS-STR2.
              DISPLAY 'WS-STR3 : 'WS-STR3.
            STOP RUN.
