            *> testing redefines, renames, etc
            IDENTIFICATION DIVISION.
            PROGRAM-ID. HELLO.

            DATA DIVISION.
              WORKING-STORAGE SECTION.
              01 WS-DESCRIPTION.
              05 WS-DATE1 VALUE '20140831'.
              10 WS-YEAR PIC X(4).
              10 WS-MONTH PIC X(2).
              10 WS-DATE PIC X(2).
              05 WS-DATE2 REDEFINES WS-DATE1 PIC 9(8).

            PROCEDURE DIVISION.
              DISPLAY "WS-DATE1 : "WS-DATE1.
              DISPLAY "WS-DATE2 : "WS-DATE2.
            STOP RUN.
