            *> this program doesnt work
            IDENTIFICATION DIVISION.
            PROGRAM-ID. HELLO.

            ENVIRONMENT DIVISION.
            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
            SELECT FILEN ASSIGN TO INPUT.
              ORGANIZATION IS SEQUENTIAL.
              ACCESS IS SEQUENTIAL.

            DATA DIVISION.
            FILE SECTION.
            FD FILEN
            01 NAME PIC A(25).

            WORKING-STORAGE SECTION.
            01 WS-STUDENT PIC A(30).
            01 WS-ID PIC 9(5).

            LOCAL-STORAGE SECTION.
            01 LS-CLASS PIC 9(3).

            LINKAGE SECTION.
            01 LS-ID PIC 9(5).

            PROCEDURE DIVISION.
            DISPLAY 'EXECUTING'.
            STOP RUN.
