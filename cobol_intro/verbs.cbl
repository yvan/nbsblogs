            *> this is about basic verbs
            IDENTIFICATION DIVISION.
            PROGRAM-ID. YOMAMA.

            DATA DIVISION.
              WORKING-STORAGE SECTION.
              01 WS-NUM1 PIC 9(9) VALUE 10.
              01 WS-NUM2 PIC 9(9) VALUE 10.
              01 WS-NUMA PIC 9(9) VALUE 100.
              01 WS-NUMB PIC 9(9) VALUE 15.
              01 WS-NUMC PIC 9(9).
              01 WS-RES-DIV PIC 9(9).
              01 WS-RES-MULT PIC 9(9).
              01 WS-RES-SUB PIC 9(9).
              01 WS-RES-ADD PIC 9(9).
              01 WS-RES-MOV PIC X(9).

            PROCEDURE DIVISION.
              COMPUTE WS-NUMC = (WS-NUM1 * WS-NUM2).
              DIVIDE WS-NUMA BY WS-NUMB GIVING WS-RES-DIV.
              MULTIPLY WS-NUMA BY WS-NUMB GIVING WS-RES-MULT.
              SUBTRACT WS-NUMA FROM WS-NUMB GIVING WS-RES-SUB.
              ADD WS-NUMA TO WS-NUMB GIVING WS-RES-ADD.
              MOVE WS-NUMA TO WS-RES-MOV.
              INITIALIZE WS-NUM1.
              INITIALIZE WS-NUM2 REPLACING NUMERIC DATA BY 12345.
              DISPLAY "WS-NUMC:"WS-NUMC
              DISPLAY "WS-RES-DIV:"WS-RES-DIV
              DISPLAY "WS-RES-MULT:"WS-RES-MULT
              DISPLAY "WS-RES-SUB:"WS-RES-SUB
              DISPLAY "WS-RES-ADD:"WS-RES-ADD
              DISPLAY "WS-RES-MOV:"WS-RES-MOV
              DISPLAY "REINITIALIZED WS-NUM1: "WS-NUM1
              DISPLAY "REINITIALIZED WS-NUM2: "WS-NUM2
            STOP RUN.
