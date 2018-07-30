            *> perform thru, until, times, varying -- all kinds of loops
            *> goto and goto depending on -- control program flow
            IDENTIFICATION DIVISION.
            PROGRAM-ID. LOOPS.

            DATA DIVISION.
              WORKING-STORAGE SECTION.
              01 WS-CNT PIC 9(1) VALUE 0.
              01 WS-A PIC 9 VALUE 0.
              01 WS-Z PIC 9 VALUE 2.

            PROCEDURE DIVISION.
              *> goto
              A-PARA-GOTO.
              DISPLAY 'PARA-A GOTO'
              GO TO B-PARA-GOTO.

              *> DEPENDENT GOTO THAT WILL GO TO 2
              *> WS-Z IS SET TO 2.
              B-PARA-GOTO.
              DISPLAY 'B-PARA-GOTO'.
              GO TO C-PARA-GOTO D-PARA-GOTO DEPENDING ON WS-Z.

              C-PARA-GOTO.
              DISPLAY 'IN-C-PARA-GOTO'.

              D-PARA-GOTO.
              DISPLAY 'IN D-PARA-GOTO'.

              *> OUR VARYING LOOP (ITS A CUSTOM LOOP)
              A-PARA-VARY.
              PERFORM B-PARA-VARY VARYING WS-A FROM 1 BY 1 UNTIL WS-A=5.

              *> OUR TIMES LOOP
              A-PARA-TIMES.
              PERFORM B-PARA-TIMES 3 TIMES.

              *> OUR UNTIL LOOP
              A-PARA-UNTIL.
              PERFORM B-PARA-UNTIL WITH TEST AFTER UNTIL WS-CNT>3.

              *> OUR THRU LOOP
              A-PARA-THRU.
              PERFORM DISPLAY 'IN A-PARA-THRU'
              END-PERFORM.
              PERFORM C-PARA-THRU THRU E-PARA-THRU.

              B-PARA-THRU.
              DISPLAY 'IN B-PARA-THRU'.
              STOP RUN.

              B-PARA-VARY.
              DISPLAY 'IN B-PARA ' WS-A.

              B-PARA-TIMES.
              DISPLAY 'IN B-PARA-TIMES'.

              B-PARA-UNTIL.
              DISPLAY 'WS-CNT : 'WS-CNT
              ADD 1 TO WS-CNT.

              C-PARA-THRU.
              DISPLAY 'IN C-PARA-THRU'.

              D-PARA-THRU.
              DISPLAY 'IN D-PARA-THRU'.

              E-PARA-THRU.
              DISPLAY 'IN E-PARA-THRU'.
