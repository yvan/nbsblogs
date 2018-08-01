            IDENTIFICATION DIVISION.
            PROGRAM-ID. LOOPS.

            DATA DIVISION.
              WORKING-STORAGE SECTION.
              01 WS-CNT PIC 9(1) VALUE 0.
              01 WS-A PIC 9 VALUE 0.
              01 WS-Z PIC 9 VALUE 2.

            PROCEDURE DIVISION.
              *> run the b-para-times paragraph
              *> 3 times
              PERFORM B-PARA-TIMES 3 TIMES.

              *> run b-para-until the count variable
              *> ws-cnt incremented inside the paragraph is greater than 3
              PERFORM B-PARA-UNTIL WITH TEST AFTER UNTIL WS-CNT>3.

              *> you can actually run form one paragraph
              *> to another ending paragraph
              *> through all paragrpahs in between the two
              PERFORM C-PARA-THRU THRU E-PARA-THRU.

              *> perform a varying style loop which specified
              *> a value to increment until it reaches some
              *> final
              PERFORM B-PARA-VARY VARYING WS-A FROM 1 BY 1 UNTIL WS-A=5.
              DISPLAY 'WS-A AFTER VARYING 'WS-A.
              STOP RUN.

            *> define paragraphs/functions that will
            *> be called in our loops above
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

            B-PARA-VARY.
            DISPLAY 'IN B-PARA ' WS-A.
