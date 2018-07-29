            *> if statements
            *> relation statements like greater or less than
            *> sign conditions for checking if a thing is positive or negative
            *> class conditions, (type for comaprisons in every other language)
            *> custom conditions (PASS/FAIL below)
            *> negated conditions
            *> combined condition (conditions with AND / OR)
            *> evaluate aka a switch statement
            IDENTIFICATION DIVISION.
            PROGRAM-ID. CONDITIONALS.

            DATA DIVISION.
              WORKING-STORAGE SECTION.
              01 WS-NUM1 PIC 9(9).
              01 WS-NUM2 PIC 9(9).
              01 WS-NUM3 PIC 9(5).
              01 WS-NUM4 PIC 9(6).
              01 NEG-NUM PIC S9(9) VALUE -1234.
              01 POS-NUM PIC S9(9) VALUE 123456.
              01 CLASS1 PIC X(9) VALUE 'ABCD '.
              01 CLASS2 PIC 9(9) VALUE 123456789.
              01 CHECK-VAL PIC 9(3).
              88 PASS VALUES ARE 041 THRU 100.
              88 FAIL VALUES ARE 000 THRU 40.

            PROCEDURE DIVISION.
              *> WHY DO WE NEED THIS?
              *> A000-FIRST-PARA?
              A000-FIRST-PARA.
              MOVE 25 TO WS-NUM1 WS-NUM3.
              MOVE 15 TO WS-NUM2 WS-NUM4.

              IF WS-NUM1 > WS-NUM2 THEN
                DISPLAY 'IN LOOP 1 - IF BLOCK'
                IF WS-NUM3 = WS-NUM4 THEN
                  DISPLAY 'IN LOOP 2 - IF BLOCK'
                ELSE
                  DISPLAY 'IN LOOP 2 - ELSE BLOCK'
                END-IF
              ELSE
                DISPLAY 'IN LOOP 1 -ELSE BLOCK'
              END-IF

              IF WS-NUM1 IS GREATER THAN OR EQUAL TO WS-NUM2 THEN
                DISPLAY 'WS-NUM1 IS GREATER THAN WS-NUM2'
              ELSE
                DISPLAY 'WS-NUM1 IS LESS THAN WS-NUM2'
              END-IF.

              IF NEG-NUM IS POSITIVE THEN
                DISPLAY 'NEG-NUM IS POSITIVE'.
              IF NEG-NUM IS NEGATIVE THEN
                DISPLAY 'NEG-NUM IS NEGATIVE'.
              IF POS-NUM IS POSITIVE THEN
                DISPLAY 'POS-NUM IS POSITIVE'.
              IF POS-NUM IS NEGATIVE THEN
                DISPLAY 'POS-NUM US NEGATIVE'.

              IF CLASS1 IS ALPHABETIC THEN
                DISPLAY 'CLASS1 IS ALPHABETIC'.
              IF CLASS1 IS NUMERIC THEN
                DISPLAY 'CLASS1 IS NUMERIC'.
              IF CLASS2 IS NUMERIC THEN
                DISPLAY 'CLASS2 IS NUMERIC'.

              MOVE 65 TO CHECK-VAL.
              IF PASS
                DISPLAY 'PASSED WITH 'CHECK-VAL' MARKS.'
              IF FAIL
                DISPLAY 'FAILED WITH 'CHECK-VAL' MARKS.'

              MOVE 50 TO WS-NUM1.
              MOVE 60 TO WS-NUM2.
              IF NOT WS-NUM1 IS LESS THAN WS-NUM2 THEN
                DISPLAY 'IF-BLOCK'
              ELSE
                DISPLAY 'ELSE-BLOCK'
              END-IF

              IF WS-NUM1 IS LESS THAN WS-NUM2 AND WS-NUM1 IS LESS THAN 100 THEN
                DISPLAY 'COMBINED CONDITION'
              ELSE
                DISPLAY 'NAH'
              END-IF

              EVALUATE TRUE
                WHEN WS-NUM1 < 2
                  DISPLAY 'WS-NUM1 LESS THAN 2'
                WHEN WS-NUM1 < 19
                  DISPLAY 'WS-NUM1 LESS THAN 19'
                WHEN WS-NUM1 < 1000
                  DISPLAY 'WS-NUM1 LESS THAN 1000'
              END-EVALUATE.

            STOP RUN.
