            IDENTIFICATION DIVISION.
            PROGRAM-ID. CONDITIONALS.

            DATA DIVISION.
              WORKING-STORAGE SECTION.
              *> setting up places to store values
              *> no values set yet
              01 NUM1 PIC 9(9).
              01 NUM2 PIC 9(9).
              01 NUM3 PIC 9(5).
              01 NUM4 PIC 9(6).
              *> create a positive and a negative
              *> number to check
              01 NEG-NUM PIC S9(9) VALUE -1234.
              *> create variables for testing classes
              01 CLASS1 PIC X(9) VALUE 'ABCD '.
              *> create statements that can be fed
              *> into a cobol conditional
              01 CHECK-VAL PIC 9(3).
                88 PASS VALUES ARE 041 THRU 100.
                88 FAIL VALUES ARE 000 THRU 40.

            PROCEDURE DIVISION.
              *> set 25 into num1 and num3
              *> set 15 into num2 and num4
              MOVE 25 TO NUM1 NUM3.
              MOVE 15 TO NUM2 NUM4.

              *> comparing two numbers and checking for equality
              IF NUM1 > NUM2 THEN
                DISPLAY 'IN LOOP 1 - IF BLOCK'
                IF NUM3 = NUM4 THEN
                  DISPLAY 'IN LOOP 2 - IF BLOCK'
                ELSE
                  DISPLAY 'IN LOOP 2 - ELSE BLOCK'
                END-IF
              ELSE
                DISPLAY 'IN LOOP 1 -ELSE BLOCK'
              END-IF

              *> use a custom pre-defined condition
              *> which checks CHECK-VAL
              MOVE 65 TO CHECK-VAL.
              IF PASS
                DISPLAY 'PASSED WITH 'CHECK-VAL' MARKS.'.
              IF FAIL
                DISPLAY 'FAILED WITH 'CHECK-VAL' MARKS.'.

              *> a switch statment
              EVALUATE TRUE
                WHEN NUM1 < 2
                  DISPLAY 'NUM1 LESS THAN 2'
                WHEN NUM1 < 19
                  DISPLAY 'NUM1 LESS THAN 19'
                WHEN NUM1 < 1000
                  DISPLAY 'NUM1 LESS THAN 1000'
              END-EVALUATE.
            STOP RUN.
