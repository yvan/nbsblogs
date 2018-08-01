            *> example of using pic clause
            IDENTIFICATION DIVISION.
            PROGRAM-ID. HELLO.

            *> PIC stands for picture and can be used to define data
            *> 9 = numeric, A = alphabetic, X = alphanumeric, V = decimal, S = sign,
            *> P = assumed decimal, numbers at the start are 01 = desciption(name),
            *> 02-49 = numbers for groups, 66 = rename clause items, 77 = items cannot
            *> be subdivded, 88 = condition name entry
            DATA DIVISION.
              WORKING-STORAGE SECTION.
              01 WS-NUM1 PIC S9(3)V9(2).
              01 WS-NUM2 PIC PPP999.
              01 WS-NUM3 PIC S9(3)V9(2) VALUE -123.45.
              01 WS-NAME PIC A(6) VALUE 'ABCDEF'.
              01 WS-ID PIC X(5) VALUE 'A121$'.
              01 WS-ADDRESS.
                05 WS-HOUSE-NUMBER PIC 9(3) VALUE 337.
                05 WS-STREET PIC X(15) VALUE 'YOMAMAMA'.
                05 WS-CITY PIC X(15) VALUE 'YOMAMAMAM'.
                05 WS-COUNTRY PIC X(15) VALUE 'INDIA'.

            PROCEDURE DIVISION.
              DISPLAY "WS-NUM1 :"WS-NUM1.
              DISPLAY "WS-NUM2 :"WS-NUM2.
              DISPLAY "WS-NUM3 :"WS-NUM3.
              DISPLAY "WS-NAME :"WS-NAME.
              DISPLAY "W-ID :"WS-ID.
              DISPLAY "WS-ADDRESS :"WS-ADDRESS.
              STOP RUN.
