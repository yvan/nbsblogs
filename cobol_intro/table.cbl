            *> declaring tables
            *>  table declaration can be done using the occurs keyword
            *> do not use the word 'TABLE' on its own as it is reserved.
            IDENTIFICATION DIVISION.
            PROGRAM-ID. TAB.

            DATA DIVISION.
              WORKING-STORAGE SECTION.
              *> 1d table
              01 WS-TABLE-1D.
                05 WS-A-1 PIC A(10) VALUE 'TUTORIALS' OCCURS 5 TIMES.

              *> 2d table
              01 WS-TABLE-2D.
                05 WS-A-2 OCCURS 10 TIMES.
                  10 WS-B-2 PIC A(10) VALUE ' TUTORIALS'.
                  10 WS-C-2 OCCURS 5 TIMES.
                    15 WS-D-2 PIC X(6) VALUE ' POINT'.

              *> indexed refs, subscripts
              01 WS-TABLE-IDX.
                05 WS-A-3 OCCURS 3 TIMES INDEXED BY I.
                  10 WS-B-3 PIC A(2).
                  10 WS-C-3 OCCURS 2 TIMES INDEXED BY J.
                    15 WS-D-3 PIC X(3).

              *> table search
              01 WS-TABLE-4.
                05 WS-A-4 PIC X(1) OCCURS 18 TIMES INDEXED BY IT.
              01 WS-SRCH-4 PIC A(1) VALUE 'M'.

            PROCEDURE DIVISION.
              *> indices
              MOVE '12ABCDEF34GHIJKL56MNOPQR' TO WS-TABLE-IDX.
              PERFORM A-PARA VARYING I FROM 1 BY 1 UNTIL I>3
              *> using the set command
              SET I J TO 1.
              DISPLAY 'SECOND PRINT : 'WS-C-3(I, J).
              SET I J TO 1.
              DISPLAY 'SECOND PRINT : 'WS-C-3(I, J).
              *> search
              MOVE 'ABCDEFGHIJKLMNOPQR' TO WS-TABLE-4.
              SET IT TO 1.
              SEARCH WS-A-4
                AT END DISPLAY 'M NOT FOUND IN TABLE'
                WHEN WS-A-4(IT) = WS-SRCH-4
                DISPLAY 'LETTER M FOUND IN TABLE'
              END-SEARCH.
              
              *> PRINT STUFF
              DISPLAY 'ONE-D TABLE : 'WS-TABLE-1D.
              DISPLAY 'TWO-D TABLE : 'WS-TABLE-2D.
              DISPLAY 'SIBSCRIPT : 'WS-C-2(3,1). *> (ROW, COLUMN)
              DISPLAY 'SUBSCRIPT 2 : 'WS-A-1(3).
            STOP RUN.

            A-PARA.
            PERFORM C-PARA VARYING J FROM 1 BY 1 UNTIL J>2.

            C-PARA.
            DISPLAY "FIRST PRINT : "WS-C-3(I, J).
