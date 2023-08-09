
            IDENTIFICATION DIVISION.
           *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
            PROGRAM-ID. CHMADOR.
            ENVIRONMENT DIVISION.
           *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
            CONFIGURATION SECTION.
           *-----------------------
            INPUT-OUTPUT SECTION.
           *-----------------------
            DATA DIVISION.
           *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
            FILE SECTION.
           *-----------------------
            WORKING-STORAGE SECTION.
           *-----------------------
           77 WRK-DATA     PIC X(30).
           77 WRK-PROG     PIC X(08) VALUE 'MONTAMES'.

           PROCEDURE DIVISION.
                 CALL WRK-PROG   USING WRK-DATA.
                 DISPLAY WRK-DATA.


               STOP RUN.
