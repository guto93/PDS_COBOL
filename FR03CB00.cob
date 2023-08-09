       IDENTIFICATION                            DIVISION.              00010000
       PROGRAM-ID.  FR03CB00.                                           00020006
      *=====================================================            00030006
      *     AUTOR    : AUGUSTO MARTINS                                  00040000
      *     EMPRESA  : FOURSYS                                          00050000
      *     DATA     : 18/04/2022                                       00060000
      *     OBJETIVO : TESTE DE COMPILACAO, MOVER DADOS                 00070006
      *              : PARA AS VARIAVEIS.                               00071006
      *=====================================================            00080006
      *ENVIROMENT                                 DIVISION.             00081000
       DATA                                       DIVISION.             00082000
       WORKING-STORAGE                            SECTION.              00082100
       77 WRK-NOME       PICTURE X(15).                                 00082200
       77 WRK-SOBRENOME  PICTURE X(15).                                 00082300
       77 WRK-NOME-TEC   PICTURE X(15).                                 00082400
       PROCEDURE                                  DIVISION.             00082500
           MOVE 'AUGUSTO'    TO WRK-NOME.                               00082600
           MOVE 'MARTINS'    TO WRK-SOBRENOME.                          00082700
           MOVE 'COBOL'      TO WRK-NOME-TEC.                           00082800
           DISPLAY 'MEU PRIMEIRO PROGRAMA - FR03CB02'.                  00082900
           DISPLAY 'FEITO POR --- ' WRK-NOME WRK-SOBRENOME.             00083003
           DISPLAY 'TIPO DE LINGUAGEM: ' WRK-NOME-TEC.                  00083104
           STOP RUN.                                                    00084000
