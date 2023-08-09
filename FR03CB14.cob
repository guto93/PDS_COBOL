      *===========================================================*     00001020
       IDENTIFICATION                             DIVISION.             00002000
      *===========================================================*     00003020
       PROGRAM-ID.  FR03CB14.                                           00004000
      *===========================================================*     00005020
      *     AUTOR    : AUGUSTO MARTINS                                  00006000
      *     EMPRESA  : FOURSYS                                          00007000
      *     DATA     : 24/04/2022                                       00008011
      *     OBJETIVO : OPERADOR ARITMETICO, MULTIPLY                    00009018
      *              : RECEBER DOIS VALORES DA SYSIN E DEVOLVER         00010000
      *              : O RESULTADO.                                     00011018
      *===========================================================*     00012020
       ENVIRONMENT                                DIVISION.             00013020
      *===========================================================*     00014020
       DATA                                       DIVISION.             00015000
      *===========================================================*     00016020
                                                                        00016120
      *-----------------------------------------------------------*     00016220
       WORKING-STORAGE                            SECTION.              00017000
      *-----------------------------------------------------------*     00018020
                                                                        00018120
        01 WRK-SALARIO-COMP         PIC 9(06) COMP-3 VALUE ZEROS.       00018206
        01 WRK-INDICE-COMP          PIC 9(03) COMP-3 VALUE ZEROS.       00018306
        01 WRK-TOTAL-C              PIC 9(06) COMP-3 VALUE ZEROS.       00018406
        01 WRK-SAL-CORR             PIC 9(04) COMP-3 VALUE ZEROS.       00018517
        01 WRK-DIVIDE.                                                  00018600
           05 WRK-SALARIO           PIC 9(06) VALUE ZEROS.              00018706
           05 WRK-INDICE            PIC 9(03) VALUE ZEROS.              00018806
           05 WRK-TOTAL             PIC 9(06) VALUE ZEROS.              00018906
           05 WRK-CORR              PIC 9(04) VALUE ZEROS.              00019017
                                                                        00019120
      *-----------------------------------------------------------*     00019220
                                                                        00019320
      *===========================================================*     00019420
       PROCEDURE                                  DIVISION.             00019509
      *===========================================================*     00019620
                                                                        00020000
           ACCEPT WRK-DIVIDE     FROM SYSIN.                            00030000
      * -----------------    MULTIPLY ---------------------------*      00040022
                                                                        00040100
           MOVE   WRK-SALARIO        TO WRK-SALARIO-COMP.               00041007
           MOVE   WRK-INDICE         TO WRK-INDICE-COMP.                00042007
           MOVE   WRK-TOTAL          TO WRK-TOTAL-C.                    00043007
           MOVE   WRK-CORR           TO WRK-SAL-CORR.                   00044009
           MULTIPLY WRK-SALARIO-COMP BY WRK-INDICE-COMP                 00050007
                                          GIVING WRK-TOTAL-C            00054021
           END-MULTIPLY.                                                00054121
                                                                        00054221
           COMPUTE WRK-SAL-CORR = WRK-SALARIO-COMP + WRK-SALARIO-COMP * 00055012
                                                  WRK-INDICE-COMP / 100 00056020
           END-COMPUTE.                                                 00058021
                                                                        00059020
           MOVE   WRK-SALARIO-COMP   TO WRK-SALARIO.                    00060007
           MOVE   WRK-INDICE-COMP    TO WRK-INDICE.                     00070007
           MOVE   WRK-TOTAL-C        TO WRK-TOTAL.                      00071009
           MOVE   WRK-SAL-CORR       TO WRK-CORR.                       00071109
                                                                        00071200
           DISPLAY '===================================='               00071300
           DISPLAY 'VALOR SALARIO     : ' WRK-SALARIO.                  00071415
           DISPLAY 'INDICE            : ' WRK-INDICE.                   00071515
           DISPLAY 'TOTAL SALARIO     : ' WRK-TOTAL.                    00071615
           DISPLAY 'SALARIO CORRIGIDO : ' WRK-CORR.                     00071715
           DISPLAY '==================================== '.             00071808
           STOP RUN.                                                    00072008
