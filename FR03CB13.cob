      *===========================================================      00001031
       IDENTIFICATION                             DIVISION.             00002031
      *===========================================================      00003031
       PROGRAM-ID.  FR03CB13.                                           00004031
      *===========================================================      00005031
      *     AUTOR    : AUGUSTO MARTINS                                  00006031
      *     EMPRESA  : FOURSYS                                          00007031
      *     DATA     : 24/04/2022                                       00008032
      *     OBJETIVO : OPERADOR ARITMETICO, DIVIDE.                     00009031
      *              : RECEBER DOIS VALORES DA SYSIN, CALCULAR          00010031
      *              : O VALOR TOTAL DAS PARCELAS.                      00011031
      *===========================================================*     00012034
       ENVIRONMENT                                DIVISION.             00013034
      *===========================================================*     00014034
       DATA                                       DIVISION.             00015031
      *===========================================================*     00016034
                                                                        00016134
      *-----------------------------------------------------------*     00016234
       WORKING-STORAGE                            SECTION.              00017031
      *-----------------------------------------------------------*     00018034
                                                                        00018134
        01 WRK-VALOR-COMP           PIC 9(05) COMP-3 VALUE ZEROS.       00018231
        01 WRK-PARCEL-COMP          PIC 9(03) COMP-3 VALUE ZEROS.       00018331
        01 WRK-TOTAL-C              PIC 9(05) COMP-3 VALUE ZEROS.       00018431
        01 WRK-DIVIDE.                                                  00018531
           05 WRK-VALOR             PIC 9(05) VALUE ZEROS.              00018631
           05 WRK-QTPARCELAS        PIC 9(03) VALUE ZEROS.              00018731
           05 WRK-TOTAL             PIC 9(05) VALUE ZEROS.              00018831
                                                                        00018934
      *-----------------------------------------------------------*     00019034
                                                                        00019134
      *===========================================================*     00019234
       PROCEDURE                                  DIVISION.             00019331
      *==========================================================       00019431
                                                                        00020031
           ACCEPT WRK-DIVIDE     FROM SYSIN.                            00030031
      * ----------------- DIVIDE ---------------------                  00040031
                                                                        00040131
           MOVE   WRK-VALOR       TO WRK-VALOR-COMP.                    00040231
           MOVE   WRK-QTPARCELAS  TO WRK-PARCEL-COMP.                   00040331
           MOVE   WRK-TOTAL       TO WRK-TOTAL-C.                       00040431
                                                                        00040531
           DIVIDE WRK-VALOR-COMP  BY WRK-PARCEL-COMP GIVING WRK-TOTAL-C 00040633
                                     ON SIZE ERROR                      00040733
                                        DISPLAY 'ESTOURO DA DIVISAO'    00040833
           END-DIVIDE.                                                  00040933
                                                                        00041033
           MOVE   WRK-VALOR-COMP  TO WRK-VALOR.                         00041131
           MOVE   WRK-PARCEL-COMP TO WRK-QTPARCELAS.                    00041231
           MOVE   WRK-TOTAL-C     TO WRK-TOTAL.                         00041331
                                                                        00042031
           DISPLAY '===================================='               00043031
           DISPLAY 'VALOR        : '   WRK-VALOR                        00044031
           DISPLAY 'QT.PARCELAS  : '   WRK-QTPARCELAS                   00045031
           DISPLAY 'TOTAL        : '   WRK-TOTAL.                       00046031
           DISPLAY '==================================== '.             00047031
                                                                        00048031
           STOP RUN.                                                    00049031
