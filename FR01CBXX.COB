      *=========================================*                       00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=========================================*                       00003000
                                                                        00004000
       PROGRAM-ID.  FR01CBXX.                                           00005000
                                                                        00006000
      *=======================================================*         00007000
      *     AUTOR   : IVAN SANCHES                                      00008000
      *     EMPRESA : FOURSYS                                           00009000
      *-------------------------------------------------------*         00010000
      *     DATA   : ZZ/ZZ/ZZZ                                          00020000
      *     OBJETIVO: GRAVACAO DA ARQUIVO FUNC                          00021000
      *=======================================================*         00022000
                                                                        00023000
      *=======================================================*         00024000
       ENVIRONMENT                               DIVISION.              00025000
      *=======================================================*         00026000
       INPUT-OUTPUT                              SECTION.               00026100
       FILE-CONTROL.                                                    00026200
             SELECT PRODUTO  ASSIGN TO ARQPROD                          00026300
                FILE STATUS  IS WRK-FS-PRODUTO.                         00026400
                                                                        00026500
             SELECT RESUMO   ASSIGN TO ARQRES                           00026600
                FILE STATUS  IS WRK-FS-RESUMO.                          00026700
      *=======================================================*         00026800
       DATA                                      DIVISION.              00026900
      *=======================================================*         00027000
       FILE                                      SECTION.               00028000
       FD PRODUTO                                                       00028100
           RECORDING MODE IS F                                          00028200
           BLOCK CONTAINS 0 RECORDS.                                    00028300
       01 FD-PRODUTO  PIC X(70).                                        00028400
                                                                        00028500
       FD RESUMO                                                        00028600
           RECORDING MODE IS F                                          00028700
           BLOCK CONTAINS 0 RECORDS.                                    00028800
       01 FD-RESUMO   PIC X(40).                                        00028900
      *---------------------------------------------------              00029000
       WORKING-STORAGE                           SECTION.               00029100
      *---------------------------------------------------              00029200
       77 WRK-FS-PRODUTO  PIC X(02) VALUE SPACES.                       00029300
       77 WRK-FS-RESUMO   PIC X(02) VALUE SPACES.                       00029400
       77 WRK-MSG         PIC X(50) VALUE SPACES.                       00029500
      *=========================================*                       00029600
       PROCEDURE DIVISION.                                              00029700
      *=========================================*                       00029800
                                                                        00029900
      *---------------------------------------------------              00030000
       0000-PRINCIPAL                     SECTION.                      00030100
      *---------------------------------------------------              00030200
           PERFORM 0100-INICIAR.                                        00030300
           PERFORM 0200-PROCESSAR UNTIL WRK-FS-PRODUTO EQUAL '10'.      00030406
           PERFORM 0300-FINALIZAR.                                      00030500
      *---------------------------------------------------              00030600
       0000-99-FIM.            EXIT.                                    00030700
      *---------------------------------------------------              00030800
                                                                        00030900
      *---------------------------------------------------              00031001
       0100-INICIAR                       SECTION.                      00031100
      *---------------------------------------------------              00031201
                                                                        00031309
            OPEN INPUT PRODUTO                                          00031400
                 OUTPUT RESUMO.                                         00031500
              PERFORM 0110-TESTAR-STATUS                                00031607
                 READ PRODUTO                                           00031707
              PERFORM 0120-VERIFICAR-VAZIO.                             00031807
                                                                        00031907
      *---------------------------------------------------              00032001
       0110-TESTAR-STATUS                 SECTION.                      00032100
      *---------------------------------------------------              00032201
            IF WRK-FS-PRODUTO NOT EQUAL ZEROS                           00032300
                 MOVE ' ERRO OPEN PRODUTO ' TO WRK-MSG                  00032400
                   PERFORM 9000-TRATA-ERROS                             00032500
             END-IF.                                                    00032600
            IF WRK-FS-RESUMO NOT EQUAL ZEROS                            00032700
                 MOVE ' ERRO OPEN RESUMO '  TO WRK-MSG                  00032801
                   PERFORM 9000-TRATA-ERROS                             00033000
             END-IF.                                                    00034000
      *---------------------------------------------------              00034207
       0120-VERIFICAR-VAZIO               SECTION.                      00034307
      *---------------------------------------------------              00034407
            IF WRK-FS-PRODUTO NOT EQUAL ZEROS                           00034507
                 MOVE ' ERRO OPEN PRODUTO1' TO WRK-MSG                  00034607
                   PERFORM 9000-TRATA-ERROS                             00034707
             END-IF.                                                    00034807
            IF WRK-FS-RESUMO NOT EQUAL ZEROS                            00034907
                 MOVE ' ERRO OPEN RESUMO1'  TO WRK-MSG                  00035007
                   PERFORM 9000-TRATA-ERROS                             00035107
             END-IF.                                                    00035207
      *---------------------------------------------------              00035309
       0200-PROCESSAR                     SECTION.                      00035401
      *---------------------------------------------------              00035501
                                                                        00035603
             READ PRODUTO                                               00035706
              IF WRK-FS-PRODUTO  EQUAL ZEROS                            00037012
                  MOVE FD-PRODUTO (1:30) TO FD-RESUMO                   00038013
                   WRITE FD-RESUMO FROM FD-PRODUTO                      00040012
                   DISPLAY 'TESTE' FD-RESUMO                            00041012
              ELSE                                                      00050000
                 MOVE ' FINAL DE ARQUIVO ' TO WRK-MSG                   00060000
                                                                        00061000
             END-IF.                                                    00062000
                                                                        00062103
      *---------------------------------------------------              00062302
       0300-FINALIZAR                          SECTION.                 00063000
      *---------------------------------------------------              00063102
                   CLOSE PRODUTO                                        00064000
                   CLOSE RESUMO                                         00065000
                    MOVE 'FINAL DE PROCESSO' TO WRK-MSG                 00066000
                    PERFORM 9000-TRATA-ERROS.                           00067000
      *---------------------------------------------------              00068000
       9000-TRATA-ERROS                  SECTION.                       00069000
      *---------------------------------------------------              00070000
             DISPLAY '------------------'.                              00080000
             DISPLAY '  MENSAGEM        '  WRK-MSG.                     00090000
               STOP RUN.                                                00091000
      *--------------------------------------------------------------   00092000
       9000-99-FIM.            EXIT.                                    00093000
      *--------------------------------------------------------------   00094000
                                                                        00095000
