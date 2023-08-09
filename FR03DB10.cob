                                                                        00001000
      *=======================================================*         00002000
       IDENTIFICATION                            DIVISION.              00003000
      *=======================================================*         00004000
                                                                        00005000
       PROGRAM-ID.  FR03DB10.                                           00006000
                                                                        00007000
      *=======================================================*         00008000
      *              TREINAMENTO MAINFRAME                    *         00009000
      *=======================================================*         00010000
      *     AUTOR    : AUGUSTO MARTINS                                  00020000
      *     EMPRESA  : FOURSYS                                          00030000
      *     DATA     : 07/06/2022                                       00040000
      *     OBJETIVO : FAZER UPDATE DE DADOS, E IMPRIMIR O QUE          00050000
      *              : FOI ALTERADO.                                    00060000
      *=======================================================*         00070000
       ENVIRONMENT                                    DIVISION.         00080000
      *=======================================================*         00081000
                                                                        00081100
      *=======================================================*         00081200
       CONFIGURATION                                  SECTION.          00081300
      *=======================================================*         00081400
                                                                        00081500
          SPECIAL-NAMES.                                                00081600
              DECIMAL-POINT IS COMMA.                                   00081700
                                                                        00081800
      *=======================================================*         00081900
                                                                        00082000
      *=======================================================*         00082100
       DATA                                       DIVISION.             00082200
      *=======================================================*         00082300
                                                                        00082400
      *-------------------------------------------------------*         00082500
       WORKING-STORAGE                            SECTION.              00082600
      *-------------------------------------------------------*         00082700
                                                                        00082800
      *-------------------------------------------------------*         00082900
        01 FILLER                          PIC X(050)   VALUE           00083000
                '*******AREA DE SQL ******'.                            00083100
      *-------------------------------------------------------*         00083200
                                                                        00083300
           EXEC SQL                                                     00083400
                INCLUDE BKFUNC2                                         00083500
           END-EXEC.                                                    00083600
           EXEC SQL                                                     00083700
                INCLUDE SQLCA                                           00083800
           END-EXEC.                                                    00083900
                                                                        00084000
      *-------------------------------------------------------*         00084100
        01 FILLER                          PIC X(050)   VALUE           00084200
                '*******AREA DE AUXILIARES********'.                    00084300
      *-------------------------------------------------------*         00084400
                                                                        00084500
      *-------------------------------------------------------*         00084600
                                                                        00084700
                                                                        00084800
      *-------------------------------------------------------*         00084900
        01 FILLER                          PIC X(050)   VALUE           00085000
                '*******AREA DE AUXILIARES********'.                    00086000
      *-------------------------------------------------------*         00086100
                                                                        00086200
       77 WRK-SQLCODE       PIC -999.                                   00086300
       77 WRK-INDICATOR     PIC S9(04) COMP VALUE ZEROS.                00086400
       77 WRK-ID-DB         PIC  9(04).                                 00086500
       77 WRK-ALT-NOME      PIC X(30).                                  00086614
       77 WRK-ALT-SETOR     PIC X(04).                                  00086714
       77 WRK-ALT-SALARIO   PIC 9(08)V99.                               00086814
       77 WRK-ALT-DATAADM   PIC X(10).                                  00086914
       77 WRK-ALT-EMAIL     PIC X(40).                                  00087014
       77 WRK-ALT-TELEFONE  PIC X(11).                                  00087114
                                                                        00087214
       01 WRK-ID.                                                       00087314
          02 FILLER         PIC X(10).                                  00087414
          02 WRK-ID-AC      PIC 9(05).                                  00087514
                                                                        00087614
                                                                        00087714
       01 WRK-NOME.                                                     00087814
          02 FILLER         PIC X(10).                                  00087914
          02 WRK-NOME-AC    PIC X(30).                                  00088014
                                                                        00088114
       01 WRK-SETOR.                                                    00088214
          02 FILLER         PIC X(10).                                  00088314
          02 WRK-SETOR-AC   PIC X(04).                                  00088414
                                                                        00088514
       01 WRK-SALARIO.                                                  00088614
          02 FILLER         PIC X(10).                                  00088714
          02 WRK-SALARIO-AC PIC 9(08)V9(02).                            00088814
                                                                        00088914
       01 WRK-DATAADM.                                                  00089014
          02 FILLER         PIC X(10).                                  00089114
          02 WRK-DATAADM-AC PIC X(10).                                  00089214
                                                                        00089314
       01 WRK-EMAIL.                                                    00089414
          02 FILLER         PIC X(10).                                  00089514
          02 WRK-EMAIL-AC   PIC X(40).                                  00089614
                                                                        00089714
                                                                        00089814
       01 WRK-TELEFONE.                                                 00089914
          02 FILLER           PIC X(10).                                00090014
          02 WRK-TELEFONE-AC  PIC X(11).                                00090114
                                                                        00090214
      *-------------------------------------------------------*         00090314
                                                                        00090414
                                                                        00090514
      *=======================================================*         00090614
       PROCEDURE                                  DIVISION.             00090714
      *=======================================================*         00090814
                                                                        00090914
                                                                        00091014
      *-------------------------------------------------------*         00091114
                                                                        00091214
      *         R O T I N A  P R I N C I P A L                *         00091314
                                                                        00091414
      *-------------------------------------------------------*         00091514
                                                                        00091614
                                                                        00091714
      *-------------------------------------------------------*         00091814
       0000-PRINCIPAL                              SECTION.             00091914
      *-------------------------------------------------------*         00092014
                                                                        00092114
            PERFORM 1000-INICIAR.                                       00092214
            PERFORM 2000-PROCESSAR.                                     00092314
            PERFORM 3000-FINALIZAR.                                     00092414
                                                                        00092514
      *-------------------------------------------------------*         00092614
       0000-999-FIM.                               EXIT.                00092714
      *-------------------------------------------------------*         00092814
                                                                        00092914
                                                                        00093014
      *-------------------------------------------------------*         00093114
       1000-INICIAR                               SECTION.              00093214
      *-------------------------------------------------------*         00093314
                                                                        00093414
              ACCEPT WRK-ID-AC.                                         00093514
              ACCEPT WRK-NOME-AC.                                       00093614
              ACCEPT WRK-SETOR-AC                                       00093714
              ACCEPT WRK-SALARIO-AC.                                    00093814
              ACCEPT WRK-DATAADM-AC.                                    00093914
              ACCEPT WRK-EMAIL-AC.                                      00094014
              ACCEPT WRK-TELEFONE-AC.                                   00094114
                MOVE WRK-ID-AC     TO DB2-ID.                           00094214
                                                                        00094314
                                                                        00094414
           EXEC SQL                                                     00094514
             SELECT ID, NOME, SETOR, SALARIO, DATAADM, EMAIL, TELEFONE  00094614
              INTO :REG-FUNC2                                           00094714
               FROM FOUR001.FUNC2                                       00094814
               WHERE ID = :DB2-ID                                       00094914
           END-EXEC.                                                    00095014
                                                                        00095114
                                                                        00095214
              PERFORM 4000-DADOS-ATUAIS.                                00095314
                                                                        00095414
      *-------------------------------------------------------*         00095514
       1000-999-FIM.                              EXIT.                 00095614
      *-------------------------------------------------------*         00095714
                                                                        00095814
                                                                        00095914
      *-------------------------------------------------------*         00096014
       2000-PROCESSAR                             SECTION.              00096114
      *-------------------------------------------------------*         00096214
                                                                        00097000
               IF WRK-NOME-AC      NOT EQUAL                            00098000
                  DB2-NOME         AND                                  00098100
                  WRK-NOME-AC      NOT EQUAL SPACES                     00098200
                  MOVE WRK-NOME-AC TO DB2-NOME                          00098300
                  MOVE WRK-NOME-AC TO WRK-ALT-NOME                      00098414
               END-IF                                                   00098600
                                                                        00098700
               IF WRK-SETOR-AC     NOT EQUAL                            00098800
                  DB2-SETOR        AND                                  00098900
                  WRK-SETOR-AC     NOT EQUAL SPACES                     00099000
                   MOVE WRK-SETOR-AC TO DB2-SETOR                       00099100
                   MOVE WRK-SETOR-AC TO WRK-ALT-SETOR                   00099214
               END-IF                                                   00099400
                                                                        00099500
               IF WRK-SALARIO-AC   NOT EQUAL                            00099600
                  DB2-SALARIO      AND                                  00099700
                  WRK-SALARIO-AC   NOT EQUAL ZEROS                      00099800
                  MOVE WRK-SALARIO-AC TO DB2-SALARIO                    00099900
                  MOVE WRK-SALARIO-AC TO WRK-ALT-SALARIO                00100014
                                                                        00100114
               END-IF                                                   00100200
                                                                        00100300
               IF WRK-DATAADM-AC   NOT EQUAL                            00100400
                  DB2-DATAADM      AND                                  00100500
                  WRK-DATAADM-AC   NOT EQUAL SPACES                     00100600
                  MOVE WRK-DATAADM-AC TO DB2-DATAADM                    00100700
                  MOVE WRK-DATAADM-AC TO WRK-ALT-DATAADM                00100814
               END-IF                                                   00100900
                                                                        00101000
               IF WRK-EMAIL-AC     NOT EQUAL                            00101100
                  DB2-EMAIL-TEXT   OR                                   00101210
                  WRK-EMAIL-AC     NOT EQUAL SPACES                     00101300
                  MOVE WRK-EMAIL-AC   TO DB2-EMAIL-TEXT                 00101404
                  MOVE WRK-EMAIL-AC   TO WRK-ALT-EMAIL                  00101514
               END-IF                                                   00101600
                                                                        00101700
               IF WRK-TELEFONE-AC  NOT EQUAL                            00101809
                  DB2-TELEFONE     AND                                  00101909
                  WRK-TELEFONE-AC  NOT EQUAL SPACES                     00102009
                  MOVE WRK-TELEFONE-AC TO DB2-TELEFONE                  00102109
                  MOVE WRK-TELEFONE-AC TO WRK-ALT-TELEFONE              00102214
               END-IF                                                   00102309
           EXEC SQL                                                     00102400
             UPDATE FOUR001.FUNC2                                       00102500
               SET  NOME     =:DB2-NOME,                                00102600
                    SETOR    =:DB2-SETOR,                               00102700
                    SALARIO  =:DB2-SALARIO,                             00102800
                    DATAADM  =:DB2-DATAADM,                             00102900
                    EMAIL    =:DB2-EMAIL,                               00103003
                    TELEFONE =:DB2-TELEFONE                             00103102
                    WHERE  ID=:DB2-ID                                   00103202
           END-EXEC.                                                    00103302
                                                                        00103402
            EVALUATE SQLCODE                                            00103502
              WHEN 0                                                    00104002
               DISPLAY 'DADOS ALTERADOS COM SUCESSO'                    00104414
               DISPLAY 'ID.........'  DB2-ID                            00104507
               DISPLAY 'NOME.......'  DB2-NOME                          00104607
               DISPLAY 'SETOR......'  DB2-SETOR                         00104707
               DISPLAY 'SALARIO....'  DB2-SALARIO                       00104807
               DISPLAY 'DATAADM....'  DB2-DATAADM                       00104907
               DISPLAY 'EMAIL......'  DB2-EMAIL-TEXT                    00105007
               DISPLAY 'TELEFONE...'  DB2-TELEFONE                      00105107
              WHEN 100                                                  00105205
               DISPLAY 'FIM DE PROCESSAMENTO'                           00105305
              WHEN OTHER                                                00105405
               DISPLAY 'ERRO DE PROCESSAMENTO '                         00105505
            END-EVALUATE.                                               00105605
                                                                        00105714
            PERFORM 6000-DADOS-ALTERADOS.                               00105815
                                                                        00106005
      *-------------------------------------------------------*         00107005
       2000-999-FIM.                              EXIT.                 00107105
      *-------------------------------------------------------*         00107205
                                                                        00107305
                                                                        00107405
      *-------------------------------------------------------*         00107505
       4000-DADOS-ATUAIS                          SECTION.              00107605
      *-------------------------------------------------------*         00107705
                                                                        00107805
                DISPLAY '----------------------------------'.           00107905
                DISPLAY '--------- ATUALIZAR DADOS --------'.           00108005
                DISPLAY 'ID.........'     DB2-ID.                       00108105
                DISPLAY 'NOME.......'     DB2-NOME.                     00108205
                DISPLAY 'SETOR......'     DB2-SETOR.                    00108305
                DISPLAY 'SALARIO....'     DB2-SALARIO.                  00108405
                DISPLAY 'DATAADMID..'     DB2-DATAADM                   00108505
                DISPLAY 'EMAIL......'     DB2-EMAIL-TEXT.               00108605
                DISPLAY 'TELEFONE...'     DB2-TELEFONE.                 00108705
                DISPLAY '----------------------------------'.           00108805
                DISPLAY '----------------------------------'.           00108905
                                                                        00109005
      *-------------------------------------------------------*         00109105
       4000-999-FIM.                              EXIT.                 00109205
      *-------------------------------------------------------*         00109305
                                                                        00109405
      *-------------------------------------------------------*         00109514
       6000-DADOS-ALTERADOS                       SECTION.              00109614
      *-------------------------------------------------------*         00109714
                                                                        00109816
           DISPLAY '-------------------------'                          00109916
           DISPLAY '-------ALTERADO---------------'                     00110016
           DISPLAY 'NOME------> ' WRK-ALT-NOME                          00110116
           DISPLAY 'SETOR-----> ' WRK-ALT-SETOR                         00110216
           DISPLAY 'SALARIO---> ' WRK-ALT-SALARIO                       00110316
           DISPLAY 'DATA------> ' WRK-ALT-DATAADM                       00110416
           DISPLAY 'EMAIL-----> ' WRK-ALT-EMAIL                         00110516
           DISPLAY 'TELEFONE--> ' WRK-ALT-TELEFONE                      00110616
           DISPLAY '------------------------------'.                    00111216
                                                                        00111314
      *-------------------------------------------------------*         00111414
       6000-999-FIM.                              EXIT.                 00111514
      *-------------------------------------------------------*         00111614
                                                                        00111714
                                                                        00111814
      *-------------------------------------------------------*         00111914
       3000-FINALIZAR                             SECTION.              00112014
      *-------------------------------------------------------*         00112114
                                                                        00112214
              STOP RUN.                                                 00112314
                                                                        00112414
      *-------------------------------------------------------*         00112514
       3000-999-FIM.                              EXIT.                 00112614
      *-------------------------------------------------------*         00113005
                                                                        00120002
