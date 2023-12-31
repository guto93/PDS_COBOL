                                                                        00001000
      *=======================================================*         00002000
       IDENTIFICATION                            DIVISION.              00003000
      *=======================================================*         00004000
                                                                        00005000
       PROGRAM-ID.  FR03DB03.                                           00006000
                                                                        00007000
      *=======================================================*         00008000
      *              TREINAMENTO MAINFRAME                    *         00009000
      *=======================================================*         00010000
      *     AUTOR    : AUGUSTO MARTINS                                  00020000
      *     EMPRESA  : FOURSYS                                          00030000
      *     DATA     : 03/06/2022                                       00040000
      *     OBJETIVO : INSERIR DADOS DA SYSIN,COM O FORMATO DOS         00050000
      *              : DADOS DA TABELA FOUR001.FUNC                     00060000
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
                INCLUDE BOOKFUNC                                        00083500
           END-EXEC.                                                    00083600
           EXEC SQL                                                     00083700
                INCLUDE SQLCA                                           00083800
           END-EXEC.                                                    00083900
                                                                        00084000
      *-------------------------------------------------------*         00084700
        01 FILLER                          PIC X(050)   VALUE           00084800
                '*******AREA DE AUXILIARES********'.                    00084901
      *-------------------------------------------------------*         00085000
                                                                        00085100
      *-------------------------------------------------------*         00085700
                                                                        00085800
                                                                        00085900
      *-------------------------------------------------------*         00086000
        01 FILLER                          PIC X(050)   VALUE           00086100
                '*******AREA DE AUXILIARES********'.                    00086200
      *-------------------------------------------------------*         00086300
                                                                        00086400
       77 WRK-SQLCODE       PIC -999.                                   00086601
       77 WRK-INDICATOR     PIC S9(04) COMP VALUE ZEROS.                00086701
                                                                        00086901
       01 WRK-ID.                                                       00087001
          02 FILLER         PIC X(10).                                  00087101
          02 WRK-ID-AC      PIC 9(04).                                  00087201
                                                                        00087301
       01 WRK-NOME.                                                     00087401
          02 FILLER         PIC X(10).                                  00087501
          02 WRK-NOME-AC    PIC X(30).                                  00087601
                                                                        00087701
       01 WRK-SETOR.                                                    00087801
          02 FILLER         PIC X(10).                                  00087901
          02 WRK-SETOR-AC   PIC X(04).                                  00088001
                                                                        00088101
       01 WRK-SALARIO.                                                  00088202
          02 FILLER         PIC X(10).                                  00088301
          02 WRK-SALARIO-AC PIC 9(08)V99.                               00088401
                                                                        00088501
       01 WRK-DATAADM.                                                  00088602
          02 FILLER         PIC X(10).                                  00088701
          02 WRK-DATAADM-AC PIC X(10).                                  00088801
                                                                        00088901
       01 WRK-EMAIL.                                                    00089001
          02 FILLER         PIC X(10).                                  00089101
          02 WRK-EMAIL-AC   PIC X(40).                                  00089201
                                                                        00089301
                                                                        00089701
      *-------------------------------------------------------*         00089801
                                                                        00089901
                                                                        00090001
      *=======================================================*         00090101
       PROCEDURE                                  DIVISION.             00090201
      *=======================================================*         00090301
                                                                        00090401
                                                                        00090501
      *-------------------------------------------------------*         00090601
                                                                        00090701
      *         R O T I N A  P R I N C I P A L *                        00090801
                                                                        00090901
      *-------------------------------------------------------*         00091001
                                                                        00091101
                                                                        00091201
      *-------------------------------------------------------*         00091301
       0000-PRINCIPAL                              SECTION.             00091401
      *-------------------------------------------------------*         00091501
                                                                        00091601
            PERFORM 1000-INICIAR.                                       00091701
            PERFORM 2000-PROCESSAR.                                     00091804
            PERFORM 3000-FINALIZAR.                                     00091901
                                                                        00092101
      *-------------------------------------------------------*         00092201
       0000-999-FIM.                               EXIT.                00092301
      *-------------------------------------------------------*         00092401
                                                                        00092501
                                                                        00092601
      *-------------------------------------------------------*         00092701
       1000-INICIAR                               SECTION.              00092801
      *-------------------------------------------------------*         00092901
                                                                        00093001
                                                                        00093101
             ACCEPT WRK-ID.                                             00093201
             ACCEPT WRK-NOME.                                           00093301
             ACCEPT WRK-SETOR.                                          00093401
             ACCEPT WRK-SALARIO.                                        00093501
             ACCEPT WRK-DATAADM.                                        00093601
             ACCEPT WRK-EMAIL.                                          00093701
                                                                        00094001
                                                                        00094101
      *-------------------------------------------------------*         00094401
       1000-999-FIM.                              EXIT.                 00094501
      *-------------------------------------------------------*         00094601
                                                                        00094701
                                                                        00094801
      *-------------------------------------------------------*         00094901
       2000-PROCESSAR                             SECTION.              00095001
      *-------------------------------------------------------*         00095101
                                                                        00095401
            MOVE    WRK-ID-AC            TO DB2-ID.                     00095518
            MOVE    WRK-NOME-AC          TO DB2-NOME.                   00095618
            MOVE    WRK-SETOR-AC         TO DB2-SETOR.                  00095718
            MOVE    WRK-SALARIO-AC       TO DB2-SALARIO.                00095818
            MOVE    WRK-DATAADM-AC       TO DB2-DATAADM.                00095918
            MOVE    WRK-EMAIL-AC         TO DB2-EMAIL.                  00096818
                                                                        00096901
            EXEC SQL                                                    00097018
              INSERT INTO                                               00097118
              FOUR001.FUNC(ID,NOME,SETOR,SALARIO,DATAADM,EMAIL)         00097218
              VALUES(:DB2-ID,                                           00097318
                     :DB2-NOME,                                         00097418
                     :DB2-SETOR,                                        00097918
                     :DB2-SALARIO,                                      00098018
                     :DB2-DATAADM,                                      00098118
                     :DB2-EMAIL)                                        00098218
            END-EXEC.                                                   00098318
                                                                        00098418
                                                                        00098518
             EVALUATE SQLCODE                                           00098618
               WHEN 0                                                   00098718
                DISPLAY 'DADOS GRAVADOS'                                00098818
                DISPLAY 'ID......'  DB2-ID                              00098918
                DISPLAY 'NOME....'  DB2-NOME                            00099018
                DISPLAY 'SETOR...'  DB2-SETOR                           00099118
                DISPLAY 'SALARIO.'  DB2-SALARIO                         00099218
                DISPLAY 'DATAADM.'  DB2-DATAADM                         00099318
                DISPLAY 'EMAIL...'  DB2-EMAIL                           00099418
               WHEN -181                                                00099519
                DISPLAY 'ERRO COMFORMATO DA DATA ' WRK-DATAADM-AC       00099621
               WHEN OTHER                                               00099818
                MOVE SQLCODE        TO WRK-SQLCODE                      00099920
                DISPLAY 'ERRO....'  WRK-SQLCODE                         00100018
             END-EVALUATE.                                              00100118
                                                                        00100205
      *-------------------------------------------------------*         00100305
       2000-999-FIM.                              EXIT.                 00100405
      *-------------------------------------------------------*         00100505
                                                                        00101005
                                                                        00101701
      *-------------------------------------------------------*         00101801
       3000-FINALIZAR                             SECTION.              00101901
      *-------------------------------------------------------*         00102001
                                                                        00102101
              STOP RUN.                                                 00102921
                                                                        00103701
      *-------------------------------------------------------*         00103801
       3000-999-FIM.                              EXIT.                 00103901
      *-------------------------------------------------------*         00104001
                                                                        00105001
