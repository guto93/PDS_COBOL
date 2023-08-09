      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR11ARQ01.                                           00040005
                                                                        00050000
      *=====================================================            00060000
      *  AUTOR: GUSTAVO                                                 00070005
      *  EMPRESA: FOURSYS                                               00080000
      *=====================================================            00090000
                                                                        00100000
      *====================================================             00101000
       ENVIRONMENT                               DIVISION.              00102000
      *====================================================             00103000
       CONFIGURATION                             SECTION.               00103101
       SPECIAL-NAMES.                                                   00103201
           DECIMAL-POINT  IS COMMA.                                     00103302
                                                                        00103402
       INPUT-OUTPUT                              SECTION.               00104002
       FILE-CONTROL.                                                    00104102
           SELECT CLIENTES  ASSIGN TO CLIENTES                          00104202
               FILE STATUS  IS WRK-FS-CLIENTES.                         00104302
                                                                        00104402
      *====================================================             00105000
       DATA                                      DIVISION.              00106000
      *====================================================             00107000
       FILE                                      SECTION.               00107102
       FD CLIENTES                                                      00107202
           RECORDING  MODE IS F                                         00107302
           BLOCK CONTAINS 0 RECORDS.                                    00107402
                                                                        00107502
       01 FD-CLIENTES.                                                  00107602
          05 FD-CODIGO PIC X(04).                                       00107702
          05 FD-NOME   PIC X(20).                                       00107802
          05 FD-BANCO  PIC X(10).                                       00107904
                                                                        00108003
       WORKING-STORAGE                           SECTION.               00109002
      *====================================================             00110200
                                                                        00110303
                                                                        00110403
       77 WRK-MSG-OPEN    PIC X(30) VALUE ' STATUS OPEN  '.             00110603
       77 WRK-MSG-CLOSE   PIC X(30) VALUE ' STATUS CLOSE '.             00110703
       77 WRK-MSG         PIC X(30) VALUE SPACES.                       00110803
                                                                        00110903
       77 WRK-FS-CLIENTES PIC 9(02).                                    00111003
                                                                        00111103
       PROCEDURE                                 DIVISION.              00111203
      *====================================================             00111303
       0000-PRINCIPAL                            SECTION.               00111403
           PERFORM 1000-INICIAR.                                        00111503
           PERFORM 2000-PROCESSAR UNTIL WRK-FS-CLIENTES EQUAL 10.       00111604
           PERFORM 3000-FINALIZAR.                                      00111703
            STOP RUN.                                                   00111803
                                                                        00111903
                                                                        00112003
       1000-INICIAR                              SECTION.               00112103
           OPEN INPUT CLIENTES.                                         00112203
            MOVE WRK-MSG-OPEN   TO WRK-MSG.                             00112303
            PERFORM 4000-TESTAR-STATUS.                                 00112403
             EVALUATE WRK-FS-CLIENTES                                   00112504
              WHEN 00                                                   00112604
      *---------- ARQUIVO EXISTE TESTAR SE TEM 1 REGISTRO PELO MENOS    00112704
                 PERFORM 5000-TESTAR-VAZIO                              00112804
              WHEN 35                                                   00112904
      *--------- QUANDO O ARQUIVO NAO EXISTE STOPA O PROGRAMA ---       00113003
                 DISPLAY 'ARQUIVO NAO EXISTE'                           00113103
                 STOP RUN                                               00113203
              WHEN OTHER                                                00113704
                  DISPLAY 'ERRO NO OPEN ' WRK-FS-CLIENTES               00113804
                   STOP RUN                                             00113904
             END-EVALUATE.                                              00114004
                                                                        00114104
       2000-PROCESSAR                            SECTION.               00114204
              DISPLAY 'REGISTRO. ' FD-CLIENTES.                         00114304
               PERFORM 5000-TESTAR-VAZIO.                               00114404
       3000-FINALIZAR                            SECTION.               00114504
           CLOSE CLIENTES.                                              00114604
            MOVE WRK-MSG-CLOSE   TO WRK-MSG.                            00114704
            PERFORM 4000-TESTAR-STATUS.                                 00114804
              DISPLAY '------- FIM DE PROCESSAMENTO'.                   00114904
                                                                        00115004
                                                                        00115104
       4000-TESTAR-STATUS                    SECTION.                   00115204
              DISPLAY '-----------------------------'.                  00115304
              DISPLAY WRK-MSG   ' - ' WRK-FS-CLIENTES.                  00115404
              DISPLAY '-----------------------------'.                  00115504
                                                                        00115604
       5000-TESTAR-VAZIO                     SECTION.                   00115704
            READ CLIENTES.                                              00115804
                IF WRK-FS-CLIENTES  EQUAL 10                            00115904
                     DISPLAY ' FINAL DE ARQUIVO '                       00116004
      *               STOP RUN                                          00116104
                END-IF.                                                 00117004
