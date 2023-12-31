      *===========================================================*     00001000
       IDENTIFICATION                             DIVISION.             00002000
      *===========================================================*     00003000
                                                                        00003100
       PROGRAM-ID.  FR03CB20.                                           00003200
                                                                        00003300
      *===========================================================*     00003400
      *              TREINAMENTO MAINFRAME                              00003500
      *===========================================================*     00003600
                                                                        00003700
      *===========================================================*     00003800
      *                                                                 00003900
      *   PROGRAMADOR: AUGUSTO MARTINS                                  00004000
      *   EMPRESA    : FOURSYS                                          00005000
      *   DATA       : 25/04/2022                                       00006000
      *   OBJETIVO   : VALIDAR OS DADOS INSERIDOS NA SYSIN, E USAR      00007000
      *              : UMA VARIAVEL LOGICA NIVEL 88.                    00008000
      *                                                                 00009000
      *===========================================================*     00010000
                                                                        00011000
                                                                        00012000
      *===========================================================*     00012100
       ENVIRONMENT                                DIVISION.             00012200
      *===========================================================*     00012300
                                                                        00012400
      *-----------------------------------------------------------*     00012500
       CONFIGURATION                              SECTION.              00012600
      *-----------------------------------------------------------*     00012700
                                                                        00012800
       SPECIAL-NAMES.                                                   00012900
           DECIMAL-POINT IS COMMA.                                      00013000
                                                                        00013100
      *===========================================================*     00013200
       DATA                                       DIVISION.             00013300
      *===========================================================*     00013400
                                                                        00013500
                                                                        00013600
      *-----------------------------------------------------------*     00013700
       WORKING-STORAGE                            SECTION.              00013800
      *-----------------------------------------------------------*     00013900
                                                                        00014000
                                                                        00015000
      *-----------------------------------------------------------*     00015100
      *---------    INICIO  DA WORKING - SECTION         ---------*     00015200
      *-----------------------------------------------------------*     00015300
                                                                        00015400
                                                                        00015500
      *-----------------------------------------------------------*     00015600
                                                                        00015700
       01 WRK-VALIDAR.                                                  00015800
           05 WRK-DATA                 PIC 9(08) VALUE ZEROS.           00015900
           05 WRK-LANCAMENTO           PIC X(30) VALUE SPACES.          00016000
           05 WRK-VALOR                PIC 9(05)V99 VALUE ZEROS.        00016100
           05 WRK-TIPO                 PIC X(01) VALUE SPACES.          00016200
              88 WRK-TIPO1                       VALUE 'C' 'D'.         00016319
                                                                        00016418
       01 WRK-MSG                      PIC A(20) VALUE SPACES.          00016702
      *-----------------------------------------------------------*     00016800
                                                                        00016900
                                                                        00017000
      *===========================================================*     00017100
       PROCEDURE                                  DIVISION.             00017200
      *===========================================================*     00017300
                                                                        00017400
      *************************************************************     00017500
      *                    ROTINA PRINCIPAL                       *     00017600
      *************************************************************     00017700
                                                                        00017800
      *-----------------------------------------------------------*     00017900
       0000-PRINCIPAL                               SECTION.            00018000
      *-----------------------------------------------------------*     00018100
                                                                        00018200
            PERFORM 1000-INICIAR.                                       00018304
            PERFORM 2000-PROCESSAR.                                     00018404
            PERFORM 9999-FIMARQ.                                        00019005
                                                                        00019100
      *-----------------------------------------------------------*     00019200
       0000-99-FIM.                                  EXIT.              00019300
      *-----------------------------------------------------------*     00019400
                                                                        00019500
      *************************************************************     00019600
      *                    PROCEDIMENTO INICIAL                   *     00019700
      *************************************************************     00019800
                                                                        00019900
      *-----------------------------------------------------------*     00020000
       1000-INICIAR                                  SECTION.           00020100
      *-----------------------------------------------------------*     00020200
                                                                        00020300
             ACCEPT WRK-VALIDAR   FROM SYSIN.                           00020400
                                                                        00020500
      *-----------------------------------------------------------*     00020600
       1000-99-FIM.                                   EXIT.             00020700
      *-----------------------------------------------------------*     00020800
                                                                        00020900
                                                                        00021000
      *************************************************************     00022000
      *                    PROCESSAR                              *     00023000
      *************************************************************     00024000
                                                                        00025000
                                                                        00025100
      *-----------------------------------------------------------*     00025200
       2000-PROCESSAR                                SECTION.           00025300
      *-----------------------------------------------------------*     00025400
                                                                        00025800
               IF  NOT WRK-TIPO1                                        00025922
                  PERFORM 9000-TRATAR-ERRO                              00026116
               ELSE                                                     00026215
                  MOVE ' PROCESSAR-ARQUVO'  TO WRK-MSG                  00026316
               END-IF.                                                  00026515
                                                                        00028000
               DISPLAY '-' WRK-MSG  WRK-TIPO.                           00029010
                                                                        00030300
                                                                        00030400
      *-----------------------------------------------------------*     00030500
       2000-99-FIM.                                  EXIT.              00030600
      *-----------------------------------------------------------*     00030700
                                                                        00030800
                                                                        00030900
      *-----------------------------------------------------------*     00031000
       9000-TRATAR-ERRO                             SECTION.            00031103
      *-----------------------------------------------------------*     00031200
                                                                        00031300
               DISPLAY 'ERRO NO PROCESSAMENTO DE ARQUIVOS'              00031400
               PERFORM 9999-FIMARQ.                                     00031500
                                                                        00031600
      *-----------------------------------------------------------*     00031700
       9000-99-FIM.                                  EXIT.              00031800
      *-----------------------------------------------------------*     00031900
                                                                        00032000
                                                                        00032100
      *-----------------------------------------------------------*     00032200
       9999-FIMARQ                                   SECTION.           00032300
      *-----------------------------------------------------------*     00032400
                                                                        00032500
              DISPLAY 'FIM DO PROCESSAMENTO'.                           00032600
              STOP RUN.                                                 00032700
                                                                        00032800
      *-----------------------------------------------------------*     00032900
       9999-99-FIM.                                   EXIT.             00033000
      *-----------------------------------------------------------*     00034000
