      *=========================================*                       00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=========================================*                       00003000
                                                                        00004000
       PROGRAM-ID.  ARQ004.                                             00005000
                                                                        00006000
      *=======================================================*         00007000
      *     AUTOR   : AUGUSTO MARTINS                                   00008000
      *     EMPRESA : FOURSYS                                           00009000
      *-------------------------------------------------------*         00010000
      *     DATA   : 03/05/2022                                         00020000
      *     OBJETIVO: GRAVAR DADOS NO ARQUIVO DE SAIDA RESUM.           00021000
      *=======================================================*         00022000
                                                                        00023000
      *=======================================================*         00024000
       ENVIRONMENT                               DIVISION.              00025000
      *=======================================================*         00026000
       INPUT-OUTPUT                              SECTION.               00026100
       FILE-CONTROL.                                                    00026200
             SELECT PRODUCT  ASSIGN TO JCLARQ02                         00026300
                FILE STATUS  IS WRK-FS-PRODUCT.                         00026400
             SELECT RESUM   ASSIGN TO RESUM                             00026600
                FILE STATUS  IS WRK-FS-RESUM.                           00026700
      *=======================================================*         00026800
       DATA                                      DIVISION.              00026900
      *=======================================================*         00027000
       FILE                                      SECTION.               00027100
       FD PRODUCT                                                       00027200
           RECORDING MODE IS F                                          00027300
           BLOCK CONTAINS 0 RECORDS.                                    00028000
       01 FD-PRODUCT PIC X(70).                                         00028200
                                                                        00028300
       FD RESUM                                                         00028400
           RECORDING MODE IS F                                          00028500
           BLOCK CONTAINS 0 RECORDS.                                    00028600
       01 FD-RESUM   PIC X(40).                                         00028700
      *---------------------------------------------------              00028800
       WORKING-STORAGE                           SECTION.               00028900
      *---------------------------------------------------              00029000
       77 WRK-FS-RESUM    PIC X(02) VALUE SPACES.                       00029100
       77 WRK-FS-PRODUCT  PIC X(02) VALUE SPACES.                       00029200
       77 WRK-MSG         PIC X(50) VALUE SPACES.                       00029300
       77 WRK-DADOS       PIC X(40) VALUE SPACES.                       00029400
      *=========================================*                       00029500
       PROCEDURE DIVISION.                                              00029600
      *=========================================*                       00030000
                                                                        00030100
      *-------------------------------------------------------------*   00030200
       0000-PRINCIPAL                     SECTION.                      00030300
      *-------------------------------------------------------------*   00030400
                                                                        00030500
           PERFORM 1000-INICIAR.                                        00030600
           PERFORM 2000-PROCESSAR UNTIL WRK-FS-PRODUCT EQUAL '10'       00030800
           PERFORM 3000-FINALIZAR.                                      00031000
                                                                        00031100
                                                                        00031200
      *-------------------------------------------------------------*   00031300
       0000-99-FIM.                       EXIT.                         00031400
      *-------------------------------------------------------------*   00031500
                                                                        00031600
                                                                        00031700
      *-------------------------------------------------------------*   00031800
       1000-INICIAR                       SECTION.                      00031900
      *-------------------------------------------------------------*   00032000
                                                                        00032100
            OPEN INPUT  PRODUCT                                         00032200
                 OUTPUT RESUM.                                          00032300
              PERFORM 1100-TESTAR-FILE-STATUS                           00032400
              READ PRODUCT.                                             00032500
                                                                        00032600
      *-------------------------------------------------------------*   00032700
       1000-99-FIM.                       EXIT.                         00032800
      *-------------------------------------------------------------*   00032900
                                                                        00033000
                                                                        00033100
                                                                        00033200
      *-------------------------------------------------------------*   00033300
       1100-TESTAR-FILE-STATUS            SECTION.                      00033400
      *-------------------------------------------------------------*   00033500
                                                                        00033600
                IF WRK-FS-PRODUCT  NOT EQUAL ZEROS                      00033700
                   MOVE ' ERRO OPEN '   TO WRK-MSG                      00033800
                     PERFORM 9000-TRATAR-ERROS                          00033900
                END-IF.                                                 00034000
                                                                        00034100
                IF WRK-FS-RESUM    NOT EQUAL ZEROS                      00034200
                   MOVE ' ERRO OPEN '   TO WRK-MSG                      00034300
                     PERFORM 9000-TRATAR-ERROS                          00034400
                END-IF.                                                 00034500
                                                                        00034600
      *-------------------------------------------------------------*   00034700
       1100-99-FIM.                       EXIT.                         00034800
      *-------------------------------------------------------------*   00034900
                                                                        00035000
                                                                        00035100
      *-------------------------------------------------------------*   00035200
       2000-PROCESSAR                     SECTION.                      00035300
      *-------------------------------------------------------------*   00035400
                                                                        00035500
               IF WRK-FS-PRODUCT  EQUAL ZEROS                           00035900
                   MOVE  FD-PRODUCT(1:40)  TO FD-RESUM                  00036000
                   WRITE FD-RESUM                                       00036100
                ELSE                                                    00036200
                     MOVE 'FINAL DE ARQUIVO ' TO WRK-MSG                00036300
                END-IF                                                  00036400
                                                                        00036500
                READ PRODUCT.                                           00036600
                                                                        00036700
      *-------------------------------------------------------------*   00036800
       2000-99-FIM.                       EXIT.                         00036900
      *-------------------------------------------------------------*   00037000
                                                                        00037100
                                                                        00037200
      *-------------------------------------------------------------*   00037300
       3000-FINALIZAR                     SECTION.                      00037400
      *-------------------------------------------------------------*   00037500
                                                                        00037600
            CLOSE PRODUCT                                               00037700
            CLOSE RESUM                                                 00037800
                                                                        00037900
                 PERFORM 9000-TRATAR-ERROS.                             00038000
                 MOVE ' FINAL DO PROCESSO ' TO WRK-MSG.                 00038100
                                                                        00038300
      *-------------------------------------------------------------*   00038400
       3000-99-FIM.                       EXIT.                         00038500
      *-------------------------------------------------------------*   00038600
                                                                        00038700
                                                                        00038800
                                                                        00038900
      *-------------------------------------------------------------*   00039000
       9000-TRATAR-ERROS                  SECTION.                      00039100
      *-------------------------------------------------------------*   00039200
                                                                        00039300
             DISPLAY '------------------'                               00039400
             DISPLAY   WRK-MSG                                          00039500
             DISPLAY '------------------'                               00039600
                STOP RUN.                                               00039700
                                                                        00039800
      *-------------------------------------------------------------*   00039900
       9000-99-FIM.            EXIT.                                    00040000
      *-------------------------------------------------------------*   00041000
                                                                        00050000
