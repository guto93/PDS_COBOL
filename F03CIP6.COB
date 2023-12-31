      *=======================================================*         00010000
       IDENTIFICATION                            DIVISION.              00011000
      *=======================================================*         00012000
                                                                        00013000
       PROGRAM-ID. F03CIP2.                                             00014000
                                                                        00015000
      *=======================================================*         00016000
      *             T  R  E  I  N  A  M  E  N  T  O                     00017000
      *=======================================================*         00018000
      *     PROGRAMA......: FR03DB10                                    00019000
      *     TIPO..........: SERVICO DE ACESSO A DADOS                   00020000
      *-------------------------------------------------------*         00021000
      *    PROGRAMADOR(A): AUGUSTO MARTINS  - TREINAMENTO               00021100
      *    ANALISTA......: IVAN PETRUCCI    - TREINAMENTO               00021200
      *    DATA..........: 21/06/2022                                   00021300
      *-------------------------------------------------------*         00021400
      *-------------------------------------------------------*         00021500
      *    OBJETIVO..: GRAVAR DADOS INSERIDOS DO CICS,PARA              00021600
      *                TABELA FOUR001.FUNC.                             00021701
      *-------------------------------------------------------*         00021800
      *=======================================================*         00021900
       DATA                                       DIVISION.             00022000
      *=======================================================*         00022100
                                                                        00022200
      *-------------------------------------------------------*         00022300
       WORKING-STORAGE                            SECTION.              00022400
      *-------------------------------------------------------*         00022500
                                                                        00022600
                                                                        00022700
      *-------------------------------------------------------*         00022800
       01 FILLER                          PIC X(050)   VALUE            00022900
                 '*******AREA DA  BOOK   ******'.                       00023000
      *-------------------------------------------------------*         00023100
                                                                        00023200
            COPY F03CIM1.                                               00023300
                                                                        00023400
      *-------------------------------------------------------*         00023500
       01 FILLER                          PIC X(050)   VALUE            00023600
                 '*******AREA DB2  ********'.                           00023700
      *-------------------------------------------------------*         00023800
                                                                        00023900
                                                                        00024000
           EXEC SQL                                                     00024100
             INCLUDE BOOKFUNC                                           00024200
           END-EXEC.                                                    00024300
                                                                        00024400
           EXEC SQL                                                     00024500
             INCLUDE SQLCA                                              00024600
           END-EXEC.                                                    00024700
                                                                        00024800
      *-------------------------------------------------------*         00024900
       01 FILLER                          PIC X(050)   VALUE            00025000
                '*******AREA DE AUXILIARES********'.                    00025100
      *-------------------------------------------------------*         00025200
                                                                        00025300
       77 WRK-SAIDA       PIC X(30)  VALUE SPACES.                      00025400
       77 WRK-IDS         PIC 9(05)  VALUE ZEROS.                       00025500
       77 WRK-EMAIL-NULL  PIC S9(04) COMP.                              00025600
       77 WRK-SALARIO     PIC 9(10).                                    00025700
       77 WRK-SQLCODE     PIC -999.                                     00025800
                                                                        00025900
      *=======================================================*         00026000
       PROCEDURE                                  DIVISION.             00026100
      *=======================================================*         00026200
                                                                        00026300
                                                                        00026400
      *-------------------------------------------------------*         00026500
                                                                        00026600
      *         R O T I N A  P R I N C I P A L *                        00026700
                                                                        00026800
      *-------------------------------------------------------*         00026900
                                                                        00027000
      *-------------------------------------------------------*         00028000
       0000-PRINCIPAL                              SECTION.             00029000
      *-------------------------------------------------------*         00030000
                                                                        00040000
                  PERFORM 1000-INICIAR-CICS                             00050000
                  PERFORM 2000-PROCESSAR                                00051000
                  PERFORM 3000-FINALIZAR-CICS.                          00051100
                                                                        00051200
      *-------------------------------------------------------*         00051300
       0000-999-FIM.                               EXIT.                00051400
      *-------------------------------------------------------*         00051500
                                                                        00051600
                                                                        00051700
      *-------------------------------------------------------*         00051800
       1000-INICIAR-CICS                          SECTION.              00051900
      *-------------------------------------------------------*         00052000
                                                                        00052100
            EXEC CICS SEND                                              00052200
              MAPSET('F03CIM1')                                         00052300
              MAP('MAPTEST')                                            00052400
              ERASE                                                     00052500
              MAPONLY                                                   00052600
            END-EXEC.                                                   00052700
                                                                        00052800
                                                                        00052900
      *-------------------------------------------------------*         00053000
       1000-999-FIM.                              EXIT.                 00054000
      *-------------------------------------------------------*         00055000
                                                                        00056000
                                                                        00057000
      *-------------------------------------------------------*         00058000
       2000-PROCESSAR                             SECTION.              00059000
      *-------------------------------------------------------*         00060000
                                                                        00070000
            EXEC CICS RECEIVE                                           00080000
              MAPSET('F03CIM1')                                         00090000
              MAP('MAPTEST')                                            00091000
              INTO(MAPTESTI)                                            00092000
            END-EXEC.                                                   00093000
                                                                        00094000
           IF EIBAID = '6'                                              00095000
               MOVE IDI                    TO DB2-ID                    00096000
                                                                        00097000
            EXEC SQL                                                    00098000
              SELECT ID,NOME,SETOR,SALARIO,DATAADM,EMAIL                00099000
                INTO :DB2-ID,                                           00100000
                     :DB2-NOME,                                         00110000
                     :DB2-SETOR,                                        00120000
                     :DB2-SALARIO,                                      00121000
                     :DB2-DATAADM,                                      00122000
                     :DB2-EMAIL :WRK-EMAIL-NULL                         00123000
                 FROM FOUR001.FUNC                                      00124000
                 WHERE ID =:DB2-ID                                      00125000
            END-EXEC                                                    00126000
                                                                        00126100
                EVALUATE SQLCODE                                        00126200
                 WHEN 0                                                 00126300
                     MOVE DB2-ID                TO IDO                  00126400
                     MOVE DB2-NOME              TO NOMEO                00126500
                     MOVE DB2-SETOR             TO SETORO               00126600
                     MOVE DB2-SALARIO           TO WRK-SALARIO          00126700
                     MOVE WRK-SALARIO           TO SALO                 00126800
                     MOVE DB2-DATAADM           TO DATADMO              00126900
                                                                        00127000
                  IF WRK-EMAIL-NULL EQUAL -1                            00127100
                     MOVE SPACES            TO EMAILO                   00127200
                  ELSE                                                  00127300
                     MOVE DB2-EMAIL         TO EMAILO                   00127400
                  END-IF                                                00127500
                                                                        00127600
                    MOVE ' ENCONTRADO '       TO MSGO                   00127700
                WHEN 100                                                00127800
                  INITIALIZE MAPTESTO                                   00127900
                    MOVE ' NAO ENCONTRADO ' TO MSGO                     00128000
                WHEN OTHER                                              00128100
                  MOVE 'ERRO.....:'          TO MSGO                    00128200
                  MOVE WRK-SQLCODE           TO MSGO(11:04)             00128300
               END-EVALUATE                                             00128400
              END-IF.                                                   00128500
                                                                        00128600
             IF EIBAID = '5'                                            00128700
                   MOVE IDI             TO DB2-ID                       00128800
                   MOVE NOMEI           TO DB2-NOME                     00128900
                   MOVE SETORI          TO DB2-SETOR                    00129000
                   MOVE SALI            TO DB2-SALARIO                  00129100
                   MOVE WRK-SALARIO     TO DB2-SALARIO                  00129200
                   MOVE DATADMI         TO DB2-DATAADM                  00129300
                   MOVE EMAILI          TO DB2-EMAIL                    00129400
                                                                        00129500
                 EXEC SQL                                               00129600
                   INSERT INTO FOUR001.FUNC(ID,NOME,SETOR,SALARIO,      00129700
                                            DATAADM,EMAIL)              00129800
                   VALUES(:DB2-ID,                                      00129900
                          :DB2-NOME,                                    00130000
                          :DB2-SETOR,                                   00130100
                          :DB2-SALARIO,                                 00130200
                          :DB2-DATAADM,                                 00130300
                          :DB2-EMAIL )                                  00130400
                   END-EXEC                                             00130500
                                                                        00130600
                  EVALUATE SQLCODE                                      00130700
                    WHEN 0                                              00130800
                      EXEC SQL                                          00130900
                        COMMIT                                          00131000
                     END-EXEC                                           00131100
                          MOVE '-   INSERIDO -  ' TO MSGO               00131200
                     WHEN OTHER                                         00131300
                           MOVE SQLCODE TO WRK-SQLCODE                  00131400
                           MOVE 'ERRO ..: ' TO MSGO                     00131500
                           MOVE WRK-SQLCODE TO MSGO(11:04)              00131600
                       END-EVALUATE                                     00131700
                                                                        00131800
                 END-IF.                                                00131900
                                                                        00132000
                                                                        00132100
                                                                        00132200
            IF EIBAID = '3'                                             00132300
               EXEC CICS                                                00132400
                 RETURN                                                 00132500
               END-EXEC                                                 00132600
            END-IF.                                                     00132700
                                                                        00132800
                                                                        00132900
            EXEC CICS SEND                                              00133000
              MAPSET('F03CIM1')                                         00133100
              MAP('MAPTEST')                                            00133200
              DATAONLY                                                  00133300
            END-EXEC.                                                   00133400
                                                                        00133500
      *-------------------------------------------------------*         00133600
       2000-999-FIM.                              EXIT.                 00133700
      *-------------------------------------------------------*         00133800
                                                                        00133900
                                                                        00134000
      *-------------------------------------------------------*         00134100
       3000-FINALIZAR-CICS                        SECTION.              00134200
      *-------------------------------------------------------*         00134300
                                                                        00134400
            EXEC CICS                                                   00134500
              RETURN TRANSID('T032')                                    00134600
            END-EXEC.                                                   00134700
                                                                        00134800
             MOVE 'FINALIZAR ' TO WRK-SAIDA.                            00134900
                                                                        00135000
      *-------------------------------------------------------*         00136000
       3000-999-FIM.                              EXIT.                 00137000
      *-------------------------------------------------------*         00138000
                                                                        00139000
