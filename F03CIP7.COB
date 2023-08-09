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
      *    DATA..........: 22/06/2022                                   00021300
      *-------------------------------------------------------*         00021400
      *-------------------------------------------------------*         00021500
      *    OBJETIVO..: FAZER UPDATE DE DE DADOS INSERIDOS NO CICS       00021600
      *                PARA A TABELA FOUR001.FUNC                       00021700
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
       77 WRK-NOME        PIC X(10) VALUE SPACES.                       00025900
       77 WRK-SETOR       PIC X(04) VALUE SPACES.                       00026000
       77 WRK-FLEG        PIC S9(04).                                   00026100
      *=======================================================*         00026200
       PROCEDURE                                  DIVISION.             00026300
      *=======================================================*         00026400
                                                                        00026500
                                                                        00026600
      *-------------------------------------------------------*         00026700
                                                                        00026800
      *         R O T I N A  P R I N C I P A L *                        00026900
                                                                        00027000
      *-------------------------------------------------------*         00028000
                                                                        00029000
      *-------------------------------------------------------*         00030000
       0000-PRINCIPAL                              SECTION.             00040000
      *-------------------------------------------------------*         00050000
                                                                        00051000
                  PERFORM 1000-INICIAR-CICS                             00051100
                  PERFORM 2000-PROCESSAR                                00051200
                  PERFORM 3000-FINALIZAR-CICS.                          00051300
                                                                        00051400
      *-------------------------------------------------------*         00051500
       0000-999-FIM.                               EXIT.                00051600
      *-------------------------------------------------------*         00051700
                                                                        00051800
                                                                        00051900
      *-------------------------------------------------------*         00052000
       1000-INICIAR-CICS                          SECTION.              00052100
      *-------------------------------------------------------*         00052200
                                                                        00052300
            EXEC CICS SEND                                              00052400
              MAPSET('F03CIM1')                                         00052500
              MAP('MAPTEST')                                            00052600
              ERASE                                                     00052700
              MAPONLY                                                   00052800
            END-EXEC.                                                   00052900
                                                                        00053000
                                                                        00053100
      *-------------------------------------------------------*         00053200
       1000-999-FIM.                              EXIT.                 00053300
      *-------------------------------------------------------*         00053400
                                                                        00053500
                                                                        00053600
      *-------------------------------------------------------*         00053700
       2000-PROCESSAR                             SECTION.              00053800
      *-------------------------------------------------------*         00053900
                                                                        00054000
            EXEC CICS RECEIVE                                           00055000
              MAPSET('F03CIM1')                                         00056000
              MAP('MAPTEST')                                            00057000
              INTO(MAPTESTI)                                            00058000
            END-EXEC.                                                   00059000
                                                                        00060000
           IF EIBAID = '6'                                              00070000
               MOVE IDI                    TO DB2-ID                    00080000
                                                                        00090000
            EXEC SQL                                                    00100000
              SELECT ID,NOME,SETOR,SALARIO,DATAADM,EMAIL                00110000
                INTO :DB2-ID,                                           00120000
                     :DB2-NOME,                                         00121000
                     :DB2-SETOR,                                        00122000
                     :DB2-SALARIO,                                      00123000
                     :DB2-DATAADM,                                      00124000
                     :DB2-EMAIL :WRK-EMAIL-NULL                         00125000
                 FROM FOUR001.FUNC                                      00126000
                 WHERE ID =:DB2-ID                                      00126100
            END-EXEC                                                    00126200
                                                                        00126300
                EVALUATE SQLCODE                                        00126400
                 WHEN 0                                                 00126500
                     MOVE DB2-ID                TO IDO                  00126600
                     MOVE DB2-NOME              TO NOMEO                00126700
      *              MOVE DB2-SETOR             TO SETORO               00126800
      *              MOVE DB2-SALARIO           TO WRK-SALARIO          00126900
      *              MOVE WRK-SALARIO           TO SALO                 00127000
      *              MOVE DB2-DATAADM           TO DATADMO              00127100
                                                                        00127200
                  IF WRK-EMAIL-NULL EQUAL -1                            00127300
                     MOVE SPACES            TO EMAILO                   00127400
                  ELSE                                                  00127500
                     MOVE DB2-EMAIL         TO EMAILO                   00127600
                  END-IF                                                00127700
                                                                        00127800
                    MOVE ' ENCONTRADO '       TO MSGO                   00127900
                WHEN 100                                                00128000
                  INITIALIZE MAPTESTO                                   00128100
                    MOVE ' NAO ENCONTRADO ' TO MSGO                     00128200
                WHEN OTHER                                              00128300
                  MOVE 'ERRO.....:'          TO MSGO                    00128400
                  MOVE WRK-SQLCODE           TO MSGO(11:04)             00128500
               END-EVALUATE                                             00128600
              END-IF.                                                   00128700
                                                                        00128800
             IF EIBAID = '5'                                            00128900
                   MOVE IDI             TO DB2-ID                       00129000
                   MOVE NOMEI           TO DB2-NOME                     00129100
                   MOVE SETORI          TO DB2-SETOR                    00129200
                   MOVE SALI            TO WRK-SALARIO                  00129300
                   MOVE WRK-SALARIO     TO DB2-SALARIO                  00129400
                   MOVE DATADMI         TO DB2-DATAADM                  00129500
                   MOVE EMAILI          TO DB2-EMAIL                    00129600
                                                                        00129700
                 EXEC SQL                                               00129800
                   INSERT INTO FOUR001.FUNC(ID,NOME,SETOR,SALARIO,      00129900
                                            DATAADM,EMAIL)              00130000
                   VALUES(:DB2-ID,                                      00130100
                          :DB2-NOME,                                    00130200
                          :DB2-SETOR,                                   00130300
                          :DB2-SALARIO,                                 00130400
                          :DB2-DATAADM,                                 00130500
                          :DB2-EMAIL )                                  00130600
                   END-EXEC                                             00130700
                                                                        00130800
                  EVALUATE SQLCODE                                      00130900
                    WHEN 0                                              00131000
                      EXEC SQL                                          00131100
                        COMMIT                                          00131200
                     END-EXEC                                           00131300
                          MOVE '-   INSERIDO -  ' TO MSGO               00131400
                     WHEN OTHER                                         00131500
                           MOVE SQLCODE TO WRK-SQLCODE                  00131600
                           MOVE 'ERRO ..: ' TO MSGO                     00131700
                           MOVE WRK-SQLCODE TO MSGO(11:04)              00131800
                       END-EVALUATE                                     00131900
                                                                        00132000
                 END-IF.                                                00132100
                                                                        00132200
            IF EIBAID = '8'                                             00132300
               MOVE IDI                  TO DB2-ID                      00132400
               MOVE NOMEI                TO WRK-NOME                    00132500
           EXEC SQL                                                     00132600
              SELECT ID, NOME, SETOR, SALARIO, DATAADM, EMAIL           00132700
               INTO :REG-FUNC                                           00132800
               FROM FOUR001.FUNC                                        00132900
               WHERE ID = :DB2-ID                                       00133000
           END-EXEC                                                     00134000
                                                                        00135000
               EVALUATE SQLCODE                                         00135100
                 WHEN 0                                                 00135200
                 IF WRK-NOME NOT EQUAL                                  00135300
                    DB2-NOME AND                                        00135400
                    WRK-NOME NOT EQUAL SPACES                           00135500
                    MOVE WRK-NOME   TO DB2-NOME                         00135600
                  END-IF                                                00135700
                                                                        00135800
                  IF SETORI       NOT EQUAL                             00135900
                     DB2-SETOR    AND                                   00136000
                     SETORI       NOT EQUAL SPACES                      00137000
                     MOVE SETORI         TO DB2-SETOR                   00138000
                                                                        00138100
                     MOVE 1              TO WRK-FLEG                    00138200
                   END-IF                                               00138300
                                                                        00138400
                  WHEN OTHER                                            00138500
                     MOVE 'ERRO....'     TO MSGO                        00138600
                                                                        00138700
              END-EVALUATE.                                             00138800
                                                                        00138900
             IF WRK-FLEG EQUAL 1                                        00139000
                                                                        00139100
           EXEC SQL                                                     00139200
              UPDATE FOUR001.FUNC                                       00139300
               SET  NOME     =:DB2-NOME,                                00139400
                    SETOR    =:DB2-SETOR,                               00139500
                    SALARIO  =:DB2-SALARIO,                             00139600
                    DATAADM  =:DB2-DATAADM,                             00139700
                    EMAIL    =:DB2-EMAIL                                00139800
                    WHERE  ID=:DB2-ID                                   00139900
           END-EXEC                                                     00140000
                                                                        00140100
                 IF SQLCODE EQUAL ZEROS                                 00140200
                      EXEC SQL                                          00140300
                        COMMIT                                          00140400
                     END-EXEC                                           00140500
                   ELSE                                                 00140600
                     MOVE '--ERROSCO-'       TO MSGO                    00140700
                 END-IF                                                 00140800
                                                                        00140900
               MOVE 'ATUALIZADO COM SUCESSO' TO MSGO                    00141000
                                                                        00141100
             END-IF.                                                    00141200
                                                                        00141300
                                                                        00141400
            IF EIBAID = '3'                                             00141500
               EXEC CICS                                                00141600
                 RETURN                                                 00141700
               END-EXEC                                                 00141800
            END-IF.                                                     00141900
                                                                        00142000
                                                                        00142100
            EXEC CICS SEND                                              00142200
              MAPSET('F03CIM1')                                         00142300
              MAP('MAPTEST')                                            00142400
              DATAONLY                                                  00142500
            END-EXEC.                                                   00142600
                                                                        00142700
      *-------------------------------------------------------*         00142800
       2000-999-FIM.                              EXIT.                 00142900
      *-------------------------------------------------------*         00143000
                                                                        00143100
                                                                        00143200
      *-------------------------------------------------------*         00143300
       3000-FINALIZAR-CICS                        SECTION.              00143400
      *-------------------------------------------------------*         00143500
                                                                        00143600
            EXEC CICS                                                   00143700
              RETURN TRANSID('T032')                                    00143800
            END-EXEC.                                                   00143900
                                                                        00144000
             MOVE 'FINALIZAR ' TO WRK-SAIDA.                            00144100
                                                                        00144200
      *-------------------------------------------------------*         00144300
       3000-999-FIM.                              EXIT.                 00144400
      *-------------------------------------------------------*         00144500
                                                                        00144600
