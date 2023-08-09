      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030002
       PROGRAM-ID. FR01CB02.                                            00040000
                                                                        00050000
      *=====================================================            00060000
      *  AUTOR: IVAN                                                    00070000
      *  EMPRESA: FOURSYS                                               00080000
      *=====================================================            00090000
                                                                        00100000
      *====================================================             00101000
       ENVIRONMENT                               DIVISION.              00102000
      *====================================================             00103000
                                                                        00110000
      *====================================================             00120000
       DATA                                      DIVISION.              00130000
      *====================================================             00140000
       WORKING-STORAGE                           SECTION.               00140103
       01 WRK-DADOS.                                                    00140205
          05 WRK-CODIGO PIC 9(05) VALUE ZEROS.                          00140305
          05 WRK-NOME   PIC X(30) VALUE SPACES.                         00140405
                                                                        00140507
       77 WRK-CONT-REG  PIC 9(05) COMP VALUE ZEROS.                     00140607
                                                                        00141000
      *====================================================             00150000
       PROCEDURE                                 DIVISION.              00160000
      *====================================================             00170000
       0000-PRINCIPAL                            SECTION.               00170105
           PERFORM 0100-INICIAR.                                        00171005
           PERFORM 0200-PROCESSAR UNTIL WRK-CODIGO = 99999              00173005
           PERFORM 0300-FINALIZAR.                                      00174105
           STOP RUN.                                                    00174207
       0100-INICIAR                              SECTION.               00175007
           ACCEPT WRK-DADOS.                                            00176005
       0200-PROCESSAR                            SECTION.               00179205
           DISPLAY 'CODIGO..... ' WRK-CODIGO.                           00180005
           DISPLAY 'NOME....... ' WRK-NOME.                             00181005
             ADD 1 TO WRK-CONT-REG.                                     00181107
            ACCEPT WRK-DADOS.                                           00181206
       0300-FINALIZAR                            SECTION.               00182005
           DISPLAY '-------- FIM ----------'.                           00183005
           DISPLAY ' TOTAL REGISTROS...... ' WRK-CONT-REG.              00184007
           DISPLAY '-------- FIM ----------'.                           00185007
