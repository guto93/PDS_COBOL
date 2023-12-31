       IDENTIFICATION                            DIVISION.              00010000
       PROGRAM-ID.  FR03CB07.                                           00020000
      *===========================================================      00030000
      *     AUTOR    : AUGUSTO MARTINS                                  00040000
      *     EMPRESA  : FOURSYS                                          00050000
      *     DATA     : 20/04/2022                                       00060000
      *     OBJETIVO : USO DE REDEFINES, PARA SEPARAR AREAS DO          00070000
      *              : CONTEUDO INSERIDO NA SYSIN.                      00070109
      *===========================================================      00071000
                                                                        00071110
                                                                        00071210
      *===========================================================      00071310
       ENVIRONMENT                                DIVISION.             00072010
      *===========================================================      00072110
                                                                        00072210
      *-----------------------------------------------------------*     00072310
       CONFIGURATION                              SECTION.              00072410
      *-----------------------------------------------------------*     00072510
                                                                        00072610
        SPECIAL-NAMES.                                                  00072710
            DECIMAL-POINT IS COMMA.                                     00072810
                                                                        00072910
                                                                        00073010
      *-----------------------------------------------------------*     00073110
                                                                        00073210
                                                                        00073310
      *===========================================================*     00073410
       DATA                                       DIVISION.             00073500
      *===========================================================*     00073610
                                                                        00073710
      *----------------------------------------------------------*      00073810
       WORKING-STORAGE                            SECTION.              00074000
      *----------------------------------------------------------*      00074110
                                                                        00074210
       01 WRK-REGISTRO-COMPLETO.                                        00075007
          05 WRK-NOME            PIC X(015) VALUE SPACES.               00076107
          05 WRK-RUA             PIC X(015) VALUE SPACES.               00076207
          05 WRK-NUMER           PIC 9(05)  VALUE ZEROS.                00076307
          05 WRK-BAIRRO          PIC X(015) VALUE SPACES.               00076407
       01 WRK-REG-NOME           REDEFINES WRK-REGISTRO-COMPLETO.       00076507
          05 WRK-NM              PIC X(15).                             00076607
          05 FILLER              PIC X(30).                             00076707
       01 WRK-ENDR-COMPLETO      REDEFINES  WRK-REGISTRO-COMPLETO.      00076807
          05 FILLER              PIC X(015).                            00076907
          05 WRK-RUA             PIC X(015).                            00077007
          05 WRK-NUMERO          PIC 9(05).                             00077107
          05 WRK-BAIRRO          PIC X(10).                             00077207
                                                                        00077310
      *----------------------------------------------------------*      00077410
                                                                        00077510
      *==========================================================*      00077610
       PROCEDURE                                  DIVISION.             00077700
      *==========================================================*      00077810
                                                                        00077910
                                                                        00078110
      *************************************************************     00078210
      *                    ROTINA PRINCIPAL                       *     00078310
      *************************************************************     00078410
                                                                        00078510
                                                                        00078610
      *==========================================================*      00078710
       0000-PRINCIPAL                             SECTION.              00078810
      *==========================================================*      00078910
                                                                        00079010
                 PERFORM  1000-INICIAR.                                 00079110
                 PERFORM  2000-PROCESSAR.                               00079210
                 PERFORM  9999-FIMARQ.                                  00079310
                                                                        00079510
      *==========================================================*      00079610
       0000-99-FIM.                                EXIT.                00079710
      *==========================================================*      00079810
                                                                        00079910
                                                                        00080010
      *************************************************************     00080110
      *                    PROCEDIMENTO INICIAL                   *     00080210
      *************************************************************     00080310
                                                                        00080410
                                                                        00080510
      *==========================================================*      00080610
       1000-INICIAR                                SECTION.             00080710
      *==========================================================*      00080810
                                                                        00081010
           ACCEPT WRK-REGISTRO-COMPLETO FROM SYSIN.                     00081110
                                                                        00081210
      *==========================================================*      00081310
       1000-99-FIM.                                EXIT.                00081410
      *==========================================================*      00081510
                                                                        00081610
                                                                        00081710
      *************************************************************     00081810
      *                    PROCESSAR                              *     00081910
      *************************************************************     00082010
                                                                        00082110
                                                                        00082210
      *==========================================================*      00082310
       2000-PROCESSAR                              SECTION.             00082410
      *==========================================================*      00082510
                                                                        00082710
           DISPLAY '------------------------'                           00082800
           DISPLAY 'NOME....   :' WRK-NM      OF WRK-REG-NOME           00082908
           DISPLAY 'RUA......  :' WRK-RUA     OF WRK-ENDR-COMPLETO.     00083003
           DISPLAY 'NUMERO.... :' WRK-NUMERO  OF WRK-ENDR-COMPLETO.     00083100
           DISPLAY 'BAIRRO.... :' WRK-BAIRRO  OF WRK-ENDR-COMPLETO.     00083200
           DISPLAY '------------------------'.                          00084000
                                                                        00085010
                                                                        00086010
      *==========================================================*      00086110
       2000-99-FIM.                                EXIT.                00086210
      *==========================================================*      00086310
                                                                        00087010
                                                                        00088010
      *==========================================================*      00088110
       9999-FIMARQ                                 SECTION.             00088210
      *==========================================================*      00088310
                                                                        00089010
           STOP RUN.                                                    00090000
                                                                        00100010
      *==========================================================*      00101010
       9999-99-FIM.                                EXIT.                00102010
      *==========================================================*      00103010
                                                                        00110010
