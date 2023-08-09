      *=============================================================*   00001003
       IDENTIFICATION                            DIVISION.              00010000
      *=============================================================*   00011003
                                                                        00012003
       PROGRAM-ID.  FR03CB03.                                           00020002
                                                                        00021003
      *=============================================================*   00030003
      *     AUTOR    : AUGUSTO MARTINS                                  00040000
      *     EMPRESA  : FOURSYS                                          00050000
      *     DATA     : 19/04/2022                                       00060001
      *     OBJETIVO : RECEBER DADOS DE CLIENTE                         00070001
      *              : DA SYSIN                                         00071002
      *==============================================================*  00080003
       ENVIRONMENT                                DIVISION.             00081003
      *==============================================================*  00081103
                                                                        00081204
      *-----------------------------------------------------------*     00081304
       CONFIGURATION                              SECTION.              00081404
      *-----------------------------------------------------------*     00081504
                                                                        00081604
         SPECIAL-NAMES.                                                 00081704
             DECIMAL-POINT IS COMMA.                                    00081804
                                                                        00081904
      *-----------------------------------------------------------*     00082004
                                                                        00082103
      *==============================================================*  00082203
       DATA                                       DIVISION.             00082300
      *==============================================================*  00082403
                                                                        00082503
      *--------------------------------------------------------------*  00082603
       WORKING-STORAGE                            SECTION.              00082800
      *--------------------------------------------------------------*  00082905
                                                                        00083005
       77 WRK-NOME       PICTURE X(15) VALUE SPACES.                    00083100
                                                                        00083203
      *--------------------------------------------------------------*  00083303
                                                                        00083403
      *==============================================================*  00083503
       PROCEDURE                                  DIVISION.             00083600
      *==============================================================*  00083703
                                                                        00083804
      ****************************************************************  00083904
      *                    ROTINA PRINCIPAL                          *  00084004
      ****************************************************************  00084104
                                                                        00084204
      *=========================================================*       00084304
       0000-PRINCIPAL                             SECTION.              00084404
      *=========================================================*       00084504
                                                                        00084606
            PERFORM 1000-INICIAR.                                       00084706
            PERFORM 2000-PROCESSAR.                                     00084806
            PERFORM 9999-FIMARQ.                                        00084906
                                                                        00085004
      *=========================================================*       00085104
       0000-99-FIM.                               EXIT.                 00085204
      *=========================================================*       00085304
                                                                        00085404
                                                                        00085504
      *************************************************************     00085604
      *                    PROCEDIMENTO INICIAL                   *     00085704
      *************************************************************     00085804
                                                                        00085904
      *-----------------------------------------------------------*     00086004
       1000-INICIAR                                  SECTION.           00086104
      *-----------------------------------------------------------*     00086204
                                                                        00086304
              ACCEPT WRK-NOME  FROM SYSIN.                              00086405
                                                                        00086504
      *-----------------------------------------------------------*     00086604
       1000-99-FIM.                                   EXIT.             00086704
      *-----------------------------------------------------------*     00087004
                                                                        00087105
      *-----------------------------------------------------------*     00087205
       2000-PROCESSAR                                SECTION.           00087305
      *-----------------------------------------------------------*     00087405
                                                                        00087505
             DISPLAY 'NOME: ' WRK-NOME.                                 00087605
                                                                        00087705
                                                                        00088004
      *-----------------------------------------------------------*     00088105
       2000-99-FIM.                                  EXIT.              00088207
      *-----------------------------------------------------------*     00088305
                                                                        00089005
                                                                        00089305
      *-----------------------------------------------------------*     00089404
       9999-FIMARQ                                  SECTION.            00089505
      *-----------------------------------------------------------*     00089604
             STOP RUN.                                                  00089705
      *-----------------------------------------------------------*     00089804
       9999-99-FIM.                                   EXIT.             00089904
      *-----------------------------------------------------------*     00090004
