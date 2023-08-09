      *=============================================================*   00001008
       IDENTIFICATION                            DIVISION.              00010000
      *=============================================================*   00011008
                                                                        00012008
       PROGRAM-ID.  FR03CB04.                                           00020006
                                                                        00021008
      *=============================================================*   00030008
      *     AUTOR    : AUGUSTO MARTINS                                  00040000
      *     EMPRESA  : FOURSYS                                          00050000
      *     DATA     : 19/04/2022                                       00060005
      *     OBJETIVO : RECEBER DADOS DA SYSIN E                         00070007
      *              : IMPRIMIR                                         00071007
      *=============================================================*   00080008
       ENVIRONMENT                                DIVISION.             00081008
      *=============================================================*   00081108
                                                                        00081209
      *-----------------------------------------------------------*     00081309
       CONFIGURATION                              SECTION.              00081409
      *-----------------------------------------------------------*     00081509
                                                                        00081609
        SPECIAL-NAMES.                                                  00081709
            DECIMAL-POINT IS COMMA.                                     00081809
                                                                        00081909
      *-----------------------------------------------------------*     00082009
                                                                        00082208
      *=============================================================*   00082308
       DATA                                       DIVISION.             00082400
      *=============================================================*   00082508
                                                                        00082608
      *-------------------------------------------------------------*   00082708
       WORKING-STORAGE                            SECTION.              00082800
      *-------------------------------------------------------------*   00082909
                                                                        00083009
       01 WRK-CLIENTE.                                                  00083100
          05 WRK-ID         PIC 9(05) VALUE ZEROS.                      00083201
          05 WRK-CLIENT     PIC X(30) VALUE SPACES.                     00083300
          05 WRK-TEL        PIC 9(10) VALUE ZEROS.                      00083400
          05 WRK-GERENTE    PIC X(15) VALUE SPACES.                     00083500
                                                                        00083608
      *-------------------------------------------------------------*   00083708
                                                                        00083808
      *=============================================================*   00083908
       PROCEDURE                                  DIVISION.             00084000
      *=============================================================*   00084108
                                                                        00084209
                                                                        00084309
      ***************************************************************   00084409
      *                    ROTINA PRINCIPAL                         *   00084509
      ***************************************************************   00084609
                                                                        00084709
                                                                        00084809
      *=========================================================*       00084909
       0000-PRINCIPAL                             SECTION.              00085009
      *=========================================================*       00085109
                                                                        00085209
             PERFORM 1000-INICIAR.                                      00085310
             PERFORM 2000-PROCESSAR.                                    00085410
             PERFORM 9999-FIMARQ.                                       00085510
                                                                        00085609
      *=========================================================*       00085709
       1000-INICIAR                               SECTION.              00085809
      *=========================================================*       00085909
                                                                        00086009
           ACCEPT WRK-CLIENTE FROM SYSIN.                               00086109
                                                                        00086209
      *=========================================================*       00086309
       1000-99-FIM.                               EXIT.                 00086409
      *=========================================================*       00086509
                                                                        00086609
                                                                        00086709
      ***********************************************************       00086809
      *                    PROCESSAR                            *       00086909
      ***********************************************************       00087009
                                                                        00087109
                                                                        00087209
      *=========================================================*       00087309
       2000-PROCESSAR                             SECTION.              00087409
      *=========================================================*       00087509
                                                                        00087609
               DISPLAY '------------------------'                       00087709
               DISPLAY 'ID....   :' WRK-ID                              00087809
               DISPLAY 'CLIENTE  :' WRK-CLIENT                          00087909
               DISPLAY 'TELEFONE :' WRK-TEL                             00088009
               DISPLAY 'GERENTE. :' WRK-GERENTE                         00088109
               DISPLAY '------------------------'.                      00088209
                                                                        00088309
      *=========================================================*       00088409
       2000-99-FIM.                                  EXIT.              00088509
      *=========================================================*       00088609
                                                                        00088709
                                                                        00088809
      *=========================================================*       00088909
       9999-FIMARQ                                   SECTION.           00089009
      *=========================================================*       00091009
                                                                        00100009
                 STOP RUN.                                              00100109
                                                                        00100209
      *=========================================================*       00101009
       9999-99-FIM.                                  EXIT.              00102009
      *=========================================================*       00103009
                                                                        00110009