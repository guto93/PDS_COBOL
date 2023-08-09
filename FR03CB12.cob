      *===========================================================*     00001003
       IDENTIFICATION                             DIVISION.             00002000
      *===========================================================*     00003003
       PROGRAM-ID.  FR03CB12.                                           00004000
      *===========================================================*     00005003
      *                                                                 00005203
      *     AUTOR    : AUGUSTO MARTINS                                  00006000
      *     EMPRESA  : FOURSYS                                          00007000
      *     DATA     : 20/04/2022                                       00008000
      *     OBJETIVO : OPERADOR ARITMETICO, SUBTRACT.                   00009000
      *              : RECEBER DOIS VALORES DA SYSIN E DEVOLVER         00010000
      *              : O RESULTADO QUE PODE SER NEGATIVO PARA WRK-RES   00011000
      *===========================================================*     00012003
       ENVIRONMENT                                DIVISION.             00013003
      *===========================================================*     00014003
       DATA                                       DIVISION.             00015000
      *===========================================================*     00016003
                                                                        00016103
      *-----------------------------------------------------------*     00016203
       WORKING-STORAGE                            SECTION.              00017000
      *-----------------------------------------------------------*     00018103
                                                                        00018203
       01 WRK-OPERADOR-AD.                                              00019000
          05 WRK-N1              PIC 99   VALUE ZEROS.                  00020000
          05 WRK-N2              PIC 99   VALUE ZEROS.                  00030000
          05 WRK-RES             PIC -ZZ  VALUE ZEROS.                  00040001
                                                                        00040103
      *-----------------------------------------------------------*     00041003
                                                                        00042003
      *==========================================================       00050000
       PROCEDURE                                  DIVISION.             00060000
      *==========================================================       00070000
                                                                        00071002
           ACCEPT WRK-OPERADOR-AD    FROM SYSIN.                        00071100
      * ----------------- SUBTRACT -------------------                  00071200
           SUBTRACT WRK-N1           FROM WRK-N2  GIVING WRK-RES.       00071300
                                                                        00071402
           DISPLAY '===================================='               00071500
           DISPLAY 'A SUBTRACAO DE NUM2:' WRK-N1                        00071600
                        ',POR NUM1:'      WRK-N2                        00071700
           DISPLAY 'TOTAL:'               WRK-RES                       00071800
           DISPLAY '==================================== '              00071900
           STOP RUN.                                                    00072000