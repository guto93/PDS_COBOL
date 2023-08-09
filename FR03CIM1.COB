F03CIM2 DFHMSD TYPE=MAP,MODE=INOUT,LANG=COBOL,CTRL=FREEKB,TIOAPFX=YES   00010000
MAPTEST DFHMDI SIZE=(24,80)                                             00020000
        DFHMDF POS=(1,30),                                             *00030000
               LENGTH=11,                                              *00040000
               ATTRB=ASKIP,                                            *00050000
               INITIAL='CALCULADORA'                                    00060000
*****************************************************                   00070000
        DFHMDF POS=(05,20),LENGTH=10,INITIAL='CODIGO...:',ATTRB=ASKIP   00080000
ID      DFHMDF POS=(05,31),LENGTH=05,ATTRB=(UNPROT,IC,NUM)              00090000
        DFHMDF POS=(05,37),LENGTH=01,ATTRB=PROT                         00100000
*****************************************************                   00110000
                                                                        00120000
        DFHMDF POS=(07,20),LENGTH=10,INITIAL='NOME.....:',ATTRB=ASKIP   00130000
NOME    DFHMDF POS=(07,31),LENGTH=30,ATTRB=UNPROT                       00140000
        DFHMDF POS=(07,62),LENGTH=1,ATTRB=PROT                          00141000
*****************************************************                   00142000
                                                                        00143000
        DFHMDF POS=(09,20),LENGTH=10,INITIAL='SETOR....:',ATTRB=ASKIP   00144000
SETOR   DFHMDF POS=(09,31),LENGTH=04,ATTRB=UNPROT                       00145000
        DFHMDF POS=(09,36),LENGTH=1,ATTRB=PROT                          00146000
*****************************************************                   00147000
                                                                        00148001
        DFHMDF POS=(11,20),LENGTH=10,INITIAL='SALARIO..:',ATTRB=ASKIP   00149000
SAL     DFHMDF POS=(11,31),LENGTH=10,ATTRB=UNPROT                       00149100
        DFHMDF POS=(11,42),LENGTH=1,ATTRB=PROT                          00149200
*****************************************************                   00149300
                                                                        00149400
        DFHMDF POS=(13,20),LENGTH=10,INITIAL='DATA ADM.:',ATTRB=ASKIP   00149500
DATADM  DFHMDF POS=(13,31),LENGTH=10,ATTRB=UNPROT                       00149600
        DFHMDF POS=(13,42),LENGTH=1,ATTRB=PROT                          00149700
*****************************************************                   00149800
                                                                        00149900
        DFHMDF POS=(15,20),LENGTH=10,INITIAL='EMAIL....:',ATTRB=ASKIP   00150000
EMAIL   DFHMDF POS=(15,31),LENGTH=40,ATTRB=UNPROT                       00150100
        DFHMDF POS=(15,72),LENGTH=1,ATTRB=PROT                          00150200
*****************************************************                   00150300
        DFHMDF POS=(22,10),LENGTH=10,INITIAL='MSG......:',ATTRB=ASKIP   00150400
MSG     DFHMDF POS=(22,21),LENGTH=40,ATTRB=ASKIP                        00150500
        DFHMDF POS=(22,62),LENGTH=1,ATTRB=PROT                          00150600
*****************************************************                   00150700
        DFHMSD TYPE=FINAL                                               00150800
        END                                                             00150900
