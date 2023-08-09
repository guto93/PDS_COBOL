F03CIM1 DFHMSD TYPE=MAP,MODE=INOUT,LANG=COBOL,CTRL=FREEKB,TIOAPFX=YES
MAPTEST DFHMDI SIZE=(24,80)
        DFHMDF POS=(2,19),                                             *
               LENGTH=34,                                              *
               ATTRB=ASKIP,                             			            *
               INITIAL='|--------------------------------|'
*****************************************************
TITULO  DFHMDF POS=(03,19),                                            *
               LENGTH=01,                                              *
               ATTRB=ASKIP,                                            *
               INITIAL='|'
*****************************************************
TIT1    DFHMDF POS=(3,21),                                             *
               LENGTH=30,                                              *
               ATTRB=ASKIP,                                            *
               INITIAL='RELATORIO DE CLIENTES '
*****************************************************
TIT2    DFHMDF POS=(03,52),                                            *
               LENGTH=01,                                              *
               ATTRB=ASKIP,                                            *
               INITIAL='|'
*****************************************************
TIT3    DFHMDF POS=(4,19),                                             *
               LENGTH=34,                                              *
               ATTRB=ASKIP,                             			            *
               INITIAL='|--------------------------------|'
*****************************************************
        DFHMDF POS=(05,20),LENGTH=10,INITIAL='CODIGO...:',ATTRB=ASKIP
ID      DFHMDF POS=(05,31),LENGTH=05,ATTRB=(UNPROT,IC,NUM)
        DFHMDF POS=(05,37),LENGTH=01,ATTRB=PROT
*****************************************************
        DFHMDF POS=(07,20),LENGTH=10,INITIAL='NOME.....:',ATTRB=ASKIP
NOME    DFHMDF POS=(07,31),LENGTH=30,ATTRB=UNPROT
        DFHMDF POS=(07,62),LENGTH=1,ATTRB=PROT
*****************************************************
        DFHMDF POS=(09,20),LENGTH=10,INITIAL='SETOR....:',ATTRB=ASKIP
SETOR   DFHMDF POS=(09,31),LENGTH=04,ATTRB=UNPROT
        DFHMDF POS=(09,36),LENGTH=1,ATTRB=PROT
*****************************************************
        DFHMDF POS=(11,20),LENGTH=10,INITIAL='SALARIO..:',ATTRB=ASKIP
SAL     DFHMDF POS=(11,31),LENGTH=10,ATTRB=UNPROT
        DFHMDF POS=(11,42),LENGTH=1,ATTRB=PROT
*****************************************************
        DFHMDF POS=(13,20),LENGTH=10,INITIAL='DATA ADM.:',ATTRB=ASKIP
DATADM  DFHMDF POS=(13,31),LENGTH=02,ATTRB=PROT
        DFHMDF POS=(13,34),LENGTH=01,INITIAL='/',ATTRB=ASKIP
DATAD   DFHMDF POS=(13,36),LENGTH=02,ATTRB=PROT
        DFHMDF POS=(13,39),LENGTH=01,INITIAL='/',ATTRB=ASKIP
*****************************************************
       DFHMDF POS=(15,20),LENGTH=10,INITIAL='EMAIL....:',ATTRB=ASKIP
EMAIL   DFHMDF POS=(15,31),LENGTH=40,ATTRB=UNPROT
        DFHMDF POS=(15,72),LENGTH=1,ATTRB=PROT
*****************************************************
        DFHMDF POS=(22,10),LENGTH=10,INITIAL='MSG......:',ATTRB=ASKIP
MSG     DFHMDF POS=(22,21),LENGTH=40,ATTRB=ASKIP
        DFHMDF POS=(22,62),LENGTH=1,ATTRB=PROT
*****************************************************
        DFHMSD TYPE=FINAL
        END
