F03CIM1 DFHMSD TYPE=MAP,MODE=INOUT,LANG=COBOL,CTRL=FREEKB,TIOAPFX=YES, *
               MAPATTS=(COLOR)
MAPTEST DFHMDI SIZE=(24,80)
        DFHMDF POS=(2,19),                                             *
               LENGTH=34,                                              *
               ATTRB=ASKIP,                             			            *
               INITIAL='|--------------------------------|',           *
               COLOR=YELLOW
*****************************************************
        DFHMDF POS=(03,19),                                            *
               LENGTH=01,                                              *
               ATTRB=ASKIP,                                            *
               INITIAL='|',                                            *
               COLOR=YELLOW
*****************************************************
        DFHMDF POS=(3,25),                                             *
               LENGTH=25,                                              *
               ATTRB=ASKIP,                                            *
               INITIAL='RELATORIO DE CLIENTES ',                       *
               COLOR=NEUTRAL
*****************************************************
        DFHMDF POS=(03,52),                                            *
               LENGTH=01,                                              *
               ATTRB=ASKIP,                                            *
               INITIAL='|',                                            *
               COLOR=YELLOW
*****************************************************
        DFHMDF POS=(4,19),                                             *
               LENGTH=34,                                              *
               ATTRB=ASKIP,                             			            *
               INITIAL='|--------------------------------|',           *
               COLOR=YELLOW
*****************************************************

        DFHMDF POS=(05,20),LENGTH=02,INITIAL='  ',ATTRB=ASKIP
DELT    DFHMDF POS=(05,23),LENGTH=30,ATTRB=ASKIP
        DFHMDF POS=(05,54),LENGTH=1,ATTRB=PROT,                        *
               COLOR=NEUTRAL
*****************************************************
        DFHMDF POS=(07,20),LENGTH=10,INITIAL='CODIGO...:',ATTRB=ASKIP, *
               COLOR=BLUE
ID      DFHMDF POS=(07,31),LENGTH=05,ATTRB=(UNPROT,IC,NUM),            *
               PICOUT='9(05)'
        DFHMDF POS=(07,37),LENGTH=01,ATTRB=PROT
*****************************************************
        DFHMDF POS=(09,20),LENGTH=10,INITIAL='NOME.....:',ATTRB=ASKIP, *
               COLOR=BLUE
NOME    DFHMDF POS=(09,31),LENGTH=30,ATTRB=UNPROT
        DFHMDF POS=(09,62),LENGTH=1,ATTRB=PROT
*****************************************************
        DFHMDF POS=(11,20),LENGTH=10,INITIAL='SETOR....:',ATTRB=ASKIP, *
               COLOR=BLUE
SETOR   DFHMDF POS=(11,31),LENGTH=04,ATTRB=UNPROT
        DFHMDF POS=(11,36),LENGTH=1,ATTRB=PROT
*****************************************************
        DFHMDF POS=(13,20),LENGTH=10,INITIAL='SALARIO..:',ATTRB=ASKIP, *
               COLOR=BLUE
SAL     DFHMDF POS=(13,31),LENGTH=10,ATTRB=UNPROT
        DFHMDF POS=(13,42),LENGTH=1,ATTRB=PROT
*****************************************************
        DFHMDF POS=(15,20),LENGTH=10,INITIAL='DATA ADM.:',ATTRB=ASKIP, *
               COLOR=BLUE
DATADM  DFHMDF POS=(15,31),LENGTH=10,ATTRB=UNPROT
        DFHMDF POS=(15,42),LENGTH=1,ATTRB=PROT
FORM    DFHMDF POS=(15,44),LENGTH=10,INITIAL='YYYY-MM-DD'
*****************************************************
        DFHMDF POS=(17,20),LENGTH=10,INITIAL='EMAIL....:',ATTRB=ASKIP, *
               COLOR=BLUE
EMAIL   DFHMDF POS=(17,31),LENGTH=40,ATTRB=UNPROT
        DFHMDF POS=(17,72),LENGTH=1,ATTRB=PROT
*****************************************************
        DFHMDF POS=(20,10),LENGTH=10,INITIAL='MSG......:',ATTRB=ASKIP
MSG     DFHMDF POS=(20,21),LENGTH=40,ATTRB=ASKIP,                      *
               COLOR=NEUTRAL
        DFHMDF POS=(20,62),LENGTH=1,ATTRB=PROT
*****************************************************
        DFHMDF POS=(22,19),LENGTH=47,                                  *
               INITIAL='F3-EXIT    F5-INCLUIR  F6-CONSULTA',           *
               COLOR=BLUE
        DFHMDF POS=(23,19),LENGTH=40,                                  *
               INITIAL='F8-UPDATE  F9-DELETAR  ENTER-ATUALIZAR',       *
               COLOR=BLUE
        DFHMSD TYPE=FINAL
        END
