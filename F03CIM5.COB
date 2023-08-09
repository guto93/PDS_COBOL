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
        DFHMSD TYPE=FINAL
        END
