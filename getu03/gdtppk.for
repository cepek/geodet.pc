C POMOCNA PROCEDURA PRO DEFINICI POSTUPOVEHO KLICE
C======================================================================
C         1. VERZE      /SRPEN 1984/            VUGTK
C       * 5. VERZE      /ZARI 1989/             VUGTK      ALES CEPEK
C
      SUBROUTINE GDTPPK(TEXT,IOUT)
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  IOUT
      INTEGER*1  TEXT(1)
C
0102  CALL TTASK(ITT,TEXT,IOUT)
      IF(IOUT.GT.0.OR.IOUT.EQ.-2)  RETURN
0203     IF(IOUT.EQ.-1)  GOTO 0204
            WRITE(ITT,1)
1           FORMAT('0NEPRIPUSTNA ODPOVED, VYZVA SE OPAKUJE'/)
            GOTO 0299
0204     CONTINUE
            WRITE(ITT,2)
2           FORMAT('0<END> ... NAVRAT K PREDCHOZI VYZVE'/)
0299     CONTINUE
      GOTO 0102
C
      END
