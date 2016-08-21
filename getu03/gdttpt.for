C TISK PARAMETRU SITE
C=======================================================================
C         1. VERZE  /CERVEN 1984/         VUGTK
C
      SUBROUTINE GDTTPT(LUN)
C
$INCLUDE: 'GDTSOW.COM'
      PARAMETER  (IDIM=15)
      INTEGER  I
      INTEGER*1  DZ(IDIM),SI(IDIM)
1     FORMAT(
     /' SOUBOR INFORMACI O SITI: ',15A1/
     /' DATOVA ZAKLADNA        : ',15A1/
     /' CISLO SEZNAMU          : ',I1/
     /' POCET SMERU      :',I4,22X,'POCET PEVNYCH BODU   :',I4/
     /' POCET DELEK      :',I4,22X,'POCET URCOVANYCH BODU:',I4/
     /' CELKEM POZOROVANI:',I4,22X,'CELKEM BODU          :',I4/
     /' POCET OSNOV SMERU:',I4,22X,'POCET NEZNAMYCH      :',I4)
C
      DO 0199 I=1,IDIM
         DZ(I) = ' '
         SI(I) = ' '
0199  CONTINUE
0303  IF(.NOT.PRASDZ)  GOTO 0399
         I = 1
0202     IF(I.GT.IDIM.OR.SOUBT(I).EQ.0)  GOTO 0299
            DZ(I) = SOUBT(I)
            I = I + 1
            GOTO 0202
0299     CONTINUE
0399  CONTINUE
      I = 1
0402  IF(I.GT.IDIM.OR.JMENO(I).EQ.0)  GOTO 0499
         SI(I) = JMENO(I)
         I = I + 1
         GOTO 0402
0499  CONTINUE
      POCNEZ = POCORP + 2*URCBOD
      CALL GDTTHL(LUN,'PREHLED PARAMETRU SITE:')
      IF(LUN.EQ.GSGDPT)  CALL GDSSTR(7,0)
      WRITE(LUN,1)
     / SI,DZ,ISEZ,
     / POCSME,PEVBOD,
     / POCDEL,URCBOD,
     / POCPOZ,POCBOD,
     / POCORP,POCNEZ
      RETURN
C
      END
