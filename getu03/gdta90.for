C TISK ZAHLAVI PREHLEDU STR. CHYB A PARAMETRU ELIPS CHYB
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C
      SUBROUTINE GDTA90(LUN)
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  LUN
C
      IF(LUN.EQ.GSGDPT)  CALL GDSSTR(3,0)
      WRITE(LUN,90)
90    FORMAT('   CISLO BODU    ',
     /        '  MP     MYX   STRED. EL. CHYB   KONFID. EL. CHYB   G'/
     /17('='),' [MM] = [MM] ==== A [MM] B  ALFA[G]  A'' [MM] B'' ',
     /6('=')/)
C
      END
C TISK HLAVICKY VYPISU SEZNAMU SOURADNIC BODU SITE
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C
      SUBROUTINE GDTA93(LUN)
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  LUN
C
      IF(LUN.EQ.GSGDPT)  CALL GDSSTR(3,0)
      WRITE(LUN,93)
93    FORMAT('   CISLO BODU   TYP',9X,'Y',15X,'X',/50('=')/)
C
      END
C TISK ZAHLAVI VYPISU SMERNIKU
C=======================================================================
C         2. VERZE      /RIJEN 1985/         VUGTK      ALES CEPEK
C
      SUBROUTINE GDTA96(LUN)
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  LUN
C
      IF(LUN.EQ.GSGDPT)  CALL GDSSTR(2,0)
      WRITE(LUN,96)
96    FORMAT('   STANOVISKO',13X,
     /'CIL    VYR.SMERNIK   STR.CH.  KONF.I.   EV-SK'/
     /37('='),   '[G]====== +-[CC] = +-[CC] === [CC]')
C
      END
