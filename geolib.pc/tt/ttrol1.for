C+
C TTROL1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vymaz obrazovky a (nebo) odradkovani           *
C AUTOR: Ales Cepek, VUGTK                                            *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE TTROL1(ITT,IPAR)
      INTEGER   ITT, IPAR
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura provadi vymaz obsahu obrazovky a (nebo) odradko-  *
C    vani.                                                            *
C VSTUPNI PARAMETRY:                                                  *
C    ITT    - cislo logicke jednotky prirazene obrazovce              *
C    IPAR   - IPAR < 0   procedura provede vymaz obrazovky a zapis    *
C                        ABS(IPAR) prazdnych radku (odradkovani),     *
C             IPAR = 0   vymaz obrazovky, kurzor zustane v levem      *
C                        hornim rohu obrazovky,                       *
C             IPAR > 0   procedura zapise IPAR prazdnych radku.       *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
C
    1 FORMAT(' ',4A1)
C
      IF(IPAR.LE.0)  WRITE(ITT,1) CHAR(27),'[','2','J'
      IF(IPAR.EQ.0)  RETURN
      DO 100 I = 1,IABS(IPAR)
  100 WRITE(ITT,1)
      RETURN
C
      END
