C
C COMMON /GDI01W/ - SOURADNICOVY RADEK - NEKOMPRIMOVANA FORMA
C=====================================================================
C Y       SOURADNICE
C X       SOURADNICE
C V       VYSKA
C Z       ORIENTACNI POSUN, (OBLOUKOVA MIRA, INTERVAL <0,2PI))
C CHOPOS  CHARAKTERISTIKA (STREDNI CHYBA) ORIENTACNIHO POSUNU (CC)
C CISLOB  CISLO BODU
C CISLOS  CISLO SEZNAMU
C CHBODU  CHARAKTERISTIKA BODU (SOURADNICOVEHO RADKU)
C CHSOUR  CHARAKTERISTIKA SOURADNIC
C CHVYSK  CHARAKTERISTIKA VYSKY
      DOUBLE PRECISION  Y,X,V,Z
      REAL  CHOPOS
      INTEGER CISLOB(3)
      INTEGER*1  CISLOS,CHBODU,CHSOUR,CHVYSK
      COMMON/GDI01W/Y,X,V,Z,CHOPOS,CISLOB,CISLOS,CHBODU,CHSOUR,CHVYSK
