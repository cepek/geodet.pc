C TEST SEZNAMU DATOVE ZAKLADNY
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C
      LOGICAL FUNCTION GDTSEZ(CISSEZ)
C
C GDTSEZ  .TRUE.  SEZNAM NENI PRAZDNY
C         .FALSE. SEZNAM JE PRAZDNY
C CISSEZ  VSTUPNI PARAMETR, CISLO SEZNAMU
C
$INCLUDE: 'GDDCHW.COM'
      INTEGER  CISSEZ
C
      DO 100 POLOZK=ZACADR,KONADR
         IF(W(POLOZK).LT.0)  GOTO 100
         CALL GDDCST(.TRUE.)
         IF(KLCSTR(1).NE.CISSEZ)  GOTO 100
            GDTSEZ = .TRUE.
            GOTO 200
100   CONTINUE
      GDTSEZ = .FALSE.
200   KLCSTR(1) = -1
      RETURN
C
      END