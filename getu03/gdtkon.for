C KONDENZACE SEZMER (ELIMINACE NEDEFINOVANYCH PRVKU)
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C
      SUBROUTINE GDTKON
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  POCET,I,GDTSUM
C
      POCET = SMPOC
      SMPOC = 0
      IF(POCET.LE.0)  GOTO 150
      DO 100 I=1,POCET
         IF(TYPP(I).NE.'S'.AND.TYPP(I).NE.'D')  GOTO 100
         SMPOC = SMPOC + 1
         CST(SMPOC)   = CST(I)
         CCI(SMPOC)   = CCI(I)
         PRVEK(SMPOC) = PRVEK(I)
         TYPP(SMPOC)  = TYPP(I)
         STRCH(SMPOC) = STRCH(I)
         VAHA(SMPOC)  = VAHA(I)
100   CONTINUE
150   POCSME = GDTSUM(TYPP,SMPOC,'S')
      POCDEL = GDTSUM(TYPP,SMPOC,'D')
      POCPOZ = POCSME + POCDEL
      PEVBOD = GDTSUM(TYPB,SBPOC,'P')
      URCBOD = GDTSUM(TYPB,SBPOC,'U')
      POCBOD = PEVBOD + URCBOD
      IF(POCPOZ.EQ.0)  SS = 1
      IF(POCBOD.GT.0)  GOTO 200
         SS = 0
         SBPOC = 0
         SMPOC = 0
200   CONTINUE
      RETURN
C
      END
