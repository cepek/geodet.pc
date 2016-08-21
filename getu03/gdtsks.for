C SKALARNI SOUCIN RADKU SUBMATICE W11 A W21 MATICE AW
C=======================================================================
C         1. VERZE  /SRPEN 1984/         VUGTK
C
      REAL FUNCTION GDTSKS(AW,I,J)
C
$INCLUDE: 'GDTSOW.COM'
      REAL  AW
C*    VIRTUAL  AW(VIRDIM)
      DIMENSION  AW(VIRDIM)
      INTEGER*4  I,J,K,GDTIND
      DOUBLE PRECISION  SK,AWI,AWJ
C
      SK = 0D0
      DO 100 K=1,POCNEZ
      AWI = AW(GDTIND(I,K))
      AWJ = AW(GDTIND(J,K))
100   SK = SK + AWI*AWJ
      GDTSKS = SK
      RETURN
C
      END
