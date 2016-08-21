C VYPOCET KOEF. ZVETSENI KONFID. ELIPSY
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C
      SUBROUTINE GDTPR4
C
$INCLUDE: 'GDTSOW.COM'
      REAL  ALFA,MTCHI1
C
      ALFA = (1E2-PRST)/1E2
0103  IF(TYPM0(1).NE.'A')  GOTO 0104
         KPRST = SQRT(MTCHI1(ALFA,2))
         GOTO 0199
0104  CONTINUE
         KPRST = 0E0
         IF(NADBPO.GT.0)  KPRST = SQRT(NADBPO*(ALFA**(-2E0/NADBPO)-1))
0199  CONTINUE
C
      END
