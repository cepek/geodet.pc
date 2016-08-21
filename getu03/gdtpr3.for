C PRIPRAVA 3
C=======================================================================
C         1. VERZE  /ZARI 1984/         VUGTK
C
      SUBROUTINE GDTPR3
C
$INCLUDE: 'GDTSOW.COM'
      REAL  ALFPUL,MTSTU1
      DOUBLE PRECISION  MTNORA
C
      ALFPUL = (1E2-PRST)/2E2
0103  IF(TYPM0(1).NE.'A')  GOTO 0104
         KPRST = MTNORA(DBLE(ALFPUL))
         GOTO 0199
0104  CONTINUE
         KPRST = 0E0
         IF(NADBPO.GT.0)  KPRST = MTSTU1(ALFPUL,NADBPO)
0199  CONTINUE
C
      END
