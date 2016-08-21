C PRECHOD K RESENI ROVNIC OPRAV
C======================================================================
C         1. VERZE  /SRPEN 1984/         VUGTK
C
C KK = 1  NASLEDUJE RESENI ROVNIC OPRAV
C KK = 2  AKTUALIZACE SOUBORU IS
C KK = 3  UKONCENI PROGRAMU
C
      SUBROUTINE GDTRR1
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  IOUT,PPBIT1
C
      IF(PPBIT1(PK,3).EQ.0)  GOTO 1
0102  CALL TTASK(ITT,
     / '/A/PREJDU K RESENI ROVNIC OPRAV. SOUHLASIS? (A/N): ',IOUT)
         IF(IOUT.EQ.1)  GOTO 1
         IF(IOUT.EQ.2)  GOTO 2
         IF(IOUT.EQ.-2) GOTO 3
      GOTO 0102
C
1     KK = 1
      RETURN
2     KK = 2
      RETURN
3     KK = 3
      RETURN
C
      END
