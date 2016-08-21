C PRECHOD K SESTAVENI ROVNIC OPRAV
C=======================================================================
C         1. VERZE  /CERVEN 1984/         VUGTK
C
      SUBROUTINE GDTSR1
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  PPBIT1,IOUT
C
      KK = 1
      IF(PPBIT1(PK,3).EQ.0)  RETURN
0102  CALL TTASK(ITT,
     /'/A/PREJDU K SESTAVENI ROVNIC OPRAV. SOUHLASIS? (A/N): ',IOUT)
0203     IF(IOUT.NE.-2)  GOTO 0204
            KK = 3
            RETURN
0204     IF(IOUT.NE.1)   GOTO 0205
            KK = 1
            RETURN
0205     IF(IOUT.NE.2)   GOTO 0206
            KK = 2
            RETURN
0206     CONTINUE
      GOTO 0102
C
      END
