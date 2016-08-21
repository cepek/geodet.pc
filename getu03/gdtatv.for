C ANALYZA A TISK VYSLEDKU
C=======================================================================
C         1. VERZE  /ZARI 1984/         VUGTK
C
C KK = 1  UKONCENI VYPOCTU
C KK = 2  AKTUALIZACE SOUBORU IS
C KK = 3  OPAKOVANI VYROVNANI
C
      SUBROUTINE GDTATV(AW)
C
$INCLUDE: 'GDTSOW.COM'
      REAL  AW
C*    VIRTUAL  AW(VIRDIM)
      DIMENSION  AW(VIRDIM)
      INTEGER  IOUT
C
0102  CONTINUE
      CALL GDTWTT(1,IOUT)
0203  IF(IOUT.NE.1)  GOTO 0204
         CALL GDTAMV(AW)
         GOTO 0299
0204  IF(IOUT.NE.2)  GOTO 0205
         CALL GDTANN(AW)
         GOTO 0299
0205  IF(IOUT.NE.3)  GOTO 0206
         CALL GDTAVS(AW)
         IF(KK.EQ.2)  GOTO 1
         GOTO 0299
0206  IF(IOUT.NE.4)  GOTO 0207
         CALL GDTAOO(AW)
         IF(KK.EQ.2)  GOTO 1
         IF(KK.EQ.3)  GOTO 2
         IF(KK.EQ.4)  GOTO 3
         GOTO 0299
0207  IF(IOUT.NE.-2) GOTO 0299
         GOTO 1
0299  CONTINUE
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
