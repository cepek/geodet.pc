C ANALYZA A TISK VYSLEDKU - OSTATNI OPERACE
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C
      SUBROUTINE GDTAOO(AW)
C
$INCLUDE: 'GDTSOW.COM'
      REAL  AW
C*    VIRTUAL  AW(VIRDIM)
      DIMENSION  AW(VIRDIM)
      INTEGER  IOUT
C
100   CALL GDTWTT(4,IOUT)
      KK = 1
      IF(IOUT.EQ.-2) RETURN
      IF(IOUT.LE.0)  GOTO 100
      GOTO (1,2,3,4,5,6,7,8,9,10) IOUT
1     CALL GDTZPA
      GOTO 100
2     CALL GDTLIN(AW)
      GOTO 100
3     CALL GDTOPV(AW)
      IF(KK.EQ.1)  IOUT = 4
      IF(KK.EQ.2)  IOUT = 1
      KK = IOUT
      RETURN
4     WRITE(ITT,4001)
4001  FORMAT(
     /'0UPOZORNENI: PO UKONCENI AKTUALIZACE NASLEDUJE PRIMY NAVRAT',
     /' K TE ETAPE'/
     /'             VYPOCTU, KTERA ODPOVIDA CHARAKTERU ZMEN V DATECH'
     //)
      CALL GDTPPK('/A/MAM PRIKROCIT K AKTUALIZACI? (A/N): ',IOUT)
      IF(IOUT.NE.1)  GOTO 100
      KK = 3
      RETURN
5     CALL GDTSCH(AW)
      GOTO 100
6     CALL GDTULZ(AW)
      GOTO 100
7     CONTINUE
8     CONTINUE
9     CONTINUE
      GOTO 100
10    KK = 2
      RETURN
C
      END
