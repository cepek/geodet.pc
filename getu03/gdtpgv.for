C PRIPRAVA GLOBALNICH VELICIN
C=======================================================================
C         1. VERZE  /SRPEN 1984/         VUGTK
C
      SUBROUTINE GDTPGV(AW)
C
$INCLUDE: 'GDTSOW.COM'
      REAL  AW
C*    VIRTUAL  AW(VIRDIM)
      DIMENSION  AW(VIRDIM)
      INTEGER*4  GDTIND,I,J,K
      REAL  GDTSKS,VV,QV,XAM
C
      NADBPO = POCPOZ - POCNEZ + DEFEKT
      SUMVVS = 0E0
      SUMVVD = 0E0
      SUMQVS = 0E0
      SUMQVD = 0E0
      MAXDLT = 0E0
      INDELT = 0
0102  INDELT = INDELT + 1
      IF(TYPP(INDELT).EQ.' ')  GOTO 0102
C  INDEX BEZNEHO PRVKU V SEZMER
      I = 0
C  CISLO BEZNEHO RADKU V MATICI AW
      J = 0
0202  I = I + 1
      IF(I.GT.SMPOC)  GOTO 0299
      IF(TYPP(I).EQ.' ')  GOTO 0202
         J = J + 1
         VV = AW(GDTIND(J,NARED))
         VV = VV**2
         QV = 1E0 - GDTSKS(AW,J,J)
0303     IF(NADBPO.LE.0.OR.QV.LT.1E-4)  GOTO 0399
            XAM = VV/QV
            IF(XAM.LE.MAXDLT)  GOTO 0399
            MAXDLT = XAM
            INDELT = I
0399     CONTINUE
0403     IF(TYPP(I).NE.'S')  GOTO 0404
            SUMVVS = SUMVVS + VV
            SUMQVS = SUMQVS + QV
            GOTO 0499
0404     CONTINUE
            SUMVVD = SUMVVD + VV
            SUMQVD = SUMQVD + QV
0499     CONTINUE
         GOTO 0202
0299  CONTINUE
      PVV = SUMVVS + SUMVVD
      CALL TTZAHL(ITT,
     / 'ANALYZA A TISK VYSLEDKU, PRIP. ZPRACOVANI VOLNE SITE:')
      IF(NADBPO.GT.0)  M01 = SQRT(PVV/NADBPO)
0503  IF(NADBPO.GE.5)  GOTO 0504
         M0AKT = M0
         CALL PPMOV1(1,'APRIORNI',1,TYPM0,1,8)
         GOTO 0599
0504  CONTINUE
         M0AKT = M01
         CALL PPMOV1(1,' EMPIR. ',1,TYPM0,1,8)
0599  CONTINUE
      PRST = 95E0
      POB = 0
C*    CALL PPINI1(' ',25,NAZVAR)
      DO 9982 II82=1,25
9982     NAZVAR(II82) = ' '
      CALL PPMOV1(1,TYPB,1,INTERT,1,SBPOC)
      I = 1
0602  IF(I.GT.DEFEKT)  GOTO 0699
         J = ABS(INDS(I))
         DO 0799 K=POCPOZ+1,MARED
0799     AW(GDTIND(K,J)) = 0E0
         I = I + 1
         GOTO 0602
0699  CONTINUE
C
      END
