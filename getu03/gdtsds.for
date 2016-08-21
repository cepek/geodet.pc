C SEZNAM VYROVNANYCH DELEK A SMERNIKU VYPOCTENYCH ZE SOURADNIC
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C
      SUBROUTINE GDTSDS(AW)
C
$INCLUDE: 'GDTSOW.COM'
      REAL  AW
C*    VIRTUAL  AW(VIRDIM)
      DIMENSION  AW(VIRDIM)
      CHARACTER*(*) RET
      PARAMETER
     /(RET='SEZNAM DELEK A SMERNIKU VYPOCTENYCH Z VYROV. SOURADNIC:')
      PARAMETER (ITRAD=20)
      INTEGER  IPOZ,IOPR,I,J,CIPOZ,INDEX,IER,CPOZ,GDTSUM
      INTEGER*4 GDTIND
      LOGICAL  NOVOSN,EOF,WAIT
      INTEGER*1  POSUN,QTABOD(MAX),STANOV(13),CIL(13)
      EXTERNAL  GDTA69
      DOUBLE PRECISION  Y(2),X(2),SM,D
      REAL  KOREK
70    FORMAT(A1,13A1,2X,13A1,F13.5,F13.6)
C
      CALL GDTPNV
      IF(KK.EQ.2)  RETURN
0103  IF(.NOT.LPT)  GOTO 0199
         CALL GDSSTR(0,0)
         CALL GDTTHL(GSGDPT,RET)
         CALL GDTA69(GSGDPT)
0199  CONTINUE
0203  IF(.NOT.LTT)  GOTO 0299
         CALL GDTTHL(ITT,RET)
         WAIT = .TRUE.
C CITAC ZPRACOVANYCH OSNOV
         IOSN = 0
0299  CONTINUE
      CALL GDTOS1
0302  CALL GDTOSN(IPOZ,IOPR,NOVOSN,EOF)
      IF(EOF)  GOTO 0399
0403  IF(.NOT.NOVOSN)  GOTO 0499
            POSUN = '0'
C*          CALL PPINI1(' ',SBPOC,QTABOD)
            DO 9981 II81=1,SBPOC
9981           QTABOD(II81) = ' '	
            CALL GDTEXT(CST(IPOZ),STANOV)
0503        IF(.NOT.LPT)  GOTO 0599
               J = 4
               CIPOZ = CCI(IPOZ)
               DO 500 I=1,SMPOC
500            IF(CCI(I).EQ.CIPOZ.AND.TYPP(I).NE.' ')  J = J + 1
               CALL GDTSTR(1,J,GDTA69)
0599        CONTINUE
0603        IF(.NOT.LTT)  GOTO 0699
                IOSN = IOSN + 1
                IF(IOSN.NE.1)  CALL GDTPAU(WAIT)
                IF(WAIT)  CALL GDTA69(ITT)
0699        CONTINUE
0499     CONTINUE
         CIPOZ = CCI(IPOZ)
         IF(QTABOD(CIPOZ).NE.' ')  GOTO 0302
         QTABOD(CIPOZ) = '*'
         Y(2) = SBY(CIPOZ)
         X(2) = SBX(CIPOZ)
0703     IF(TYPB(CIPOZ).EQ.'P')  GOTO 0799
            INDEX = POCPOZ + POCORP + 2*GDTSUM(TYPB,CIPOZ,'U')
            KOREK = AW(GDTIND(INDEX,NARED))*1E-3
            X(2) = X(2) + KOREK
            INDEX = INDEX - 1
            KOREK = AW(GDTIND(INDEX,NARED))*1E-3
            Y(2)  = Y(2) + KOREK
0799     CONTINUE
         CPOZ = CST(IPOZ)
         Y(1) = SBY(CPOZ)
         X(1) = SBX(CPOZ)
0803     IF(TYPB(CPOZ).EQ.'P')  GOTO 0899
            INDEX = POCPOZ + POCORP + 2*GDTSUM(TYPB,CPOZ,'U')
            KOREK = AW(GDTIND(INDEX,NARED))*1E-3
            X(1)  = X(1) + KOREK
            INDEX = INDEX - 1
            KOREK = AW(GDTIND(INDEX,NARED))*1E-3
            Y(1)  = Y(1) + KOREK
0899     CONTINUE
         CALL NGSMDA(Y,X,SM,D,IER)
         SM = SM*RO
         CALL GDTEXT(CCI(IPOZ),CIL)
0903     IF(.NOT.LPT)  GOTO 0999
            CALL GDTSTR(1,0,GDTA69)
            WRITE(GSGDPT,70)  POSUN,STANOV,CIL,D,SM
0999     CONTINUE
1103     IF(.NOT.LTT)  GOTO1199
            WRITE(ITT   ,70)  POSUN,STANOV,CIL,D,SM
1199     CONTINUE
1203     IF(.NOT.NOVOSN)  GOTO 1299
            POSUN = ' '
C*          CALL PPINI1(' ',13,STANOV)
            DO 9982 II82=1,13
9982           STANOV(II82) = ' '	
1299     CONTINUE
         GOTO 0302
0399  CONTINUE
      IF(LTT)  CALL GDTPAU(WAIT)
C
      END
