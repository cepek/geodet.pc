C VYPIS PREHLEDU VYROVNANYCH POZOROVANI
C=======================================================================
C         2. VERZE      /RIJEN 1985/         VUGTK      ALES CEPEK
C
      SUBROUTINE GDTPVP(AW)
C
$INCLUDE: 'GDTSOW.COM'
      REAL  AW
C*    VIRTUAL  AW(VIRDIM)
      DIMENSION  AW(VIRDIM)
      INTEGER  IPOZ,IOPR,I,J,CIPOZ
      INTEGER*4 GDTIND
      LOGICAL  NOVOSN,EOF,WAIT
      EXTERNAL  GDTA66
      CHARACTER*(*) RET
      PARAMETER  (RET='PREHLED VYROVNANYCH POZOROVANI:')
      INTEGER*1  POSUN,STANOV(13),CIL(13),TYP,HV
      REAL  KOREK,GDTSKS,ML,MK
      DOUBLE PRECISION  PRIBL,VYROV
C*67  FORMAT(A1,13A1,1X,13A1,1X,A1,2F13.<IFORM>,F7.1,A1,F7.1)
67    FORMAT(A1,13A1,1X,13A1,1X,A1,2F13.5      ,F7.1,A1,F7.1)
68    FORMAT(A1,13A1,1X,13A1,1X,A1,2F13.6      ,F7.1,A1,F7.1)
C
      CALL GDTPNV
      IF(KK.EQ.2)  RETURN
      CALL GDTPR3
0103  IF(.NOT.LPT)  GOTO 0199
         CALL GDSSTR(0,0)
         CALL GDTTHL(GSGDPT,RET)
         CALL GDTA66(GSGDPT)
0199  CONTINUE
0203  IF(.NOT.LTT)  GOTO 0299
         CALL GDTTHL(ITT,RET)
         WAIT = .TRUE.
C CITAC ZPRACOVANYCH OSNOV
         IPOC = 0
0299  CONTINUE
      CALL GDTOS1
0302  CALL GDTOSN(IPOZ,IOPR,NOVOSN,EOF)
      IF(EOF)  GOTO 0399
0403     IF(.NOT.NOVOSN)  GOTO 0499
            POSUN = '0'
            CALL GDTEXT(CST(IPOZ),STANOV)
0503        IF(.NOT.LPT)  GOTO 0599
               J = 3
               CIPOZ = CCI(IPOZ)
               DO 500 I=1,SMPOC
500            IF(CCI(I).EQ.CIPOZ.AND.TYPP(I).NE.' ')  J = J + 1
               CALL GDTSTR(1,J,GDTA66)
0599        CONTINUE
0603        IF(.NOT.LTT)  GOTO 0699
               IPOC = IPOC + 1
               IF(IPOC.NE.1)  CALL GDTPAU(WAIT)
               IF(WAIT)  CALL GDTA66(ITT)
0699        CONTINUE
0499     CONTINUE
         CALL GDTEXT(CCI(IPOZ),CIL)
         TYP = TYPP(IPOZ)
         PRIBL = PRVEK(IPOZ)
         KOREK = AW(GDTIND(IOPR,NARED))
         KOREK = KOREK/ABS(VAHA(IPOZ))
         ML = M0AKT*SQRT(GDTSKS(AW,IOPR,IOPR))/ABS(VAHA(IPOZ))
         MK = KPRST*ML
0703     IF(TYP.NE.'D')  GOTO 0704
            KOREK = KOREK*1E-3
            HV    = '*'
C*          IFORM = 5
            VYROV = PRIBL + KOREK
            GOTO 0799
0704     CONTINUE
            KOREK = KOREK*1E-4
            PRIBL = PRIBL*RO
            HV    = ' '
C*          IFORM = 6
            VYROV = PRIBL + KOREK
            IF(VYROV.GT.4D2)  VYROV = VYROV - 4D2
            IF(VYROV.LT.0D0)  VYROV = VYROV + 4D2
0799     CONTINUE
0803     IF(.NOT.LPT)  GOTO 0899
C *** 2
            CALL GDTSTR(1,0,GDTA66)
	    IF(TYP.EQ.'D') THEN
               WRITE(GSGDPT,67)
     /         POSUN,STANOV,CIL,TYP,PRIBL,VYROV,ML,HV,MK
	    ELSE
               WRITE(GSGDPT,68)
     /         POSUN,STANOV,CIL,TYP,PRIBL,VYROV,ML,HV,MK
	    END IF
0899     CONTINUE
0903     IF(.NOT.LTT)  GOTO 0999
	    IF(TYP.EQ.'D') THEN
               WRITE(ITT   ,67)
     /         POSUN,STANOV,CIL,TYP,PRIBL,VYROV,ML,HV,MK
	    ELSE
               WRITE(ITT   ,68)
     /         POSUN,STANOV,CIL,TYP,PRIBL,VYROV,ML,HV,MK
	    END IF
0999     CONTINUE
1003     IF(.NOT.NOVOSN)  GOTO 1099
            POSUN = ' '
C*          CALL PPINI1(' ',13,STANOV)
            DO 9981 II81=1,13
9981           STANOV(II81) = ' '	
1099     CONTINUE
         GOTO 0302
0399  CONTINUE
      IF(LTT)  CALL GDTPAU(WAIT)
C
      END
