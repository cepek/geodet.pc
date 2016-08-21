C TISK HLAVICKY - PREHLED OPRAV, ANALYZA POZOROVANI
C=======================================================================
C         1. VERZE  /ZARI 1984/         VUGTK
C
      SUBROUTINE GDTA62(LUN)
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  LUN
C
      IF(LUN.EQ.GSGDPT)  CALL GDSSTR(2,0)
      WRITE(LUN,62)
62    FORMAT('   STANOVISKO',11X,'CIL  T  F[%]       V    !V''!    ',
     /'EM-D/S   EV-D/S'/39('='),' [MM/CC] =========== [MM/CC] ===')
C
      END
C TISK ZAHLAVI PREHLEDU VYROVNANYCH POZOROVANI
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C
      SUBROUTINE GDTA66(LUN)
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  LUN
C
      IF(LUN.EQ.GSGDPT)  CALL GDSSTR(2,0)
      WRITE(LUN,66)
66    FORMAT('   STANOVISKO',11X,'CIL  T',
     /        ' MER.DELKA[M] VYR.DELKA[M] ST.CH. KONF.I.'/
     /30('='),' /MER.SMER[G] /VYR.SMER[G] = +-[MM/CC] ==')
C
      END
C TISK HLAVICKY SEZNAMU VYROV.DELEK A SMERNIKU VYPOCTENYCH ZE SOURADNIC
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C
      SUBROUTINE GDTA69(LUN)
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  LUN
C
      IF(LUN.EQ.GSGDPT)  CALL GDSSTR(2,0)
      WRITE(LUN,69)
69    FORMAT('   STANOVISKO',12X,'CIL      DELKA        SMERNIK'/
     /34('='),                              ' [M] ========= [G] ==')
C
      END
C TISK PREHLEDU INFORMACI O MERENYCH PRVCICH
C=======================================================================
C         2. VERZE      /RIJEN 1985/            VUGTK      ALES CEPEK
C       * 5. VERZE      /ZARI 1989/             VUGTK      ALES CEPEK
C
      SUBROUTINE GDTA6S(TISK,IOP)
C
$INCLUDE: 'GDTSOW.COM'
      LOGICAL  TISK,WAIT,NOVOSN,EOF
      INTEGER  IOP,IPOC,IPOZ,IOPR,J,CIPOZ,I
      CHARACTER*(*) RET
      PARAMETER  (RET='PREHLED INFORMACI O MERENYCH PRVCICH:')
      EXTERNAL  GDT006
      INTEGER*1  POSUN,STANOV(13),CIL(13),TYP,HV
      DOUBLE PRECISION  MER
      REAL  STCH,CHZ,GDSTCH
      REAL  NULCHZ
      DATA  NULCHZ /'  - '/
C*1   FORMAT(A1,13A1,3X,13A1,1X,A1,1X,F13.<IFR>,2X,F8.2,A1,1X,A4)
1     FORMAT(A1,13A1,3X,13A1,1X,A1,1X,F13.6    ,2X,F8.2,A1,1X,A4)
2     FORMAT(A1,13A1,3X,13A1,1X,A1,1X,F13.5    ,2X,F8.2,A1,1X,A4)
6     FORMAT(' 6 ... VYPIS INFORMACI O MERENYCH PRVCICH')
C
      IF(SS.LT.2)  RETURN
0103  IF(.NOT.TISK) GOTO 0199
         WRITE(ITT,6)
         RETURN
0199  CONTINUE
      IF(IOP.NE.6)  RETURN
      CALL GDTPNV
      IF(KK.EQ.2)  RETURN
1103  IF(.NOT.LPT)  GOTO 1199
         CALL GDTTHL(GSGDPT,RET)
         CALL GDT006(GSGDPT)
1199  CONTINUE
0203  IF(.NOT.LTT)  GOTO 0299
         CALL GDTTHL(ITT,RET)
         WAIT = .TRUE.
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
               CALL GDTSTR(1,J,GDT006)
0599        CONTINUE
0603        IF(.NOT.LTT)  GOTO 0699
               IPOC = IPOC + 1
               IF(IPOC.NE.1)  CALL GDTPAU(WAIT)
               IF(WAIT)  CALL GDT006(ITT)
0699        CONTINUE
0499     CONTINUE
         CALL GDTEXT(CCI(IPOZ),CIL)
         TYP = TYPP(IPOZ)
         STCH = STRCH(IPOZ)
         CHZ  = GDSTCH(CHAZ(IPOZ))
         IF(CHAZ(IPOZ).LE.0)  CHZ = NULCHZ
         MER  = PRVEK(IPOZ)
0703     IF(TYP.NE.'S')  GOTO 0704
            MER = MER*RO
            HV  = ' '
C*          IFR = 6
            GOTO 0799
0704     CONTINUE
            HV  = '*'
C*          IFR = 5
0799     CONTINUE
0803     IF(.NOT.LPT)  GOTO 0899
            CALL GDTSTR(1,0,GDT006)
	    IF(TYP.EQ.'S') THEN
               WRITE(GSGDPT,1)  POSUN,STANOV,CIL,TYP,MER,STCH,HV,CHZ
	    ELSE
               WRITE(GSGDPT,2)  POSUN,STANOV,CIL,TYP,MER,STCH,HV,CHZ
	    END IF
0899     CONTINUE
0903     IF(.NOT.LTT)  GOTO 0999
            IF(TYP.EQ.'S') THEN
               WRITE(ITT   ,1)  POSUN,STANOV,CIL,TYP,MER,STCH,HV,CHZ
	    ELSE
               WRITE(ITT   ,2)  POSUN,STANOV,CIL,TYP,MER,STCH,HV,CHZ
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
      CALL GDT000
C
      END
