C ANALYZA SIRENI CHYB
C=======================================================================
C         2. VERZE      /RIJEN 1985/         VUGTK      ALES CEPEK
C
      SUBROUTINE GDTSCH(AW)
C
$INCLUDE: 'GDTSOW.COM'
      REAL  AW
C*    VIRTUAL  AW(VIRDIM)
      DIMENSION  AW(VIRDIM)
      CHARACTER*(*) RET
      PARAMETER (RET='ANALYZA SIRENI CHYB V SITI:')
      INTEGER  PPBIT1,IOUT,ISTA,ICIL,IPOZ,IOPR,JOPR,NAVRAT,CHYBA,
     / IPOM,IBOD,GDTSUM,TYPVEL,HMEZ,I
      LOGICAL  PRT,EOF,NOVOSN,PRVNI
      INTEGER*1  POMTYP(2),T,MTYP(5),MVSTA(13),MVCIL(13),VTYP(7),
     / VVSTA(13),VVCIL(13),PTYP
      REAL  DELPOZ,DELVYR(2),GDTSKS,PI,PJ
      EXTERNAL  GDT120
      DATA  POMTYP /'D','S'/
121   FORMAT(/1X,5A1,1X,13A1,F12.3,4X,7A1,1X,13A1,F12.3)
1211  FORMAT(7X,13A1,24X,13A1)
1212  FORMAT(7X,13A1,22X,'X',F26.3)
122   FORMAT('0FUNKCE UMOZNUJE URCIT ZMENU KTEREKOLIV VYROVNANE',
     /' VELICINY V ZAVISLOSTI'/' NA ZMENE VYBRANE MERENE VELICINY'
     //)
C
      WRITE(ITT,122)
      CALL GDT712(ITT)
0103  IF(PPBIT1(PK,3).EQ.0)  GOTO 0104
         CALL GDSDAN('/A/MAM PORIZOVAT PROTOKOL? (A/N): ',IOUT)
         PRT = IOUT.EQ.1
         GOTO 0199
0104  CONTINUE
         PRT = PPBIT1(PK,5).EQ.0
0199  CONTINUE
0203  IF(.NOT.PRT)  GOTO 0299
         CALL GDSSTR(0,0)
         CALL GDT712(GSGDPT)
         CALL GDTTHL(GSGDPT,RET)
         CALL GDT120(GSGDPT)
0299  CONTINUE
0402  CONTINUE
0502  CALL TTASK(ITT,'TYP MERENE VELICINY (D/S): ',ITYP)
      IF(ITYP.EQ.-2) RETURN
      IF(ITYP.LE.0)  GOTO 0502
      CALL GDTSCP('STANOVISKO: ',MVSTA,ISTA,EOF)
      IF(EOF)  GOTO 0502
      CALL GDTSCP('CIL       : ',MVCIL,ICIL,EOF)
      IF(EOF)  GOTO 0502
      PTYP = POMTYP(ITYP)
      ASSIGN 0502 TO CHYBA
      ASSIGN 0600 TO NAVRAT
      GOTO 9000
0600  CONTINUE
      IOPR = IPOM
      PI   = ABS(VAHA(IPOZ))
0602  CALL TTASKR(ITT,'ZMENA [MM/CC]: ',IOUT,DELPOZ,1)
      IF(IOUT.EQ.-2) GOTO 0502
      IF(IOUT.LE.0)  GOTO 0602
      IF(ABS(DELPOZ).GE.1E6)  GOTO 0602
0700  WRITE(ITT,1)
1     FORMAT(/' PREHLED TYPU VYROVNANYCH VELICIN, JEJICHZ ZMENA SE',
     /' URCUJE:'/
     /' 1 ... DELKA (POKUD BYLA MERENA)'/
     /' 2 ... SMER  (POKUD BYL MEREN)'/
     /' 3 ... ORIENTACNI POSUN'/
     /' 4 ... SOURADNICE URCOVANEHO BODU'
     //)
0702  CALL TTASK(ITT,'ZVOL TYP VELICINY (1-4): ',TYPVEL)
      IF(TYPVEL.EQ.-2) GOTO 0502
      IF(TYPVEL.LE.0)  GOTO 0700
      IF(TYPVEL.LE.2)  GOTO 0900
C
      CALL GDTSCP('CISLO BODU: ',VVSTA,IBOD,EOF)
      IF(EOF)  GOTO 0700
      IF(IBOD.LE.0)  GOTO 0700
      IF(TYPVEL.NE.3)  GOTO 0800
         IF(SBZ(IBOD).GE.0)  GOTO 820
            CALL TTPUT1(ITT,1,'NEEXISTUJICI ORIENTACNI POSUN:')
            GOTO 0700
820      CONTINUE
         JOPR = POCPOZ
         DO 0850 I=1,IBOD
0850     IF(SBZ(I).GE.0.AND.TYPB(I).NE.' ')  JOPR = JOPR + 1
         PJ = 1E0
         GOTO 1000
0800  CONTINUE
0803  IF(TYPB(IBOD).EQ.'U')  GOTO 0899
         CALL TTPUT1(ITT,1,'BOD NENI URCOVANY:')
         GOTO 0700
0899  CONTINUE
      JOPR = POCPOZ + POCORP + 2*GDTSUM(TYPB,IBOD,'U') - 1
      PJ = 1E0
      GOTO 1000
C
0900  CONTINUE
      CALL GDTSCP('STANOVISKO: ',VVSTA,ISTA,EOF)
      IF(EOF)  GOTO 0700
      CALL GDTSCP('CIL       : ',VVCIL,ICIL,EOF)
      IF(EOF)  GOTO 0700
      PTYP = POMTYP(TYPVEL)
      ASSIGN 0700 TO CHYBA
      ASSIGN 0901 TO NAVRAT
      GOTO 9000
0901  JOPR = IPOM
      PJ = ABS(VAHA(IPOZ))
C
1000  CONTINUE
      HMEZ = 1
      IF(TYPVEL.EQ.4)  HMEZ = 2
      DO 1099 I=1,HMEZ
      JOPR = JOPR + I - 1
1099  DELVYR(I) = GDTSKS(AW,IOPR,JOPR)*PI/PJ*DELPOZ
C
1103  IF(.NOT.PRT)  GOTO 1199
      IF(POMTYP(ITYP).EQ.'D')  CALL PPMOV1(1,'DELKA',1,MTYP,1,5)
      IF(POMTYP(ITYP).EQ.'S')  CALL PPMOV1(1,'SMER ',1,MTYP,1,5)
      IF(TYPVEL.EQ.1)  CALL PPMOV1(1,'DELKA  ',1,VTYP,1,7)
      IF(TYPVEL.EQ.2)  CALL PPMOV1(1,'SMER   ',1,VTYP,1,7)
      IF(TYPVEL.EQ.3)  CALL PPMOV1(1,'OR.POS.',1,VTYP,1,7)
      IF(TYPVEL.EQ.4)  CALL PPMOV1(1,'SOUR. Y',1,VTYP,1,7)
C*    IF(TYPVEL.GT.2)  CALL PPINI1(' ',13,VVCIL)
      IF(TYPVEL.GT.2) THEN
         DO 9981 II81=1,13
9981        VVCIL(II81) = ' '
      END IF	
      CALL GDTSTR(3,0,GDT120)
      WRITE(GSGDPT,121)  MTYP,MVSTA,DELPOZ,VTYP,VVSTA,DELVYR(1)
      IF(TYPVEL.NE.4)  WRITE(GSGDPT,1211)  MVCIL,VVCIL
      IF(TYPVEL.EQ.4)  WRITE(GSGDPT,1212)  MVCIL,DELVYR(2)
1199  CONTINUE
      T = 'M'
      IF(TYPVEL.EQ.2.OR.TYPVEL.EQ.3)  T = 'C'
      WRITE(ITT,2)  (DELVYR(I),T,T,I=1,HMEZ)
2     FORMAT(/' VYVOLANA ZMENA JE ROVNA',2(F12.3,1X,2A1,2X))
      GOTO 0700
C
C VYHLEDANI ZADANEHO PRVKU V SEZMER (ISTA,ICIL,PTYP)
9000  CONTINUE
      PRVNI = .TRUE.
9001  CALL GDTOS1
9102  CALL GDTOSN(IPOZ,IPOM,NOVOSN,EOF)
      IF(EOF)  GOTO 9199
         IF(CST(IPOZ).EQ.ISTA.AND.CCI(IPOZ).EQ.ICIL
     /   .AND.TYPP(IPOZ).EQ.PTYP)  GOTO NAVRAT
         GOTO 9102
9199  CONTINUE
9203  IF(POMTYP(ITYP).NE.'D'.OR..NOT.PRVNI)  GOTO 9299
         IPOM  = ISTA
         ISTA  = ICIL
         ICIL  = IPOM
         PRVNI = .FALSE.
         GOTO 9001
9299  CONTINUE
      WRITE(ITT,123)
123   FORMAT('0NEEXISTUJICI PRVEK SITE'/)
      GOTO CHYBA
C
      END
