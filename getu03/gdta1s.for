C ZMENA MNOZINY BODU SITE
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C
      SUBROUTINE GDTA1S(TISK,IOP)
C
$INCLUDE: 'GDTSOW.COM'
      LOGICAL  TISK
      INTEGER  IOP
1     FORMAT(///
     /' K DISPOZICI JSOU NASLEDUJICI OPERACE SE SOUBOREM INFORMACI O',
     /' SITI (IS):'//
     /' 1 ... ZMENA MNOZINY BODU SITE')
C
0103  IF(.NOT.TISK)  GOTO 0199
         WRITE(ITT,1)
         RETURN
0199  CONTINUE
      IF(IOP.NE.1)  RETURN
      CALL TTZAHL(ITT,'ZMENA MNOZINY BODU SITE:')
0102  WRITE(ITT,10)
10    FORMAT(/
     /' TYP ZMENY: 1 ... VYMAZ BODU'/
     /'            2 ... DOPLNENI BODU Z KLAVESNICE'/)
0202  CALL TTASK(ITT,'ZVOL TYP ZMENY (1-2): ',IOUT)
      IF(IOUT.EQ.-2) RETURN
      IF(IOUT.LE.0)  GOTO 0202
      IF(IOUT.EQ.1)  CALL GDTA1T
      IF(IOUT.EQ.2)  CALL GDTA1U
      CALL GDTKON
      GOTO 0102
C
      END
C VYMAZ BODU
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C
      SUBROUTINE GDTA1T
C
$INCLUDE: 'GDTSOW.COM'
      LOGICAL  VYMAZ,EOF
      INTEGER  I,IBOD
      INTEGER*1  BOD(13)
22    FORMAT(' VYRAZENY PRVEK: BOD ',13A1)
C
      IF(SS.LT.1)  RETURN
      CALL TTZAHL(ITT,'VYMAZ BODU:')
      VYMAZ = .FALSE.
0102  CONTINUE
      CALL GDTSCP('CISLO BODU: ',BOD,IBOD,EOF)
      IF(EOF)  GOTO 0199
         TYPB(IBOD) = ' '
         SBZ(IBOD)  = -1
         CALL GDSMEZ(BOD,13,I)
         CALL GDSSTR(1,0)
         WRITE(GSGDPT,22)  BOD
         VYMAZ = .TRUE.
         I = 1
0202     IF(I.GT.SMPOC)  GOTO 0299
            IF(CST(I).EQ.IBOD.OR.CCI(I).EQ.IBOD)  TYPP(I) = ' '
            I = I + 1
            GOTO 0202
0299     CONTINUE
         GOTO 0102
0199  CONTINUE
      IF(VYMAZ)  SS = 2
      RETURN
C
      END
C DOPLNENI BODU Z KLAVESNICE
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C       * 5. VERZE      /ZARI 1989/             VUGTK      ALES CEPEK
C
      SUBROUTINE GDTA1U
C
$INCLUDE: 'GDTSOW.COM'
      EXTERNAL GDDHRS,GDDHRC
      INTEGER  IOUT,INDEX
      LOGICAL  NOVY,ULOZ
      INTEGER  PPP(3),ERR,CISBOD(7)
      COMMON /GDDCHW/ PPP,ERR,CISBOD
C
      CALL GDSSTR(2,12)
      CALL GDTTHL(GSGDPT,'DEFINICE SITE:')
      WRITE(GSGDPT,1)
1     FORMAT(' PREHLED DOPLNENYCH BODU'/)
      CALL GDTZCP(-1,0)
      ULOZ = .FALSE.
      CALL TTZAHL(ITT,'DOPLNENI BODU Z KLAVESNICE:')
0102  CALL GDTPPK('TYP BODU (P/U): ',IOUT)
      IF(IOUT.EQ.-2) GOTO 0199
C*    ZAHLAV = .FALSE.         ***** ???????
      IF(IOUT.EQ.1)
     /CALL GDTZCB('CISLO PEVNEHO BODU: ',   'P',.FALSE.)
      IF(IOUT.EQ.2)
     /CALL GDTZCB('CISLO URCOVANEHO BODU: ','U',.FALSE.)
      CALL GDTHCB(CISBOD(2),INDEX,.FALSE.,NOVY)
      IF(NOVY)  GOTO 0102
      ULOZ = .TRUE.
0203  IF(SS.LE.1)  GOTO 0299
      CALL GDTPM3(INDEX,INDEX,GDDHRS)
      CALL GDTPM3(INDEX,INDEX,GDDHRC)
0299  CONTINUE
      GOTO 0102
0199  CONTINUE
      IF(ULOZ.AND.SS.GT.1)  CALL GDTPM4
      IF(ULOZ)  SS = 2
      RETURN
C
      END
