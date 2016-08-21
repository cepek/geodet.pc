C VLASTNI VYPIS PREHLEDU STR. CHYB A PARAMETRU ELIPS CHYB (VYPIS 2)
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C       * 5. VERZE      /ZARI 1989/             VUGTK      ALES CEPEK
C
      SUBROUTINE GDTVY2(AW)
C
$INCLUDE: 'GDTSOW.COM'
      REAL  AW
C*    VIRTUAL  AW(VIRDIM)
      DIMENSION  AW(VIRDIM)
      EXTERNAL  GDTA90
      CHARACTER*(*) RET
      PARAMETER  (RET='PREHLED STR. CHYB A PARAMETRU ELIPS CHYB:')
      PARAMETER  (ITRAD=20)
      LOGICAL  WAIT,GDTINC
      INTEGER  CITAC,I,J,II,IER,PRV,POS
      INTEGER*4 GDTIND
      REAL  CYY,CXX,M0AKT2,GDTSKS,MP,MYX,A,B,ALFA,A1,B1,G,DY,DX
      REAL  CYX
      INTEGER*1  STANOV(13),TEXT(70)
      CHARACTER*70 CTEXT
      EQUIVALENCE (CTEXT,TEXT)
91    FORMAT(13A1,F8.1,F7.1,F8.1,4F7.1,F6.1)
911   FORMAT(1X,70A1)
C
0103  IF(.NOT.LPT)  GOTO 0199
         CALL GDSSTR(0,0)
         CALL GDTTHL(GSGDPT,RET)
         CALL GDTA90(GSGDPT)
0199  CONTINUE
0203  IF(.NOT.LTT)  GOTO 0299
         CALL GDTTHL(ITT,RET)
         CALL GDTA90(ITT)
         WAIT = GDTINC(3-ITRAD)
         CITAC = 0
0299  CONTINUE
      M0AKT2 = M0AKT**2
C PORADOVE CISLO NEZNAME
      I = POCPOZ + POCORP
      DO 100 II=1,SBPOC
      IF(TYPB(II).NE.'U')  GOTO 100
         I   = I + 1
         CYY = M0AKT2*GDTSKS(AW,I,I)
         CYX = M0AKT2*GDTSKS(AW,I,I+1)
         DY  = AW(GDTIND(I,NARED))
         I   = I + 1
         CXX = M0AKT2*GDTSKS(AW,I,I)
         DX  = AW(GDTIND(I,NARED))
         CALL GDTEXT(II,STANOV)
         MP  = CYY + CXX
         MYX = SQRT(MP/2E0)
         MP  = SQRT(MP)
C*       CALL PPINI4(0E0,,A,B,ALFA,A1,B1,G)
         A  = 0
	 B  = 0
	 ALFA = 0
	 A1 = 0
	 B1 = 0
	 G  = 0
         CALL MTELP2(CYY,CYX,CXX,A,B,ALFA,IER)
         PRV = 29
         POS = 70
0303     IF(IER.EQ.1)  GOTO 0399
            PRV = 66
            A1  = KPRST*A
            B1  = KPRST*B
0403        IF(.NOT.(A.GT.0E0.AND.B.GT.0E0))  GOTO 0499
               PRV = 71
               G   = SQRT(((DX*COS(ALFA)+DY*SIN(ALFA))/A1)**2 +
     /                    ((DY*COS(ALFA)-DX*SIN(ALFA))/B1)**2 )
0499        CONTINUE
            ALFA = ALFA *RO
            IF(ALFA.GT.4E2)  ALFA = ALFA - 4E2
            IF(ALFA.LT.0E0)  ALFA = ALFA + 4E2
0399     CONTINUE
C*       ENCODE(70,91,TEXT)  STANOV,MP,MYX,A,B,ALFA,A1,B1,G
         WRITE(CTEXT,91)     STANOV,MP,MYX,A,B,ALFA,A1,B1,G
         J = PRV
0802     IF(J.GT.POS)  GOTO 0899
            IF(TEXT(J).NE.'.')  TEXT(J) = ' '
            IF(TEXT(J).EQ.'.')  TEXT(J) = '-'
            J = J + 1
            GOTO 0802
0899     CONTINUE
0503     IF(.NOT.LPT)  GOTO 0599
            CALL GDTSTR(1,0,GDTA90)
            WRITE(GSGDPT,911)  TEXT
0599     CONTINUE
0603     IF(.NOT.LTT)  GOTO 0699
            WRITE(ITT   ,911)  TEXT
0703        IF(.NOT.GDTINC(CITAC))  GOTO 0799
               CALL GDTPAU(WAIT)
               IF(I.LT.MARED.AND.WAIT)  CALL GDTA90(ITT)
0799        CONTINUE
0699     CONTINUE
100   CONTINUE
      IF(LTT)  CALL GDTPAU(WAIT)
C
      END
