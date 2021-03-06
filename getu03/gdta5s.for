C VYPIS INFORMACI O BODECH A ORIENTACNICH POSUNECH
C=======================================================================
C         1. VERZE      /RIJEN 1984/            VUGTK
C       * 5. VERZE      /ZARI 1989/             VUGTK      ALES CEPEK
C
      SUBROUTINE GDTA5S(TISK,IOP)
C
$INCLUDE: 'GDTSOW.COM'
      LOGICAL  TISK,WAIT,GDTINC
      INTEGER  IOP,CITAC,II
      REAL  GDSTCH
      PARAMETER  (ITRAD =20,ITXT=11)
      INTEGER*1  STANOV(13),TEXT(ITXT)
      CHARACTER CTEXT*(ITXT)
      EQUIVALENCE (CTEXT,TEXT)
      CHARACTER*(*) RET
      PARAMETER
     / (RET='PREHLED INFORMACI O BODECH A ORIENTACNICH POSUNECH:')
      EXTERNAL  GDT005
C*1   FORMAT(1X,13A1,1X,A4,2X,A1,F14.5,F15.5,1X,A4,<ITXT>A1)
1     FORMAT(1X,13A1,1X,A4,2X,A1,F14.5,F15.5,1X,A4,11A1)
2     FORMAT(F11.6)
5     FORMAT(' 5 ... VYPIS INFORMACI O BODECH A ORIENTACNICH POSUNECH')
C
      IF(SS.LT.1)  RETURN
0103  IF(.NOT.TISK)  GOTO 0199
         WRITE(ITT,5)
         RETURN
0199  CONTINUE
      IF(IOP.NE.5)  RETURN
      CALL GDTPNV
      IF(KK.EQ.2)  RETURN
0203  IF(.NOT.LPT)  GOTO 0299
         CALL GDTTHL(GSGDPT,RET)
         CALL GDT005(GSGDPT)
0299  CONTINUE
0303  IF(.NOT.LTT)  GOTO 0399
         CALL GDTTHL(ITT,RET)
         CITAC = 0
         WAIT  = GDTINC(3-ITRAD)
         CALL GDT005(ITT)
0399  CONTINUE
      DO 100 II=1,SBPOC
         IF(TYPB(II).EQ.' ')  GOTO 100
         CALL GDTEXT(II,STANOV)
         CALL PPMOV1(1,'    -      ',1,TEXT,1,ITXT)
C*       IF(SBZ(II).GE.0)  ENCODE(ITXT,2,TEXT)  SBZ(II)*RO
         IF(SBZ(II).GE.0)  WRITE(CTEXT,2)       SBZ(II)*RO
0403     IF(.NOT.LPT)  GOTO 0499
            CALL GDTSTR(1,0,GDT005)
            WRITE(GSGDPT,1)  STANOV,GDSTCH(CHAB(II)),TYPB(II),
     /                       SBY(II),SBX(II),GDSTCH(CHAS(II)),TEXT
0499     CONTINUE
0503     IF(.NOT.LTT)  GOTO 0599
            WRITE(ITT   ,1)  STANOV,GDSTCH(CHAB(II)),TYPB(II),
     /                       SBY(II),SBX(II),GDSTCH(CHAS(II)),TEXT
0603        IF(.NOT.GDTINC(CITAC))  GOTO 0699
               CALL GDTPAU(WAIT)
               IF(II.LT.SBPOC.AND.WAIT)  CALL GDT005(ITT)
0699        CONTINUE
0599     CONTINUE
100   CONTINUE
      IF(LTT)  CALL GDTPAU(WAIT)
C
      END
