C VYHLEDANI MERENYCH PRVKU V DATOVE ZAKLADNE PRO ZADANE BODY
C=======================================================================
C         1. VERZE  /LISTOPAD 1984/         VUGTK
C
      SUBROUTINE GDTPM3(PRV,POS,PROC)
C
C PRV     PRACOVNI CISLO PRVNIHO ZPRACOVAVANEHO BODU
C POS     PRACOVNI CISLO POSLEDNIHO ZPRACOVAVANEHO BODU
C PROC    PROCEDURA  GDDHRS  NEBO  GDDHRC
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  IBOD,PRV,POS
      INTEGER  PSTA,PCIL,II,IOUT,PPBIT1,GDTSUM
      LOGICAL  NOVY
      INTEGER*1     S(13),C(13)
      REAL     GDSTCH
      INTEGER  POM,ERR,CISBOD
      COMMON  /GDDCHW/ POM(3),ERR,CISBOD(7)
$INCLUDE: 'GDI02W.COM'
    5 FORMAT(/' SEZNAM MERENYCH PRVKU JE OBSAZEN'/)
    6 FORMAT(/
     / ' V  DZ  JSEM NALEZL'/
     / ' PRO STANOVISKO ',13A1,A4/
     / ' A CIL          ',13A1,A4)
    7 FORMAT(' SMER  ',F11.7,' [G] +- ',F6.3,' [CC]')
    8 FORMAT(' DELKU ',F11.3,' [M] +- ',F6.3,' [MM]')
    9 FORMAT(' S CHARAKTERISTIKOU ZAMERY',I4)
C
      II = SMPOC + 1
7002  IF(II.GT.MAXMER)  GOTO 7099
         TYPP(II) = ' '
         II = II + 1
         GOTO 7002
7099  CONTINUE
      CISBOD(1) = ISEZ
0102  DO 0199 IBOD=PRV,POS
         IF(TYPB(IBOD).EQ.' ')  GOTO 0199
         CISBOD(2) = CB1(IBOD)
         CISBOD(3) = CB2(IBOD)
         CISBOD(4) = CB3(IBOD)
         CALL GDDHSR
         IF(ERR.GE.-2)  CALL PROC
0202     IF(ERR.LT.0)  GOTO 0299
            CALL GDDKOM(4)
0303        CALL GDTHCB(CSTAN,PSTA,.FALSE.,NOVY)
            IF(NOVY)  GOTO 0399
            CALL GDTHCB(CCILE,PCIL,.FALSE.,NOVY)
            IF(NOVY)  GOTO 0399
               CALL GDSKCB(3,S,II,CSTAN,II)
               CALL GDSKCB(3,C,II,CCILE,II)
0403           IF(CHSMER.LT.0)  GOTO 0499
0503              IF(PPBIT1(PK,1).NE.0)  GOTO 0599
                     WRITE(ITT,6) S,GDSTCH(CHAB(PSTA)),
     /                            C,GDSTCH(CHAB(PCIL))
                     WRITE(ITT,7) SMER*RO,CHSMER
                     IF(CHRAD.GT.0)  WRITE(ITT,9) CHRAD
                     CALL GDSDAN('/A/MAM PRVEK AKCEPTOVAT? (A/N): ',
     /               IOUT)
                     IF(IOUT.EQ.2)  GOTO 0499
0599              CONTINUE
                  SMPOC = GDTSUM(TYPP,MAXMER,' ')
                  IF(SMPOC.EQ.0)  GOTO 9000
                  PRVEK(SMPOC) = SMER
                  STRCH(SMPOC) = CHSMER
                  VAHA (SMPOC) = -1.
                  CST  (SMPOC) = PSTA
                  CCI  (SMPOC) = PCIL
                  TYPP (SMPOC) = 'S'
                  CHAZ (SMPOC) = CHRAD
0499           CONTINUE
0603           IF(CHD.LT.0)  GOTO 0699
0703              IF(PPBIT1(PK,1).NE.0)  GOTO 0799
                     WRITE(ITT,6) S,GDSTCH(CHAB(PSTA)),
     /                            C,GDSTCH(CHAB(PCIL))
                     WRITE(ITT,8) DELKA,CHD
                     IF(CHRAD.GT.0)  WRITE(ITT,9) CHRAD
                     CALL GDSDAN('/A/MAM PRVEK AKCEPTOVAT? (A/N): ',
     /               IOUT)
                     IF(IOUT.EQ.2)  GOTO 0699
0799              CONTINUE
                  SMPOC = GDTSUM(TYPP,MAXMER,' ')
                  IF(SMPOC.EQ.0)  GOTO 9000
                  PRVEK(SMPOC) = DELKA
                  STRCH(SMPOC) = CHD
                  VAHA (SMPOC) = -1.
                  CST  (SMPOC) = PSTA
                  CCI  (SMPOC) = PCIL
                  TYPP (SMPOC) = 'D'
                  CHAZ (SMPOC) = CHRAD
0699           CONTINUE
0399        CONTINUE
            CALL PROC
            GOTO 0202
0299     CONTINUE
0199  CONTINUE
8000  CONTINUE
      SMPOC = MAXMER
      SS = 2
      RETURN
C
9000  CALL GDSSTR(3,0)
      WRITE(GSGDPT,5)
      WRITE(ITT   ,5)
      GOTO 8000
C
      END
