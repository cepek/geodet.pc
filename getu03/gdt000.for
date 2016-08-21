C POMOCNA PROCEDURA PRO TISK POCTU BODU A OSNOV SMERU/DELEK NA TI:
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C
      SUBROUTINE GDT000
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  I,J,ISS,IDD
      LOGICAL  TSS,TDD
C
      ISS = 0
      IDD = 0
      DO 100 I=1,SBPOC
         IF(TYPB(I).EQ.' ')  GOTO 100
         TSS = .FALSE.
         TDD = .FALSE.
         DO 200 J=1,SMPOC
            IF(TYPP(J).EQ.' '.OR.CST(J).NE.I )  GOTO 200
            IF(TYPP(J).EQ.'S')  TSS = .TRUE.
            IF(TYPP(J).EQ.'D')  TDD = .TRUE.
200      CONTINUE
         IF(TSS)  ISS = ISS + 1
         IF(TSS.OR.TDD)  IDD = IDD + 1
100   CONTINUE
      WRITE(ITT,1)  POCBOD,ISS,IDD
1     FORMAT(/
     /' POCET BODU              :',I5/
     /' POCET OSNOV SMERU       :',I5/
     /' POCET OSNOV SMERU/DELEK :',I5/
     /)
      TSS = .TRUE.
      CALL GDTPAU(TSS)
C
      END
C TISK HLAVICKY PREHLEDU INFORMACI O BODECH A ORIENTACNIC POSUNECH
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C
      SUBROUTINE GDT005(LUN)
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  LUN
C
      IF(LUN.EQ.GSGDPT)  CALL GDSSTR(3,0)
      WRITE(LUN,1)
1     FORMAT('   CISLO BODU  CHAR TYP',
     /7X,'Y',14X,'X',
     /6X,'CHAR  OR. POSUN'/
     /59('='),' [G] ==='/)
C
      END
C TISK HLAVICKY PREHLEDU INFORMACI O MERENYCH PRVCICH
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C
      SUBROUTINE GDT006(LUN)
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  LUN
C
      IF(LUN.EQ.GSGDPT)  CALL GDSSTR(2,0)
      WRITE(LUN,1)
1     FORMAT('   STANOVISKO',13X,'CIL  T  MER.DELKA[M]   STR.CH.',
     /'   CHAR'/33('='),' /MER.SMER[G] = [MM/CC] ======')
C
      END
C TISK HLAVICKY - TEST LINEARIZACE
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C
      SUBROUTINE GDT114(LUN)
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  LUN
C
      IF(LUN.EQ.GSGDPT)  CALL GDSSTR(2,0)
      WRITE(LUN,114)
114   FORMAT(
     /'   STANOVISKO',12X,'CIL     T     DIF-D    DIF-SK     DIF-P'/
     /39('='),                          ' [MM] === [CC] ===== [MM]')
C
      END
C TISK HLAVICKY SIRENI CHYB V SITI
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C
      SUBROUTINE GDT120(LUN)
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  LUN
C
      IF(LUN.EQ.GSGDPT)  CALL GDSSTR(2,0)
      WRITE(LUN,120)
120   FORMAT(
     /10X,'MERENA VELICINA',19X,'VYROVNANA VELICINA'/
     /'  TYP === STAN./CIL = ZMENA[MM/CC] ==',
     /' TYP ==== STAN./CIL = ZMENA[MM/CC]')
C
      END
C TISK PREHLEDU OPERNYCH BODU / REG. ULOHA
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C
      SUBROUTINE GDT712(LUN)
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  LUN
      INTEGER*1  BOD(15)
71    FORMAT('0VYSLEDKY ODPOVIDAJI AUTOM. REGULARIZOVANE ULOZE')
72    FORMAT('0VYSLEDKY ODPOVIDAJI RESENI PRO MNOZINU OPERNYCH',
     /' BODU S CISLY:')
C
0103  IF(DEFEKT.LE.0)  GOTO 0199
0203     IF(POB.GT.0)  GOTO 0204
            IF(LUN.EQ.GSGDPT)  CALL GDSSTR(2,8)
            WRITE(LUN,71)
            GOTO 0299
0204     CONTINUE
            IF(LUN.EQ.GSGDPT)  CALL GDSSTR(2,8)
            WRITE(LUN,72)
            CALL GDTBU1(LUN.EQ.GSGDPT,LUN.EQ.ITT)
            DO 200 I=1,SBPOC
               IF(INTERT(I).NE.'O')  GOTO 200
               CALL GDTEXT(I,BOD(2))
               J = 15
               IF(BOD(14).EQ.' ')  J = 14
               BOD(J) = ','
               CALL GDTBUF(BOD,J)
200         CONTINUE
            CALL GDTBU2(1)
0299     CONTINUE
0199  CONTINUE
C
      END
