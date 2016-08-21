C TISK A ANALYZA PARAMETRU SITE
C=======================================================================
C         1. VERZE      /CERVEN 1984/           VUGTK
C       * 5. VERZE      /ZARI 1989/             VUGTK      ALES CEPEK
C
      SUBROUTINE GDTTPS(AW)
C
$INCLUDE: 'GDTSOW.COM'
      REAL  AW
C*    VIRTUAL  AW(VIRDIM)
      DIMENSION  AW(VIRDIM)
      INTEGER*4  IJ,JI,II,POCB4,IS,IC
      INTEGER  GDTSUM
      LOGICAL  SOUVIS
      LOGICAL*4  LKON
      REAL  RKON
      EQUIVALENCE  (LKON,RKON)
    3 FORMAT('0SIT NENI DEFINOVANA')
    4 FORMAT('0UPOZORNENI: VSECHNY BODY JSOU PEVNE')
    5 FORMAT('0UPOZORNENI: POCET PEVNYCH BODU NESTACI K JEDNOZNACNE',
     / ' LOKALIZACI SITE'/'             V SOUR.',
     / ' SOUSTAVE (VOLNA SIT)')
   15 FORMAT('0UPOZORNENI: SIT NENI SOUVISLA')
   16 FORMAT('0ULOHA MA PRILISNE PAMETOVE NAROKY')
C
      CALL GDTTPT(GSGDPT)
      CALL GDTTPT(ITT)
0103  IF(POCBOD.GT.0)  GOTO 0199
         CALL GDSSTR(2,0)
         WRITE(GSGDPT,3)
         WRITE(ITT   ,3)
         SS = 0
         KK = 1
         RETURN
0199  CONTINUE
0203  IF(POCPOZ.GT.0)  GOTO 0299
         CALL GDSSTR(2,0)
         WRITE(GSGDPT,3)
         WRITE(ITT   ,3)
         SS = 1
         KK = 1
         RETURN
0299  CONTINUE
      SS = 3
0303  IF(URCBOD.NE.0)  GOTO 0304
C        ZADNE URCOVANE BODY
0403     IF(POCORP.NE.0)  GOTO 0499
C           ZADNE OSNOVY SMERU
            CALL GDSSTR(2,0)
            WRITE(GSGDPT,3)
            WRITE(ITT   ,3)
            KK = 2
            RETURN
0499     CONTINUE
         CALL GDSSTR(2,0)
         WRITE(GSGDPT,4)
         WRITE(ITT   ,4)
         GOTO 0399
0304  CONTINUE
0503     IF(PEVBOD.GE.2)  GOTO 0599
            CALL GDSSTR(2,0)
            WRITE(GSGDPT,5)
            WRITE(ITT   ,5)
0599     CONTINUE
0399  CONTINUE
0683  CONTINUE
0703  POCB4 = POCBOD
      POCB4 = POCB4**2
      IF(POCB4.LE.VIRDIM)  GOTO 0799
            CALL TTPUT1(ITT,1,'K DISPOZICI NENI DOSTATECNA PAMET'//
     /      ' PRO TEST SOUVISLOSTI SITE:')
            GOTO 0699
0799     CONTINUE
         LKON = .FALSE.
         DO 0710 II=1,POCB4
            AW(II) = RKON
0710     CONTINUE
         LKON = .TRUE.
         DO 0720 II=1,SMPOC
            IS = CST(II) - GDTSUM(TYPB,CST(II),' ')
            IC = CCI(II) - GDTSUM(TYPB,CCI(II),' ')
            IJ = (IS-1)*POCBOD + IC
            JI = (IC-1)*POCBOD + IS
            AW(IJ) = RKON
            AW(JI) = RKON
0720     CONTINUE
C*       CALL GDTWAR(AW,VIRDIM,POCBOD,SOUVIS)
         CALL GDTWAR(AW,POCBOD,SOUVIS)
         IF(SOUVIS)  GOTO 0699
         CALL GDSSTR(2,0)
         WRITE(GSGDPT,15)
         WRITE(ITT   ,15)
0699  CONTINUE
0883  CONTINUE
         POCNEZ = POCORP + 2*URCBOD
         MARED  = POCNEZ + POCPOZ
         NARED  = POCNEZ + 1
         IF(FLOAT(MARED)*NARED.GT.VIRDIM)  GOTO 0884
C        DOSTATECNA KAPACITE PAMETI
         KK = 1
         GOTO 0899
0884  CONTINUE
         CALL GDSSTR(2,0)
         WRITE(GSGDPT,16)
         WRITE(ITT   ,16)
         SOUVIS = .TRUE.
         CALL GDTPAU(SOUVIS)
         KK = 2
0899  CONTINUE
      RETURN
C
      END
