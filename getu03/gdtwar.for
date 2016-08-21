C TEST SOUVISLOSTI SITE
C=======================================================================
C         1. VERZE      /PROSINEC 1984/         VUGTK
C       * 5. VERZE      /ZARI 1989/             VUGTK      ALES CEPEK
C
C WARSHALLUV ALGORITMUS:
C **********************
C
C DO K = 1 TO N;
C    DO I = 1 TO N;
C       DO J = 1 TO N;
C          P(I,J)=P(I,J).OR.(P(I,K).AND.P(K,J));
C       END;
C    END;
C END;
C
C N  ...  POCET UZLU GRAFU
C P(I,J)  PRVEK INCIDENCNI MATICE GRAFU
C         P(I,J) = TRUE  EXISTUJE-LI HRANA I-->J,
C         P(I,J) = FALSE V OPACNEM PRIPADE.
C
C JE-LI PO PROVEDENI ALGORITMU HODNOTA NEKTEREHO NEDIAGONALNIHO
C PRVKU FALSE, GRAF NENI SOUVISLY.
C
C-----------------------------------------------------------------------
C
C*    SUBROUTINE GDTWAR(AW,DIMAW,DIM,SOUVIS)
      SUBROUTINE GDTWAR(AW,DIM,SOUVIS)
C
$INCLUDE: 'GDTSOW.COM'
C
      REAL  AW
C POCET BODU SITE
      INTEGER  DIM
C VYSTUPNI PARAMETR - SIT JE/NENI SOUVISLA
      LOGICAL  SOUVIS
C INCIDENCNI MATICE UKLADANA PO RADCICH
C*    VIRTUAL  AW(DIMAW)
      DIMENSION  AW(VIRDIM)
      INTEGER*4 DIM1,K,I,J,K1,IK,IJ,KJ
      REAL  RIK,RIJ,RKJ,RTRUE
      LOGICAL*4  PIK,PIJ,PKJ,PTRUE
      EQUIVALENCE  (RIK,PIK),(RIJ,PIJ),(RKJ,PKJ),(RTRUE,PTRUE)
C
      PTRUE = .TRUE.
      DIM1  = -DIM + 1
      DO 100 K=1,DIM
         K1 = K*DIM + DIM1
         DO 200 I=1,DIM
            IK = (I-1)*DIM + K
            RIK = AW(IK)
            IF(.NOT.PIK)  GOTO 200
            IJ = I*DIM + DIM1
            KJ = K1
            DO 300 J=1,DIM
               RIJ = AW(IJ)
               IF(PIJ)  GOTO 400
               RKJ = AW(KJ)
               IF(.NOT.PKJ)  GOTO 400
               AW(IJ) = RTRUE
400            IJ = IJ + 1
300            KJ = KJ + 1
200      CONTINUE
100   CONTINUE
      SOUVIS = .FALSE.
      DO 500 I=1,DIM
         DO 600 J=1,DIM
            IF(I.EQ.J)  GOTO 600
            IJ = (I-1)*DIM + J
            RIJ = AW(IJ)
            IF(.NOT.PIJ)  RETURN
600      CONTINUE
500   CONTINUE
      SOUVIS = .TRUE.
      RETURN
C
      END
