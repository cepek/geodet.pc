C VYPOCET MAXIMALNI DIMENZE PRACOVNICH POLI INDC A ROW (VIZ GTGETR)
C=======================================================================
C
      SUBROUTINE GTDIMR(XRNZ,POCNEZ,DIMROW)
C
C XRNZ    VEKTOR SMERNIKU SEZNAMU NENULOVYCH PRVKU V RADCICH
C POCNEZ  POCET NEZNAMYCH
C DIMROW  MAXIMALNI DIMENZE PRACOVNICH POLI INDEC A ROW (MAXIMALNI POCET
C         NENULOVYCH PRVKU V RADKU ROZKLADU R)
C
      INTEGER  XRNZ(1), POCNEZ, DIMROW, NONZ, I
C
      DIMROW = 0
      DO 100 I=1,POCNEZ
         NONZ = XRNZ(I+1) - XRNZ(I)
         IF(NONZ.GT.DIMROW)  DIMROW = NONZ
100   CONTINUE
      DIMROW = DIMROW + 1
      RETURN
C
      END
