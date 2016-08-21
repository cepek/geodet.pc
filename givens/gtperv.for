C PERMUTACE VEKTORU 
C=======================================================================
C
      SUBROUTINE GTPERV(DIM,V,TMP,PERM)
C
C DIM     DIMENZE VEKTORU V (TMP)
C V       VEKTOR 
C TMP     PRACOVNI VEKTOR
C PERM    PERMUTACE
C
      INTEGER  DIM, PERM(1), I, J
C$INCLUDE: 'GTREAL.INC'
      DOUBLE PRECISION
     /         V(1), TMP(1)
C
      DO 100 I=1,DIM
         TMP(I) = V(I)
100   CONTINUE
      DO 200 I=1,DIM
         J = PERM(I)
         V(I) = TMP(J)
200   CONTINUE
      RETURN
C
      END
