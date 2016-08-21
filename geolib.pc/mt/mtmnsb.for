C+
C MTMNSB
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro nasobeni symetrickych matic                    *
C AUTOR: Eliska Valkova, VUGTK, nyni RIS hl. m. Prahy                 *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MTMNSB (A,N,B,I,J,CIJ)
      DOUBLE PRECISION A(1),B(1),CIJ
      INTEGER N,I,J
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Viz proceduru MTMNS2.                                       *
C PREHLED PARAMETRU - viz proceduru MTMNS2                            *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      CIJ=0D0
      L=(I*(I-1))/2
      M=(J*(J-1))/2
      DO 1 K=1,N
      L=L+1
      M=M+1
      IF (K.GT.I) L=L+K-2
      IF (K.GT.J) M=M+K-2
    1 CIJ=CIJ+A(L)*B(M)
      END
