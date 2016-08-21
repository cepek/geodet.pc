C+
C MTMCTC
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro cteni trojuhelnikove matice                    *
C AUTOR: Eliska Valkova, VUGTK, nyni RIS hl. m. Prahy                 *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MTMCTC (A,N,IZ)
      DOUBLE PRECISION A(1)
      INTEGER N,IZ
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Viz proceduru MTMCT3.                                       *
C PREHLED PARAMETRU - viz proceduru MTMCT3                            *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      READ (IZ,1) N
    1 FORMAT (I5)
    3 FORMAT (5F16.0)
      K=1
      DO 2 I=1,N
      READ (IZ,3) (A(J),J=K,K+I-1)
    2 K=K+I
      END
