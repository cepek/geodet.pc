C+
C MTMCTA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro cteni matice                                   *
C AUTOR: Eliska Valkova, VUGTK, nyni RIS hl. m. Prahy                 *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE MTMCTA (A,M,N,IZ)
	DOUBLE PRECISION A(1)
	INTEGER M,N,IZ
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Viz proceduru MTMCT1.                                       *
C PREHLED PARAMETRU - viz proceduru MTMCT1                            *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      READ (IZ,1)M,N
    1 FORMAT (2I5)
    3 FORMAT (5F16.0)
      K=1
      DO 2 I=1,M
      READ (IZ,3)(A(J),J=K,K+N-1)
    2 K=K+N
      END
