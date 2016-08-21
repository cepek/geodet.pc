C+
C MTMCT3
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro cteni trojuhelnikove matice                    *
C AUTOR: Eliska Valkova, VUGTK, nyni RIS hl. m. Prahy                 *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MTMCT3 (A,N,IZ)
      DIMENSION A(1)
      INTEGER N,IZ
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura cte dolni trojuhelnikovou cast ctvercove matice   *
C    stupne N volitelnym zarizenim a uklada ji po radcich do zvolene- *
C    ho, zpravidla jednorozmerneho pole. Pravidla pro predpis ctene   *
C    matice jsou popsana v textove casti dokumentace dane procedury,  *
C    odst. 2.                                                         *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETR:                                                   *
C    IZ     - cislo logicke jednotky, kterou bude matice prectena     *
C VYSTUPNI PARAMETRY:                                                 *
C    A      - pole, zpravidla jednorozmerne, s minimalni kapacitou    *
C             N*(N+1)/2 prvku, do ktereho bude po radcich ulozena     *
C             prectena dolni trojuhelnikova cast dane matice          *
C    N      - stupen prectene matice                                  *
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
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
