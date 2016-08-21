C+
C MTMCT1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro cteni matice                                   *
C AUTOR: Eliska Valkova, VUGTK, nyni RIS hl. m. Prahy                 *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE MTMCT1 (A,M,N,IZ)
	DIMENSION A(1)
	INTEGER M,N,IZ
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura precte matici typu (M x N) volitelnym zarizenim   *
C    a ulozi ji po radcich do zvoleneho pole, zpravidla jednorozmer-  *
C    neho.                                                            *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETR:                                                   *
C    IZ     - cislo logicke jednotky, kterou bude matice prectena     *
C VYSTUPNI PARAMETRY:                                                 *
C    M      - pocet radku prectene matice                             *
C    N      - pocet sloupcu prectene matice                           *
C    A      - pole, zpravidla jednorozmerne, s minimalni kapacitou    *
C             M*N prvku, do ktereho bude po radcich ulozena prectena  *
C             matice                                                  *
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
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
