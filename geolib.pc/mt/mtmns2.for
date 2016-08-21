C+
C MTMNS2
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro nasobeni symetrickych matic                    *
C AUTOR: Eliska Valkova, VUGTK, nyni RIS hl. m. Prahy                 *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MTMNS2 (A,N,B,I,J,CIJ)
      DIMENSION A(1),B(1)
      REAL CIJ
      INTEGER N,I,J
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura vypocte prvek CIJ matice                          *
C                            C = AB ,                                 *
C    kde A, B jsou symetricke matice stupne N, jejichz dolni troj-    *
C    uhelnikove casti byly pred vyvolanim procedury po radcich uloze- *
C    ny do operacni pameti.                                           *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    A (resp. B) - jednorozmerne pole, ve kterem musi byt pred vyvo-  *
C                  lanim procedury po radcich ulozena dolni trojuhel- *
C                  nikova cast matice A (resp. B)                     *
C    N           - stupen matic A, B                                  *
C    I,J         - indexy pocitaneho prvku CIJ                        *
C VYSTUPNI PARAMETR:                                                  *
C    CIJ         - hodnota skalarniho soucinu I-teho radku matice A   *
C                  s J-tym sloupcem matice B                          *
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      CIJ=0
      L=(I*(I-1))/2
      M=(J*(J-1))/2
      DO 1 K=1,N
      L=L+1
      M=M+1
      IF (K.GT.I) L=L+K-2
      IF (K.GT.J) M=M+K-2
    1 CIJ=CIJ+A(L)*B(M)
      END
