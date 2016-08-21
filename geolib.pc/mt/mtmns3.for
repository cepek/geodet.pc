C+
C MTMNS3
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro nasobeni matice symetrickou matici zprava      *
C AUTOR: Jaroslava Horejcova, VUGTK, nyni Geodezie, s. p., Praha      *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MTMNS3 (A,M,N,B,C)
      DIMENSION A(1),B(1),C(1)
      INTEGER M,N
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura vypocte soucin                                    *
C                              C = AB ,                               *
C    kde A je matice typu (M x N) a B je symetricka matice stupne N.  *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    A      - pole, zpravidla jednorozmerne, s minimalni kapacitou    *
C             M*N prvku, do ktereho musi byt pred vyvolanim procedury *
C             po radcich ulozena matice A                             *
C    M      - pocet radku matic A a C                                 *
C    N      - pocet sloupcu matic A a C, stupen matice B              *
C    B      - pole, zpravidla jednorozmerne, s minimalni kapacitou    *
C             N*(N+1)/2 prvku, do ktereho musi byt pred vyvolanim     *
C             procedury po radcich ulozena dolni trojuhelnikova cast  *
C             matice B                                                *
C VYSTUPNI PARAMETR:                                                  *
C    C      - pole, zpravidla jednorozmerne, s minimalni kapacitou    *
C             M*N prvku, do ktereho procedura po radcich ulozi vys-   *
C             lednou matici C                                         *
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      L=0
      DO 10 I=1,M*N
      C(I)=0.
      JE=I-L
      J=JE*(JE-1)/2
      DO 20 K=1,N
      J=J+1
      IF (K.GT.JE) J=J+K-2
   20 C(I)=C(I)+A(K+L)*B(J)
      IF (MOD(I,N).EQ.0) L=L+N
   10 CONTINUE
      END
