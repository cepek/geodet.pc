C+
C MTMNS4
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet dolni trojuhelnikove casti soucinu     *
C        dvou matic                                                   *
C AUTOR: Jaroslava Horejcova, VUGTK, nyni Geodezie, s. p., Praha      *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MTMNS4 (A,M,N,B,C)
      DIMENSION A(1),B(1),C(1)
      INTEGER M,N
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Predpokladejme, ze jsou dany matice A a B typu (M x N).     *
C    Procedura vypocte dolni trojuhelnikovou cast soucinu             *
C                             C = AB',                                *
C    kde B' je matice transponovana k matici B. Procedura se uplatni  *
C    predevsim tehdy, je-li matice C symetricka (napr. pri B=A).      *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    A (resp. B) - pole, zpravidla jednorozmerne, s minimalni kapa-   *
C                  citou M*N prvku, do ktereho musi byt pred vyvola-  *
C                  nim procedury po radcich ulozena matice A (resp.B) *
C    M           - pocet radku matic A a B, stupen matice C           *
C    N           - pocet sloupcu matic A a B                          *
C VYSTUPNI PARAMETR:                                                  *
C    C           - pole, zpravidla jednorozmerne, s minimalni kapaci- *
C                  tou M*(M+1)/2 prvku, do ktereho procedura ulozi po *
C                  radcich  dolni trojuhelnikovou cast matice C       *
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      K=0
      DO 10 I=1,M*N,N
      DO 10 J=1,I,N
      K=K+1
      C(K)=0.
      DO 10 L=1,N
      IA=I+L-1
      IB=J+L-1
   10 C(K)=C(K)+A(IA)*B(IB)
      END
