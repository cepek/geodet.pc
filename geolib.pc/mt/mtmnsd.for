C+
C MTMNSD
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet dolni trojuhelnikove casti soucinu     *
C        dvou matic                                                   *
C AUTOR: Jaroslava Horejcova, VUGTK, nyni Geodezie, s. p., Praha      *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MTMNSD (A,M,N,B,C)
      DOUBLE PRECISION A(1),B(1),C(1)
      INTEGER M,N
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Viz proceduru MTMNS4.                                       *
C PREHLED PARAMETRU - viz proceduru MTMNS4                            *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      K=0
      DO 10 I=1,M*N,N
      DO 10 J=1,I,N
      K=K+1
      C(K)=0D0
      DO 10 L=1,N
      IA=I+L-1
      IB=J+L-1
   10 C(K)=C(K)+A(IA)*B(IB)
      END
