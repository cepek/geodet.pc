C+
C MTMNSC
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro nasobeni matice symetrickou matici zprava      *
C AUTOR: Jaroslava Horejcova, VUGTK, nyni Geodezie, s. p., Praha      *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MTMNSC (A,M,N,B,C)
      DOUBLE PRECISION A(1),B(1),C(1)
      INTEGER M,N
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Viz proceduru MTMNS3.                                       *
C PREHLED PARAMETRU - viz proceduru MTMNS3                            *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      L=0
      DO 10 I=1,M*N
      C(I)=0D0
      JE=I-L
      J=JE*(JE-1)/2
      DO 20 K=1,N
      J=J+1
      IF (K.GT.JE) J=J+K-2
   20 C(I)=C(I)+A(K+L)*B(J)
      IF (MOD(I,N).EQ.0) L=L+N
   10 CONTINUE
      END
