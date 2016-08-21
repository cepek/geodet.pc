C+
C MTLINA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro reseni soustavy linearnich algebraickych rov-  *
C        nic se symetrickou pozitivne definitni matici                *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MTLINA (A,B,N)
      DOUBLE PRECISION A(1),B(1)
      INTEGER N
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Viz proceduru MTLIN1                                        *
C PREHLED PARAMETRU - viz proceduru MTLIN1 s tim, ze zminovanou pro-  *
C                     ceduru MTDEC1 zamenime za MTDECA                *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      DOUBLE PRECISION X
C      
      B(1)=B(1)/A(1)
      IP=2
      IF (N.EQ.1) GO TO 15
      DO 10 I=2,N
      IQ=I-1
      X=B(I)
      DO 20 K=1,IQ
      X=X-A(IP)*B(K)
   20 IP=IP+1
      B(I)=X/A(IP)
   10 IP=IP+1
   15 IP=IP-1
      B(N)=B(N)/A(IP)
      IF (N.EQ.1) RETURN
      DO 30 I=N-1,1,-1
      IP=IP-1
      IS=IP
      IQ=I+1
      X=B(I)
      DO 40 K=N,IQ,-1
      X=X-A(IS)*B(K)
   40 IS=IS-K+1
   30 B(I)=X/A(IS)
      RETURN
      END
