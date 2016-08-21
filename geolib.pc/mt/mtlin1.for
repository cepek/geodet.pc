C+
C MTLIN1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro reseni soustavy linearnich algebraickych rov-  *
C        nic se symetrickou pozitivne definitni matici                *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MTLIN1 (A,B,N)
      DIMENSION A(1),B(1)
      INTEGER N
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura umoznuje najit vektor reseni soustavy linearnich  *
C    rovnic                                                           *
C                          A x = b                                    *
C    se symetrickou pozitivne definitni matici A a vektorem pravych   *
C    stran b. Uziti procedury predpoklada, ze matice soustavy A byla  *
C    Choleskyho metodou predem rozlozena na soucin dvou matic - regu- *
C    larni dolni trojuhelnikove matice L a matice k ni transponovane, *
C    viz proceduru MTDEC1.                                            *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    A      - jednorozmerne pole pro ulozeni koeficientu dane sou-    *
C             stavy rovnic; pred vstupem do procedury musi byt do po- *
C             le A ulozena dolni trojuhelnikova cast matice L, napr.  *
C             pomoci procedury MTDEC1                                 *
C    N      - pocet rovnic dane soustavy                              *
C VSTUPNI/VYSTUPNI PARAMETRY:                                         *
C    B      - pole,zpravidla jednorozmerne; pred vyvolanim procedury  *
C             musi byt do pole B ulozen vektor pravych stran b; pri   *
C             vystupu z procedury bude ve stejnem prostoru ulozen     *
C             vektor reseni x                                         *
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
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
