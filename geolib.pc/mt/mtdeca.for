C+
C MTDECA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro rozklad pozitivne (semi)definitni matice na    *
C        soucin dvou vzajemne transponovanych trojuhelnikovych matic  *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE MTDECA (A,N,TOL,IDF)
        DOUBLE PRECISION A(1)
	INTEGER   N, IDF
	REAL      TOL
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura realizuje Choleskyho rozklad symetricke pozitivne *
C    definitni nebo semidefinitni matice A na soucin dolni trojuhel-  *
C    nikove matice L s matici k ni transponovanou. Procedura operuje  *
C    na dolni trojuhelnikove casti matice A, ulozene po radcich v     *
C    jednorozmernem poli, kde prvky matice A postupne nahrazuje ko-   *
C    respondujicimi prvky matice L.                                   *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    N      - stupen matice A                                         *
C    TOL    - tolerance pro identifikaci semidefinitni matice - viz   *
C             textovou cast dokumentace                               *
C VYSTUPNI PARAMETR:                                                  *
C    IDF    - defekt matice A                                         *
C VSTUPNI/VYSTUPNI PARAMETR:                                          *
C    A      - operacni pole s kapacitou postacujici k ulozeni         *
C             N*(N + 1)/2 prvku typu REAL*8; pred vyvolanim proce-    *
C             dury musi byt v operacnim poli po radcich ulozena dolni *
C             trojuhelnikova cast matice A; pri vystupu z procedury   *
C             bude ve stejnem prostoru analogicky ulozena dolni troj- *
C             uhelnikova cast matice L; neni-li matice pozitivne de-  *
C             finitni nebo semidefinitni, potom vysledek prace proce- *
C             dury neni definovan                                     *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      DOUBLE PRECISION X,DIAG
C     
      IP=0
      IDF=0
      DO 10 I=1,N
      IQ=IP+1
      IR=0
      DO 10 J=1,I
      X=A(IP+1)
      DIAG=A(IP+1)
      IF (IQ.GT.IP) GO TO 25
      DO 20 K=IQ,IP
      IR=IR+1
   20 X=X-A(K)*A(IR)
   25 IR=IR+1
      IP=IP+1
      IF (I-J) 30,40,30
   30 IF (A(IR)) 60,50,60
   50 A(IP)=0.
      GO TO 10
   60 A(IP)=X/A(IR)
      GO TO 10
   40 IF (X.GT.(DIAG*TOL)) GO TO 70
      A(IP)=0.
      IDF=IDF+1
      GO TO 10
   70 A(IP)=DSQRT(X)
   10 CONTINUE
      RETURN
      END
