C+
C MTVZP1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vyrovnani zprostredkujicich pozorovani         *
C AUTOR: Jaroslava Horejcova, VUGTK, nyni Geodezie, s. p., Praha,     *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE MTVZP1 (A,M,N,P,TOLMIN,X,V,QXX,M0,M0RED,W,INDEX,IER)
C
C
        INTEGER   M,N,INDEX,IER
	DIMENSION A(1),P(M),X(N),V(M),QXX(1)
	REAL      TOLMIN,M0,M0RED,W
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura najde vektor reseni a vektor oprav pro soustavu   *
C    rovnic popisujici vyrovnani zprostredkujicich pozorovani. Nume-  *
C    ricke reseni je zalozeno na Gramove - Schmidtove ortogonalizaci  *
C    a je realizovano procedurou MTORT1. Vedle vektoru neznamych a    *
C    oprav urcuje procedura MTVZP1 odhad jednotkove stredni chyby,    *
C    dolni trojuhelnikovou cast matice vahovych koeficientu nezna-    *
C    mych a nektere dalsi veliciny.                                   *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    TOLMIN - tolerance pro identifikaci singularity ulohy (linearni  *
C             zavislosti sloupcu matice soustavy rovnic oprav)        *
C    M      - pocet rovnic oprav                                      *
C    N      - pocet neznamych                                         *
C    P      - pole pro ulozeni vah pozorovanych hodnot                *
C VYSTUPNI PARAMETRY:                                                 *
C    X      - vektor neznamych                                        *
C    V      - vektor oprav                                            *
C    QXX    - pole s minimalni kapacitou N(N+1)/2 prvku typu REAL, do *
C             ktereho procedura po radcich ulozi dolni trojuhelniko-  *
C             vou cast matice vahovych koeficientu neznamych          *
C    M0     - jednotkova stredni chyba                                *
C    M0RED  - minimalni hodnota jednotkove stredni chyby, ktere lze   *
C             dosahnout vypustenim jedne rovnice oprav                *
C    W      - v absolutni hodnote nejvetsi podil opravy a odmocniny z *
C             jejiho vahoveho koeficientu; index korespondujici opra- *
C             vy je ulozen v parametru INDEX                          *
C    INDEX  - index rovnice oprav, odpovidajici "podezrelemu" pozoro- *
C             vani; indikuje rovnici, jejiz vypusteni vede k M0RED    *
C    IER    - IER = 0  regularni uloha                                *
C             IER = 1  singularni uloha, nemajici jednoznacne reseni  *
C                      (linearne zavisle sloupce matice soustavy rov- *
C                      nic oprav)                                     *
C             IER =-1  singularita - nektera slozka pole P (vaha po-  *
C                      zorovani) nema kladnou hodnotu                 *
C VSTUPNI/VYSTUPNI PARAMETRY:                                         *
C    A      - pracovni pole s minimalni kapacitou (M+N)(N+1) prvku    *
C             typu REAL; pred vyvolanim procedury musi byt do pole A  *
C             po radcich ulozena blokova matice, tvorena matici       *
C             koeficientu rovnic oprav a vektorem absolutnich clenu;  *
C             pri vystupu z procedury je v pracovnim poli zapsana     *
C             ortogonalizaci pretvorena matice - viz textovou cast    *
C             dokumentace                                             *
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
C EXTERNI PROCEDURA: MTORT1                                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	IF(M.LT.N) GO TO 80
C
C  VAHOVA UPRAVA
	L=N+1
	K=0
	DO 10 I=1,M
	   IF(P(I).LE.0.) GO TO 81
	   POM=SQRT(P(I))
	   DO 10 J=1,L
	      K=K+1
	      A(K)=A(K)*POM
10	CONTINUE
C
C  LEMOVANI MATICE A
	K=(M+N)*L
	J=M*L+1
	DO 20 I=J,K
	   A(I)=0.
20	CONTINUE
	DO 30 I=J,K,L+1
	   A(I)=1.
30	CONTINUE
C
C  ORTOGONALIZACE
	CALL MTORT1 (A,M,N,N,1,1E-1,TOLMIN,IER)
	IF(IER.NE.0) RETURN
C
C  PRESUN NEZNAMYCH DO POLE X
	J=J+N
	IP=1
	DO 40 I=J,K,L
	   X(IP)=A(I)
	   IP=IP+1
40	CONTINUE
C
C  VYPOCET VAHOVYCH KOEFICIENTU
	LPOM=J-N
	KP=0
	IP=-1
	DO 50 I=LPOM,K,L
	   IP=IP+1
	   DO 50 J=LPOM,I,L
	      KP=KP+1
	      QXX(KP)=0.
	      DO 50 IN=IP,N-1
	         I1=I+IN
	         I2=J+IN
	         QXX(KP)=QXX(KP)+A(I1)*A(I2)
50	CONTINUE
C
C  OPRAVY, [PVV], DELTA, INDEX
	SUMPVV=0.
	J=0
	DMAX=0.
	INDEX=1
	DO 70 I=1,M
	   POM=SQRT(P(I))
	   J=J+1
	   SUMAW=1.
	   DO 60 K=J,J+N-1
	      SUMAW=SUMAW-A(K)*A(K)
60	   CONTINUE
	   J=J+N
	   VP=A(J)
	   V(I)=VP/POM
	   SUMPVV=SUMPVV+VP*VP
	   IF(SUMAW.LE.1E-5) GO TO 70
	   DELTA=VP*VP/SUMAW
	   IF(DELTA.LE.DMAX) GO TO 70
	   DMAX=DELTA
	   INDEX=I
70	CONTINUE
C
C STREDNI CHYBY M0,M0RED
	M0RED=0.
	IF(M-N-1)71,73,72
71	M0=0.
	W=0.
	INDEX=1
	RETURN
72	M0RED=SQRT(AMAX1(SUMPVV-DMAX,0.)/(M-N-1))
73	M0=SQRT(SUMPVV/(M-N))
	W=SQRT(DMAX)
	RETURN
80	IER=1
	RETURN
81	IER=-1
	RETURN
	END
