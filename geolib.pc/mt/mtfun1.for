C+
C MTFUN1  
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro polynomickou aproximaci funkce podle metody    *
C        nejmensich ctvercu                                           *
C AUTOR: Frantisek Charamza, VUGTK,                                   *
C        Eliska Valkova, VUGTK, nyni RIS hl. m. Prahy                 *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
        SUBROUTINE MTFUN1 (X,Y,M,K,A,QA,V,QL,DELVV,M0,F,IER)
	DIMENSION X(1),Y(1),A(1),QA(1),V(1),QL(1),DELVV(1)
	INTEGER M,K,IER
	REAL    M0,F
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura slouzi k polynomicke aproximaci funkce, dane nej- *
C    vyse 200 hodnotami v diskretnich bodech, podle metody nejmensich *
C    ctvercu s adaptivni volbou stupne polynomu.                      *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    X      - pole, zpravidla jednorozmerne, s prvky typu REAL pro    *
C             ulozeni hodnot argumentu                                *
C    Y      - pole s analogickymi vlastnostmi jako pole X; slouzi k   *
C             ulozeni funkcnich hodnot                                *
C    M      - pocet funkcnich hodnot                                  *
C    F      - kriticka hodnota, ridici adaptaci stupne polynomu - viz *
C             textovou cast dokumentace                               *
C VYSTUPNI PARAMETRY:                                                 *
C    A (resp. QA) - pole s analogickymi vlastnostmi jako pole X;      *
C                   slouzi k ulozeni vypoctenych soucinitelu aproxi-  *
C                   macniho polynomu (resp. vahovych koeficientu      *
C                   techto soucinitelu)                               *
C    V (resp. QL) - pole s analogickymi vlastnostmi jako pole X;      *
C                   slouzi k ulozeni vypoctenych oprav funkcnich hod- *
C                   not (resp. vahovych koeficientu vyrovnanych fun-  *
C                   kcnich hodnot)                                    *
C    DELVV        - pole s analogickymi vlastnostmi jako pole X;      *
C                   jeho prvky vyjadruji pokles souctu ctvercu oprav  *
C                   pri postupnem zvysovani stupne aproximacniho po-  *
C                   lynomu                                            *
C    M0           - odhad jednotkove stredni chyby                    *
C    IER          - IER = 0  regularni vystup                         *
C                   IER = 1  singularita - nevhodne zvoleny stupen    *
C                            aproximacniho polynomu, nebo vsechny ko- *
C                            eficienty aproximacniho polynomu jsou    *
C                            statisticky rovny nule                   *
C VSTUPNI/VYSTUPNI PARAMETR:                                          *
C    K      - stupen aproximacniho polynomu definovany uzivatelem     *
C             (pri vstupu do procedury), resp. adaptovany stupen (pri *
C             vystupu z procedury)                                    *   
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	DIMENSION P(400),C(42),B(20),ALFA(20),BETA(20)
	INTEGER P0,P1,C0,C1
C	
	IER=1
	M1=M
	F1=F
	IF (M1.LE.(K+1)) RETURN
	DO 1 I=1,M1
	P(I)=0
	QL(I)=0
	P(M1+I)=1
    1   V(I)=(-Y(I))
	DO 2 I=1,K+1
	C(I+1)=0
	A(I)=0
	QA(I)=0
    2   C(K+3+I)=0
	C(1)=0
	C(K+3)=0
	C(K+4)=1
	SP2=1        
C (P(I),P(I))
	SP2MIN=-1    
C (P(I-1),P(I-1))
	SXP2=0       
C (XP(I),P(I))
C PRIPRAVA VYCHOZICH HODNOT
C INDEXOVYCH UKAZATELU V POLICH P A C
	P1=0
	C1=0
	P0=M1
	C0=K+2
C
C URCENI POLYNOMU K-TEHO STUPNE- PRIMY CHOD
C 
	DO 3 I=1,K+1
	ALF=SXP2/SP2
	ALFA(I)=ALF
	BET=SP2/SP2MIN
	BETA(I)=BET
	SP2MIN=SP2
	SP2=0	
	SXP2=0
	SYP=0
	DO 4 J=1,M1
	PJ=(X(J)-ALF)*P(P1+J)-BET*P(P0+J)
	P(P0+J)=PJ
	P2=PJ*PJ
	SP2=SP2+P2
	SXP2=SXP2+X(J)*P2
    4   SYP=SYP+Y(J)*PJ
	BI=SYP/SP2
	B(I)=BI
	DELVV(I)=BI*BI*SP2
C VYPOCET OPRAV A VAHOVYCH KOEFICIENTU VYROVNANYCH FUNKCNICH HODNOT
	DO 5 J=1,M1
	PJ=P(P0+J)
	V(J)=V(J)+PJ*BI
    5   QL(J)=QL(J)+PJ*PJ/SP2
C VYPOCET KOEFICIENTU APROXIMACNIHO POLYNOMU VE VYJADRENI TYPU (1)
C A VYPOCET ODPOVIDAJICICH VAHOVYCH KOEFICIENTU
	DO 6 J=1,I+1
	CJ=C(C1+J)-ALF*C(C1+J+1)-BET*C(C0+J+1)
	C(C0+J+1)=CJ
	A(J)=A(J)+CJ*BI
    6   QA(J)=QA(J)+CJ*CJ/SP2
C ZAMENA HODNOT INDEXOVYCH UKAZATELU
	P1=P0
	C1=C0
	P0=M1-P0
    3   C0=K+2-C0
C 
C VYPOCET CTVERCE JEDNOTKOVE STREDNI CHYBY
C 
	VV=0
	DO 7 I=1,M1
    7   VV=VV+V(I)*V(I)
	M0=VV/(M1-K-1)
	IF (VV)9,9,8
    9   IER=0
	RETURN
C
C ADAPTACE STUPNE K- ZPETNY CHOD
C 
    8   DO 10 I=K+1,1,-1
	BI=B(I)
	ALF=ALFA(I)
	BET=BETA(I)
	IF (DELVV(I)/M0.GT.F1) GO TO 11
C PRI SPLNENI PODMINKY NENI KOEFICIENT B(I-1) STATISTICKY
C ROVEN NULE A ZPETNY CHOD KONCI
C 
C KOREKCE SOUCTU CTVERCU OPRAV
	VV=VV+DELVV(I)
C KOREKCE KOEFICIENTU APROXIMACNIHO POLYNOMU
C A JEJICH VAHOVYCH KOEFICIENTU
C 
	IF (I.EQ.1) GO TO 20
	DO 12 J=1,I-1
	CJ=C(C1+J+1)
	A(J)=A(J)-CJ*BI
	QA(J)=QA(J)-CJ*CJ/SP2
   12   C(C1+J+1)=(C(C0+J)-ALF*C(C0+J+1)-CJ)/BET
C KOREKCE OPRAV A VAHOVYCH KOEFICIENTU VYROVNANYCH FUNKCNICH HODNOT
   20   DO 13 J=1,M1
	PJ=P(P1+J)
	V(J)=V(J)-PJ*BI
	QL(J)=QL(J)-PJ*PJ/SP2
   13   P(P1+J)=((X(J)-ALF)*P(P0+J)-PJ)/BET
C ZPETNY VYPOCET P(I-2) POMOCI P(I-1) A P(I)
	SP2=SP2MIN
	SP2MIN=SP2MIN/BET
C ZAMENA HODNOT INDEXOVYCH UKAZATELU
	P1=P0
	C1=C0
	P0=M1-P0
   10   C0=K+2-C0
C VSECHNY KOEFICIENTY B(I) JSOU STATISTICKY ROVNY NULE
	RETURN
   11   IER=0
	K=I-1
	M0=SQRT(VV/(M1-K-1))
C JEDNOTKOVA STREDNI CHYBA PO ADAPTACI STUPNE K
	END
