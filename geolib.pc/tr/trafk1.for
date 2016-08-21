C+
C TRAFK1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet klice afinni transformace              *
C AUTOR: Frantisek Charamza, VUGTK,                                   *
C        Eliska Valkova, VUGTK, nyni RIS hl. mesta Prahy              *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
        SUBROUTINE TRAFK1 (YXZSTA,YXZNOV,N1,N,DIM,IER,A,YXZ0,ODCH,
     /                     M0,INDEX,M0RED,AA)
        INTEGER   N,N1,IER,DIM,INDEX
        DIMENSION YXZSTA(N1,DIM),YXZNOV(N1,DIM),A(DIM,DIM),
     /            YXZ0(DIM),AA(1),ODCH(N1,DIM)
	REAL      YXZSTA,YXZNOV,A,YXZ0,ODCH,M0,M0RED,AA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura slouzi k vypoctu klice rovinne nebo prostorove    *
C    afinni transformace. Klic se pocita vyrovnanim podle metody nej- *
C    mensich ctvercu z pravouhlych souradnic volitelneho poctu iden-  *
C    tickych bodu danych ve dvou souradnicovych soustavach. Minimalni *
C    pocet bodu klice je tri pro rovinnou transformaci a ctyri pro    *
C    transformaci v prostoru. Vedle prvku transformacniho klice poci- *
C    ta procedura opravy souradnic z vyrovnani a stredni souradnico-  *
C    vou chybu klice M0. Dale identifikuje bod klice, jehoz pripadne  *
C    vypusteni by vedlo k maximalnimu poklesu souctu ctvercu oprav a  *
C    tedy k nejmensi hodnote chyby M0RED takto redukovaneho klice.    *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    YXZSTA - pole pro ulozeni souradnic identickych bodu v soustave, *
C             ze ktere se transformuje                                *
C    YXZNOV - pole pro ulozeni souradnic identickych bodu v soustave, *
C             do ktere se transformuje                                *
C    N1     - pocet radku (mez prvniho indexu) v polich pro ulozeni   *
C             souradnic identickych bodu                              *
C    N	    - pocet identickych bodu (DIM .LT. N .LE. N1) - klic se   *
C             pocita ze souradnic ulozenych v prvnich N radcich poli  *
C             YXZSTA a YXZNOV                                         *
C    DIM    - dimenze souradnicove soustavy; musi byt DIM=2 pro ro-   *
C             vinnou transformaci a DIM=3 pro transformaci v prostoru *
C VYSTUPNI PARAMETRY:                                                 *
C    A      - pole pro ulozeni transformacni matice                   *
C    YXZ0   - pole pro ulozeni prvku translacniho vektoru             *
C    ODCH   - pole oprav souradnic bodu klice                         *
C    M0     - stredni souradnicova chyba klice                        *
C    INDEX  - index bodu, jehoz vypusteni by vedlo k maximalnimu po-  *
C             klesu souctu ctvercu oprav                              *
C    M0RED  - stredni souradnicova chyba klice, redukovaneho vypuste- *
C             nim bodu s indexem INDEX                                *
C    IER    - IER = 0 regularni uloha                                 *
C             IER = 1 singularni konfigurace bodu klice               *
C             IER =-1 singularita - nedovoleny pocet identickych bodu *
C    AA     - pracovni pole pro interne volanou proceduru MTORT1 s    *
C             minimalni kapacitou (N1+DIM+1)*(2*DIM+1) prvku typu     *
C             REAL                                                    *
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
C EXTERNI PROCEDURA: MTORT1                                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	INTEGER D
C 	
        IER=-1
	D=DIM+1
        IF ((N.LT.D).OR.(N.GT.N1)) RETURN
	L=2*DIM+1
	J=1
	DO 1 I=1,N
	AA(J)=1
	DO 2 K=1,DIM
	AA(J+K)=YXZSTA(I,K)
    2   AA(J+K+DIM)=-YXZNOV(I,K)
    1   J=J+L
	NDL=(N+D)*L
	IDL=1+N*L
	DO 3 I=IDL,NDL
    3   AA(I)=0
	DO 4 I=IDL,NDL,L+1
    4   AA(I)=1
        CALL MTORT1 (AA,N,D,D,DIM,1E-1,1E-4,IER)
	IF (IER.NE.0) RETURN
	SUMAVV=0
	J=1
	DK=0
	INDEX=1
	DO 5 I=1,N
	SUMVVI=0
	SUMAW=1-AA(J)*AA(J)
	DO 6 K=1,DIM
	SUMAW=SUMAW-AA(J+K)*AA(J+K)
	V=AA(J+K+DIM)
	SUMVVI=SUMVVI+V*V
    6   ODCH(I,K)=V
	SUMAVV=SUMAVV+SUMVVI
	IF (SUMAW.LE.1E-5) GO TO 5
	DI=SUMVVI/SUMAW
	IF (DI.LE.DK) GO TO 5
	DK=DI
	INDEX=I
    5   J=J+L
	J=N*L+D
	DO 7 I=1,DIM
	YXZ0(I)=AA(J+I)
	JL=J+L+I
	DO 7 K=1,DIM
	A(I,K)=AA(JL)
    7   JL=JL+L
	M0=0
	IF (N.NE.D) M0=SQRT(SUMAVV/(DIM*(N-D)))
	M0RED=0
	IF (N.LE.(D+1)) RETURN
	M0RED=SQRT (AMAX1(SUMAVV-DK,0.)/(DIM*(N-D-1)))
	END
