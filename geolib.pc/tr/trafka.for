C+
C TRAFKA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet klice afinni transformace              *
C AUTOR: Frantisek Charamza, VUGTK,                                   *
C        Eliska Valkova, VUGTK, nyni RIS hl. mesta Prahy              *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
        SUBROUTINE TRAFKA (YXZSTA,YXZNOV,N1,N,DIM,IER,A,YXZ0,ODCH,
     /                     M0,INDEX,M0RED,AA)
        INTEGER   N,N1,IER,DIM,INDEX
        DIMENSION YXZSTA(N1,DIM),YXZNOV(N1,DIM),A(DIM,DIM),
     /            YXZ0(DIM),AA(1),ODCH(N1,DIM)
	DOUBLE PRECISION YXZSTA,YXZNOV,A,YXZ0,AA,ODCH
	REAL      M0,M0RED
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Viz zdrojovy text procedury TRAFK1                          *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY: viz TRAFK1                                       *
C VYSTUPNI PARAMETRY: viz TRAFK1 s tim, ze pracovni pole AA pro in-   *
C                     terne volanou proceduru MTORTA musi mit prvky   *
C                     typu DOUBLE PRECISION                           *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C EXTERNI PROCEDURA: MTORTA                                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
        INTEGER D
	DOUBLE PRECISION SUMAVV,DK,SUMVVI,SUMAW,V,DI
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
	CALL MTORTA (AA,N,D,D,DIM,1E-1,1E-5,IER)
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
	IF (SUMAW.LE.1D-12) GO TO 5
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
	IF (N.NE.D) M0=SQRT(SNGL(SUMAVV)/(DIM*(N-D)))
	M0RED=0
	IF (N.LE.(D+1)) RETURN
	M0RED=SQRT (AMAX1(SNGL(SUMAVV-DK),0.)/(DIM*(N-D-1)))
	END
