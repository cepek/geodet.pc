C+
C TRPDK1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet klice rovinne podobnostni transformace *
C AUTOR: Frantisek Charamza, VUGTK,                                   *
C        Eliska Valkova, VUGTK, nyni RIS hl. mesta Prahy              *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
        SUBROUTINE TRPDK1 (YXZSTA,YXZNOV,N1,N,IER,A,YXZ0,ODCH,
     /                     M0,INDEX,M0RED)
        INTEGER   N1,N,IER,INDEX
        DIMENSION YXZSTA(N1,2),YXZNOV(N1,2),A(2,2),
     /            YXZ0(2),ODCH(N1,2)
	REAL      M0,M0RED
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura slouzi k vypoctu klice podobnostni transformace   *
C    v rovine vyrovnanim podle metody nejmensich ctvercu. Klic se po- *
C    cita z pravouhlych souradnic volitelneho poctu N identickych bo- *
C    du danych ve dvou rovinnych souradnicovych soustavach. Vedle     *
C    prvku transformacniho klice pocita procedura pri N > 2 opravy    *
C    souradnic z vyrovnani a stredni souradnicovou chybu klice M0.    *
C    Dale identifikuje bod klice, jehoz pripadne vypusteni by vedlo k *
C    maximalnimu poklesu souctu ctvercu oprav a tedy k nejmensi hod-  *
C    note chyby M0RED takto redukovaneho klice.                       *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    YXZSTA - pole pro ulozeni souradnic identickych bodu v soustave, *
C             ze ktere se transformuje                                *
C    YXZNOV - pole pro ulozeni souradnic identickych bodu v soustave, *
C             do ktere se transformuje                                *
C    N1     - pocet radku (mez prvniho indexu) v polich pro ulozeni   *
C             souradnic identickych bodu                              *
C    N	    - pocet identickych bodu (2 .LE. N .LE. N1) - klic se     *
C             pocita ze souradnic ulozenych v prvnich N radcich poli  *
C             YXZSTA a YXZNOV                                         *
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
C             IER = 1 singularita - nedovoleny pocet identickych bodu *
C             IER =-1 singularita - vsechny identicke body v sousta-  *
C                     ve, ze ktere se transformuje, jsou totozne      *
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
C EXTERNI PROCEDURA: TRLIN1                                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
        IER=1 	
        IF ((N.LT.2).OR.(N.GT.N1)) RETURN
	IER=-1
	YT=0
	XT=0
	YYT=0
	XXT=0
	DO 1 I=1,N
	YT=YT+YXZSTA(I,1)
	XT=XT+YXZSTA(I,2)
	YYT=YYT+YXZNOV(I,1)
    1   XXT=XXT+YXZNOV(I,2)
	YT=YT/N
	XT=XT/N
	YYT=YYT/N
	XXT=XXT/N
	SUMA1=0
        SUMA2=0
	SUMAS2=0
	DO 2 I=1,N
	DY=YXZSTA(I,1)-YT
	DX=YXZSTA(I,2)-XT
	DYY=YXZNOV(I,1)-YYT
	DXX=YXZNOV(I,2)-XXT
	SUMA1=SUMA1+DY*DYY+DX*DXX
        SUMA2=SUMA2+DX*DYY-DY*DXX
    2   SUMAS2=SUMAS2+DX*DX+DY*DY
        IF (SUMAS2.LT.1E-5) RETURN
	IER=0
	A(1,1)=SUMA1/SUMAS2
	A(1,2)=SUMA2/SUMAS2
	A(2,2)=A(1,1)
	A(2,1)=-A(1,2)
        YXZ0(1)=YYT-A(1,1)*YT-A(1,2)*XT
	YXZ0(2)=XXT-A(1,1)*XT+A(1,2)*YT
	IF(N.NE.2) GO TO 4
	M0=0
	M0RED=0
	RETURN
    4   CALL TRLIN1 (YXZ0,A,YXZSTA,ODCH,N1,2,N,IIR)
	DELTA=0
        INDEX=1
	SUMAVV=0
	AN1=FLOAT(N-1)/N
	DO 3 I=1,N
	ODCH(I,1)=ODCH(I,1)-YXZNOV(I,1)
        ODCH(I,2)=ODCH(I,2)-YXZNOV(I,2)
	V2=ODCH(I,1)*ODCH(I,1)+ODCH(I,2)*ODCH(I,2)
	SUMAVV=SUMAVV+V2
        F=AN1-((YXZSTA(I,1)-YT)**2+(YXZSTA(I,2)-XT)**2)/SUMAS2
        IF (F.LE.1E-5) GO TO 3
	DELTAI=V2/F
	IF(DELTAI.LE.DELTA) GO TO 3
	DELTA=DELTAI
	INDEX=I
    3   CONTINUE
	M0=SQRT(SUMAVV/(2*N-4))
	IF (N.LE.3) GO TO 5
        M0RED=SQRT(AMAX1(SUMAVV-DELTA,0.)/(2*N-6))
	RETURN
    5   M0RED=0
	END
             
