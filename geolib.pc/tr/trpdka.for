C+
C TRPDKA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet klice rovinne podobnostni transformace *
C AUTOR: Frantisek Charamza, VUGTK,                                   *
C        Eliska Valkova, VUGTK, nyni RIS hl. mesta Prahy              *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
        SUBROUTINE TRPDKA (YXZSTA,YXZNOV,N1,N,IER,A,YXZ0,ODCH,M0,
     /                     INDEX,M0RED)
        INTEGER N,N1,IER,INDEX
        REAL M0,M0RED
	DIMENSION YXZSTA(N1,2),YXZNOV(N1,2),A(2,2),YXZ0(2),ODCH(N1,2)
        DOUBLE PRECISION YXZSTA,YXZNOV,A,YXZ0,ODCH
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: viz zdrojovy text procedury trpdk1                          *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY: viz trpdk1                                       *
C VYSTUPNI PARAMETRY: viz trpdk1                                      *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C EXTERNI PROCEDURA: TRLINA                                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
        DOUBLE PRECISION YT,XT,YYT,XXT,SUMA1,SUMA2,SUMAS2,DY,DX,DYY,
     /                   DXX,DELTA,SUMAVV,AN1,V2,F,DELTAI
C
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
        IF (SUMAS2.LT.1D-10) RETURN
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
    4   CALL TRLINA (YXZ0,A,YXZSTA,ODCH,N1,2,N,IIR)
	DELTA=0
        INDEX=1
	SUMAVV=0
        AN1=DBLE(N-1.)/N
	DO 3 I=1,N
	ODCH(I,1)=ODCH(I,1)-YXZNOV(I,1)
        ODCH(I,2)=ODCH(I,2)-YXZNOV(I,2)
	V2=ODCH(I,1)*ODCH(I,1)+ODCH(I,2)*ODCH(I,2)
	SUMAVV=SUMAVV+V2
        F=AN1-((YXZSTA(I,1)-YT)**2+(YXZSTA(I,2)-XT)**2)/SUMAS2
        IF (F.LE.1D-12) GO TO 3
	DELTAI=V2/F
	IF(DELTAI.LE.DELTA) GO TO 3
	DELTA=DELTAI
	INDEX=I
    3   CONTINUE
        M0=SQRT(SNGL(SUMAVV)/(2*N-4))
	IF (N.LE.3) GO TO 5
        M0RED=SQRT(AMAX1(SNGL(SUMAVV-DELTA),0.)/(2*N-6))
	RETURN
    5   M0RED=0
	END
