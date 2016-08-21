C+
C NGPTXR
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Pomocna procedura pro vyber jednoho ze dvou reseni           *
C AUTOR: Jaroslava Horejcova, VUGTK, nyni Geodezie, s. p., Praha,     *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE NGPTXR (NSUP,Y,X,IER)
	DOUBLE PRECISION Y(2),X(2)
	INTEGER          NSUP,IER	
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: NGPTXR slouzi procedure NGPTXA k vyberu jednoho ze dvou re- *
C    seni v pripade nejednoznacneho vysledku urciteho typu protinani  *
C    (viz textovou cast dokumentace procedury NGPTXA, odstavec 4.2.). *
C PREHLED PARAMETRU - viz textovou cast dokumentace procedury NGPTXA, *
C                     odstavec 4.2.3.                                 *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C EXTERNI PROCEDURY: NGSMDA, NGPTXP                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	DOUBLE PRECISION SUP2(36),SMP2(20,3),D(2),T1,T2,Y1(2)
        DOUBLE PRECISION X1(2),PI,T(2)
	INTEGER*1        SUP1(36,3),SMP1(20)
C OBLASTI COMMON:
	COMMON/NGPTXW/SMP1,SMP2,SUP1,SUP2
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	PI=DATAN2(0D0,-1D0)
	DO 10 I=1,NSUP
	K=SUP1(I,2)
	Y1(1)=SMP2(K,2)
	X1(1)=SMP2(K,3)
	IF(SUP1(I,1)-1) 1,2,3
1	Y1(2)=Y(1)
	X1(2)=X(1)
	CALL NGSMDA (Y1,X1,D(1),T1,IER)
	IF(IER.NE.0) GO TO 10 
	Y1(2)=Y(2)
	X1(2)=X(2)
	CALL NGSMDA (Y1,X1,D(2),T2,IER)
	IF(IER.NE.0) GO TO 10 
	DO 5 J=1,2
	D(J)=D(J)-SUP2(I)
	IF(DABS(D(J)).GT.PI) D(J)=D(J)-DSIGN(2D0*PI,D(J))
5	CONTINUE
	T1=1D0/T1
	T2=1D0/T2
	GO TO 8 
2	Y1(1)=Y(1)-Y1(1)
	X1(1)=X(1)-X1(1)
	D(1)=DSQRT(Y1(1)**2+X1(1)**2)-SUP2(I)
	Y1(1)=Y1(1)+Y(2)-Y(1)
	X1(1)=X1(1)+X(2)-X(1)
	D(2)=DSQRT(Y1(1)**2+X1(1)**2)-SUP2(I)
	T1=1D0
	T2=1D0
	GO TO 8
3	K=SUP1(I,3)
	Y1(2)=SMP2(K,2)
	X1(2)=SMP2(K,3)
	CALL NGPTXP (Y1,X1,Y(1),X(1),SUP2(I),D(1),T,IER)
	IF(IER.EQ.2) GO TO 10
	T1=2D0/(T(1)+T(2))
	CALL NGPTXP (Y1,X1,Y(2),X(2),SUP2(I),D(2),T,IER)
	IF(IER.EQ.2) GO TO 10
	T2=2D0/(T(1)+T(2))
8	D(1)=DABS(D(1))/T1
	D(2)=DABS(D(2))/T2
	IF(D(1).LE.1.AND.D(2).LE.1) GO TO 10
	IF(DMIN1(D(1),D(2)).EQ.D(1)) GO TO 6 
	IER=2
	D(1)=D(2)/D(1)
	GO TO 7
6	IER=1
	D(1)=D(1)/D(2)
7	IF(D(1).LE.1D-1) RETURN
10	CONTINUE
	IER=-1
	RETURN
	END
