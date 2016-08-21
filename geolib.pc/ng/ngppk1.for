C+
C NGPPK1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet pruseciku orientovane poloprimky a     *
C        kruznice                                                     *
C AUTOR: Jaroslava Horejcova, VUGTK, nyni Geodezie, s. p., Praha,     *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE NGPPK1 (Y,X,SIGMA,R,IER)
	DIMENSION Y(4),X(4)
	REAL      SIGMA,R
	INTEGER IER	
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura vypocte souradnice pruseciku orientovane polo-    *
C    primky dane pocatecnim bodem a smernikem s kruznici danou stre-  *
C    dem a polomerem.                                                 *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    SIGMA  - smernik poloprimky v obloukove mire                     *
C    R      - polomer kruznice                                        *
C VYSTUPNI PARAMETR:                                                  *
C    IER    - IER = 0  regularni uloha (dvojznacne reseni)            *
C             IER = 1  uloha ma jedine reseni                         *
C             IER = 2  singularita - polomer kruznice neni kladny     *
C             IER =-1  singularita - poloprimka miji kruznici         *
C             IER =-2  singularita - nejiste protnuti                 *
C VSTUPNI/VYSTUPNI PARAMETRY:                                         *
C    Y,X    - pole pro ulozeni souradnic; souradnice stredu kruznice  *
C             musi byt pred vyvolanim procedury ulozeny do pozic      *
C             Y(1),X(1), souradnice pocatecniho bodu poloprimky do    *
C             pozic Y(2),X(2); vypoctene souradnice pruseciku ulozi   *
C             procedura do promennych Y(3),X(3),Y(4),X(4)             *
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	IER=0
	IF (R.GT.0.) GO TO 5
	IER=2
	RETURN
5	DX=X(2)-X(1)
	DY=Y(2)-Y(1)
	C=COS(SIGMA)
	S=SIN(SIGMA)
	XPT=DX*C+DY*S
	YPT=DY*C-DX*S
	IF(ABS(YPT).GT.R) GO TO 1
	XQT=SQRT(R*R-YPT*YPT)
	IF (XQT.GE.0.15*R) GO TO 2
	IER=-2
	RETURN
2	IF(XPT.LT.XQT) GO TO 3
1	IER=-1
	RETURN
3	DX=XQT*C
	DY=XQT*S
	X(3)=X(1)+DX-YPT*S
	Y(3)=Y(1)+YPT*C+DY
	IF(XPT.LT.-XQT) GO TO 4
	IER=1
	RETURN
4	X(4)=X(3)-2.*DX
	Y(4)=Y(3)-2.*DY
	END
