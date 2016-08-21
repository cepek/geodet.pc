C+
C VGTRKA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro prevod zemepisnych souradnic do S-JTSK a zpet  *
C        a pro vypocet rovinne meridianove konvergence                *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE VGTRKA (FI,LAMBDA,Y,X,C,ITYP,IER)
	IMPLICIT REAL*8 (R)
	INTEGER ITYP,IER
	DOUBLE PRECISION FI,LAMBDA,Y,X,C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura provadi prevod mezi zemepisnymi souradnicemi na   *
C    Besselove elipsoidu a rovinnymi souradnicemi v S-JTSK a rovnez   *
C    pocita hodnotu meridianove konvergence. Uzivatel definuje kon-   *
C    kretni prevod pomoci vstupniho parametru ITYP (viz vstupni para- *
C    metry).                                                          *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    ITYP   - definuje typ pozadovane operace                         *
C             ITYP = 1:  (FI, LAMBDA) --> (Y, X, C)                   *
C             ITYP = 2:  (Y, X)       --> (FI, LAMBDA, C)             *
C             ITYP = 3:  (FI, LAMBDA) --> (C)                         *
C             ITYP = 4:  (Y, X)       --> (C)                         *
C VYSTUPNI PARAMETRY:                                                 *
C    C      - rovinna meridianova konvergence vyjadrena v obloukove   *
C             mire                                                    *
C    IER    - IER = 0  regularni uloha                                *
C             IER = 1  chyba v datech - nektera ze zadanych souradnic *
C                      lezi mimo uzemi CSSR                           *
C             IER =-1  porucha konvergence pri prevodu rovinnych sou- *
C                      radnic na zemepisne - viz textovou cast doku-  *
C                      mentace                                        *
C VSTUPNI/VYSTUPNI PARAMETRY:                                         *
C    FI     - zemepisna sirka na Besselove referencnim elipsoidu vy-  *
C             jadrena v obloukove mire                                *
C    LAMBDA - zemepisna delka na Besselove referencnim elipsoidu      *
C             vztazena k Ferru a vyjadrena v obloukove mire           *
C    Y,X    - rovinne souradnice v S-JTSK                             *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	DOUBLE PRECISION K,E,DELTAV,TAGFI,ALFA,N,C1,DTAN,DASIN,PI4,LQ
C KONSTANTY:
	DATA ALFA,E,K,N/0.100059749837154134D1,0.816968312152558343D-1,
     1                  0.996592486879506453D0,0.979924704620829603D0/
	DATA C1,RSINUQ,RCOSUQ,PI4,LQ/
     1                   0.123102301278159203D8,0.863499969506341460D0,
     2                   0.504348889819881879D0,0.785398163397448310D0,
     3                   0.741764932097590062D0/
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	DTAN(X)=DSIN(X)/DCOS(X)
	DASIN(X)=DATAN(X/DSQRT(1D0-X*X))
	GO TO (1,2,1,2) ITYP
C
C	PREVOD ZEMEPISNYCH SOURADNIC NA KROVAKOVY
C
1	IER=1
	IF((FI.GT.0.89884457D0).OR.(FI.LT.0.82903140D0)) RETURN
	IF ((LAMBDA.LT.0.50614548D0).OR.(LAMBDA.GT.0.71558499D0)) RETURN
	IER=0
	R1=((1D0-(E*DSIN(FI)))/(1D0+(E*DSIN(FI))))**(E/2D0)
	RTAGU=((DTAN(FI/2D0+PI4)*R1)**ALFA)/K
	RU=2D0*(DATAN(RTAGU)-PI4)
	DELTAV=ALFA*(LQ-LAMBDA)
	RSINS=RSINUQ*DSIN(RU)+(RCOSUQ*DCOS(RU)*DCOS(DELTAV))
	RS=DASIN(RSINS)
	RSIND=(DSIN(DELTAV)*DCOS(RU))/DCOS(RS)
	RD=DASIN(RSIND)
	REPSIL=N*RD
C
C	MERIDIANOVA KONVERGENCE
C
	RGAMA=DASIN(DSIN(DELTAV)/DCOS(RS)*RCOSUQ)
	C=REPSIL-RGAMA
	IF(ITYP.EQ.3) RETURN
	RO=C1/(DTAN(RS/2D0+PI4))**N
	Y=RO*DSIN(REPSIL)
	X=RO*DCOS(REPSIL)
	RETURN
C
C	PREVOD KROVAKOVYCH SOURADNIC NA ZEMEPISNE
C
2	IER=1
	IF((Y.GT.950000D0).OR.(Y.LT.150000D0)) RETURN
	IF((X.GT.1350000D0).OR.(X.LT.900000D0)) RETURN
	IER=0
	RO=DSQRT(Y*Y+X*X)
	REPSIL=DATAN(Y/X)
	RD=REPSIL/N
	RTAGS=(C1/RO)**(1D0/N)
	RS=2D0*(DATAN(RTAGS)-PI4)
	RSINU=RSINUQ*DSIN(RS)-(RCOSUQ*DCOS(RS)*DCOS(RD))
	RU=DASIN(RSINU)
	RGAMA=DASIN(DSIN(RD)/DCOS(RU)*RCOSUQ)
	C=REPSIL-RGAMA
	IF(ITYP.EQ.4) RETURN
	RSINV=(DCOS(RS)*DSIN(RD))/DCOS(RU)
	LAMBDA=LQ-DASIN(RSINV)/ALFA
C
C	VYPOCET FI ITERACI
C
	RFI=RU
	RT=(K*DTAN(RU/2D0+PI4))**(1D0/ALFA)
	DO 10 I=1,10
        RT1=(1D0+(E*DSIN(RFI)))/(1D0-(E*DSIN(RFI)))
	RT1=RT*(RT1**(E/2D0))
	FI=2D0*(DATAN(RT1)-PI4)
	IF(DABS(RFI-FI).LE.3.8D-15) RETURN
	RFI=FI
10	CONTINUE
	IER=-1
	RETURN
	END
