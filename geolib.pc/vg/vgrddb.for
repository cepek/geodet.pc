C+
C VGRDDB
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro redukci delky do roviny S-JTSK                 *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE VGRDDB (Y,X,SREF,SROV,ITYP,IER)
	INTEGER ITYP,IER
	DOUBLE PRECISION Y(3),X(3),SREF,SROV
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura VGRDDB slouzi k vzajemnym prevodum mezi delkou    *
C    oblouku hlavni kruznice na Gaussove kouli a delkou prime spojni- *
C    ce obrazu jeho koncovych bodu v rovine Krovakova zobrazeni.      *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    Y,X    - pole pro ulozeni souradnic. Pred vyvolanim procedury    *
C             musi byt do pozic Y(1),X(1),Y(2),X(2) ulozeny souradni- *
C             ce (Y1, X1), (Y2, X2) koncovych bodu krivky v rovine    *
C             S-JTSK; pozic Y(3), X(3) uziva procedura jako pracov-   *
C             nich.Pri nizsich pozadavcich na presnost mohou byt pro- *
C             menne Y(1) a Y(2), resp. X(1) a X(2), obsazeny stejnou, *
C             vhodne volenou souradnici                               *
C    ITYP   - definuje typ pozadovaneho prevodu                       *
C             ITYP = 1:  SREF --> SROV                                *
C             ITYP = 2:  SROV --> SREF                                *
C VYSTUPNI PARAMETRY:                                                 *
C    IER    - IER = 0  regularni uloha                                *
C             IER = 1  chyba v datech - souradnice Y, resp. X nekte-  *
C             reho ze zadanych bodu lezi mimo uzemi CSSR              *
C VSTUPNI/VYSTUPNI PARAMETRY:                                         *
C    SREF   - delka oblouku hlavni kruznice na Gaussove kouli,prip.   *
C             delka geodeticke krivky na povrchu Besselova elipsoidu  *
C             (viz textovou cast dokumentace); parametr SREF je       *
C             vstupni pri ITYP = 1 a vystupni pri ITYP = 2            *
C    SROV   - delka prime spojnice v S-JTSK; parametr SROV je vystup- *
C             ni pri ITYP = 1 a vstupni pri ITYP = 2                  *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	DOUBLE PRECISION N,N1,C1,C3,RO,S,Q,P,PI4
C KONSTANTY:
        DATA N,C1,C3,PI4/0.979924704620829603D0,0.123102301278159203D8,
     1                   0.108523705621612709D7,0.785398163397448310D0/
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	IER=1
	DO 1 I=1,2
	IF(Y(I).LT.150000D0.OR.Y(I).GT.950000D0) RETURN
	IF(X(I).LT.900000D0.OR.X(I).GT.1350000D0)RETURN
1	CONTINUE
	IER=0
	Y(3)=(Y(1)+Y(2))/2D0
	X(3)=(X(1)+X(2))/2D0
	N1=1D0/N
	Q=0D0
	DO 2 I=1,3
	RO=DSQRT(Y(I)**2+X(I)**2)
	S =2D0*(DATAN((C1/RO)**N1)-PI4)
	P =DCOS(S)/RO
	Q =Q+P
2	CONTINUE
	Q=(Q+3D0*P)*C3
	IF(ITYP.EQ.1) SROV=SREF/Q
	IF(ITYP.NE.1) SREF=SROV*Q
	RETURN
	END
