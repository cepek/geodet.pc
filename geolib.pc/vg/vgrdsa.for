C+
C VGRDSA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet zmeny smeru na primou spojnici v rovi- *
C        ne S-JTSK                                                    *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE VGRDSA (Y,X,DELTA,IER)
	INTEGER          IER
	DOUBLE PRECISION Y(2),X(2),DELTA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Pro dva body dane pravouhlymi souradnicemi (Y1, X1) a (Y2,  *
C    X2) v rovine S-JTSK pocita procedura VGRDSA zmenu smeru (smero-  *
C    vou korekci),tj. uhel, ktery v bode (Y1, X1) svira prima spojni- *
C    ce obou bodu s obrazem hlavni kruznice, spojujici odpovidajici   *
C    body na Gaussove kouli. Znamenko korekce je voleno obvyklym zpu- *
C    sobem, tedy tak, ze pri prevodu smeru do roviny je hodnota ko-   *
C    rekce ke smeru na kouli algebraicky pricitana. Procedury lze     *
C    uzit i k priblizne redukci smeru z Besselova elipsoidu (viz tex- *
C    tovou cast dokumentace).                                         *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    Y,X    - pole pro ulozeni souradnic; pred vyvolanim procedury    *
C             musi byt do odpovidajicich pozic ulozeny dane souradni- *
C             ce (Y1, X1), (Y2, X2)                                   *
C VYSTUPNI PARAMETRY:                                                 *
C    DELTA  - vypoctena smerova korekce v obloukove mire              *
C    IER    - IER = 0  regularni uloha                                *
C             IER = 1  chyba v datech; souradnice Y,resp. X nektere-  *
C                      ho ze zadanych bodu lezi mimo uzemi CSSR       *
C             IER =-1  chyba v datech; zadane body jsou totozne       *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C EXTERNI PROCEDURA: NGSMDA                                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      DOUBLE PRECISION RO(2),EPSIL(2),SIGMA,D,ROM,EPSIM,S(2)
      DOUBLE PRECISION K(2),RO12,N,C1,C2,PI4
C KONSTANTY:
      DATA N,C1,C2,PI4/0.979924704620829603D0,0.123102301278159203D8,
     1                 0.172470158919960996D-2,0.785398163397448310D0/
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IER=1
      DO 1 I=1,2
      IF(Y(I).LT.150000D0.OR.Y(I).GT.950000D0) RETURN
      IF(X(I).LT.900000D0.OR.X(I).GT.1350000D0) RETURN
1     CONTINUE
      CALL NGSMDA (Y,X,SIGMA,D,IER)
      IER=-IER
      IF(IER.NE.0) RETURN
      DO 2 I=1,2
      RO(I)=DSQRT(Y(I)**2+X(I)**2)
      EPSIL(I)=DATAN(Y(I)/X(I))
      S(I)=2D0*(DATAN((C1/RO(I))**(1D0/N))-PI4)
      K(I)=(N-DSIN(S(I)))/(6D0*N)
2     CONTINUE
      ROM=(RO(1)+RO(2))/2D0
      EPSIM=(EPSIL(1)+EPSIL(2))/2D0
      RO12=RO(2)/RO(1)
      DELTA=DSIN(EPSIL(2)-EPSIL(1))*(2D0*K(1)*RO12+K(2)/RO12)+C2*
     1(D/ROM)**3*DSIN(3D0*(SIGMA-EPSIM))
      RETURN
      END
