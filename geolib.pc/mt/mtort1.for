C+
C MTORT1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro Gramovu - Schmidtovu ortogonalizaci blokove    *
C        matice                                                       *
C AUTOR: Frantisek Charamza, VUGTK,                                   *
C        Eliska Valkova, VUGTK, nyni RIS hl. m. Prahy                 *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE MTORT1 (A,M1,M2,N1,N2,TOL,TOLMIN,IER)
	REAL      TOL,TOLMIN
	INTEGER   M1,M2,N1,N2,IER
	DIMENSION A(1)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura podrobi vstupni blokovou matici modifikovane Gra- *
C    move - Schmidtove ortogonalizaci, volitelne zpresnovane doorto-  *
C    gonalizaci. Muze byt uzita predevsim pri vyrovnani zprostredku-  *
C    jicich nebo podminkovych pozorovani podle metody nejmensich      *
C    ctvercu k vypoctu oprav, neznamych, hodnot funkci vyrovnanych    *
C    velicin i vsech vahovych koeficientu. Pri aplikaci procedury ne- *
C    ni treba sestavovat a resit soustavy normalnich rovnic, resp.    *
C    invertovat jejich matice.                                        *
C PREHLED PARAMETRU (podrobny popis viz textovou cast dokumentace)    *
C VSTUPNI PARAMETRY:                                                  *
C    M1,M2,N1,N2 - pocty radku a sloupcu submatic pole A              *
C    TOL         - tolerance pro aplikaci doortogonalizace            *
C    TOLMIN      - tolerance pro identifikaci singularity ulohy       *
C VYSTUPNI PARAMETRY:                                                 *
C    IER    - IER = 0  regularni uloha                                *
C             IER = 1  singularni uloha                               *
C VSTUPNI/VYSTUPNI PARAMETR:                                          *
C    A      - pracovni pole pro umisteni ortogonalizovane matice v    *
C             pameti; pred vyvolanim procedury je treba do pole A po  *
C             radcich ulozit vstupni matici, obsahujici mj. koefi-    *
C             cienty a absolutni cleny definicnich rovnic (rovnic     *
C             oprav, podminkovych rovnic); pri vystupu z procedury je *
C             ve stejnem prostoru analogickym zpusobem zapsana vy-    *
C             stupni, ortogonalizaci pretvorena matice                *
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	REAL MAXIM,NORM
	LOGICAL*1 B1
C	
	IER=1
	M=M1+M2
	N=N1+N2
	NM=N*(M-1)
	NM1=N*(M1-1)
	DO 1 I=1,N
	B1=.TRUE.
	MAXIM=0
	NMI=NM+I
	NM1I=NM1+I
	DO 2 J=I,NM1I,N
    2 IF (ABS(A(J)).GT.MAXIM) MAXIM=ABS(A(J))
	IF (MAXIM.EQ.0) MAXIM=1
	DO 3 J=I,NMI,N
    3 A(J)=A(J)/MAXIM
	JMX=MIN0(N1,(I-1))
    4 IF (JMX.EQ.0) GO TO 12
	DO 7 J=1,JMX
	PROD=0
	K1=J
	DO 6 K=I,NM1I,N
	PROD=PROD+A(K)*A(K1)
    6 K1=K1+N
	K1=J
	DO 7 K=I,NMI,N
	A(K)=A(K)-PROD*A(K1)
    7 K1=K1+N
   12 NORM=0
	DO 5 J=I,NM1I,N
    5 NORM=NORM+A(J)*A(J)
	NORM=SQRT(NORM)
D     WRITE (6,10)I,NORM
D  10 FORMAT (6H NORMA,I4,E15.7)
	IF (NORM.GE.TOL) GO TO 8
	IF((NORM.LT.TOLMIN).AND.(I.LE.N1)) RETURN
	B1=.NOT.B1
	IF (.NOT.B1) GO TO 4
    8 IF (I.GT.N1) NORM=1/MAXIM
	DO 9 J=I,NMI,N
    9 A(J)=A(J)/NORM
    1 CONTINUE
	IER=0
	END
