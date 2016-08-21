C+
C MTKOVA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet kovariancni matice                     *
C AUTOR: Jaroslava Horejcova, VUGTK, nyni Geodezie, s.p., Praha,      *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE MTKOVA (F,X,N,CX,DIAGCX,JACOBI,Y,M,CY,CYX,WA,WY,IER)
	DOUBLE PRECISION X(1),Y(1),WY(1)
	DIMENSION CX(1),CY(1),CYX(1),WA(1)
	INTEGER N,M,IER
	LOGICAL*1 DIAGCX,JACOBI
	EXTERNAL F
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura vypocte kovariancni matici soustavy nelinearnich  *
C    funkci nahodneho vektoru.                                        *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    F      - procedura, pomoci ktere musi uzivatel definovat analy-  *
C             zovanou soustavu danych funkci - viz textovou cast do-  *
C             kumentace                                               *
C    X      - vektor argumentu                                        *
C    N      - pocet argumentu                                         *
C    CX     - kovariancni matice vektoru argumentu                    *
C    DIAGCX - indikace typu matice CX; musi byt DIAGCX = .TRUE., je-  *
C             li CX diagonalni, jinak DIAGCX = .FALSE.                *
C    JACOBI - indikace pritomnosti Jacobiovy matice A pro zadanou     *
C             soustavu funkci mezi vstupnimi parametry; je-li misto   *
C             definicni procedury F primo zadana Jacobiova matice A   *
C             (v poli WA), musi byt JACOBI = .TRUE.; ma-li byt Jaco-  *
C             biova matice urcena procedurou MTKOVA, musi byt         *
C             JACOBI = .FALSE.                                        *
C    M      - pocet funkci                                            *
C VYSTUPNI PARAMETRY:                                                 *
C    Y      - vektor funkcnich hodnot                                 *
C    CY     - kovariancni matice vektoru funkcnich hodnot             *
C    CYX    - kovariancni matice vyjadrujici statistickou vazbu mezi  *
C             slozkami vektoru Y a X                                  *
C    IER    - parametr, jehoz nenulova hodnota signalizuje singulari- *
C             tu ulohy (selhani procedury F); v pripade JACOBI =      *
C             .TRUE. je vzdy IER = 0                                  *
C PRACOVNI POLE:                                                      *
C    WA     - pole s kapacitou M*N prvku REAL*4 pro ulozeni Jacobiovy *
C             matice; je-li JACOBI = .TRUE., musi byt pred vyvolanim  *
C             procedury do pole WA po radcich ulozena Jacobiova mati- *
C             ce A; v opacnem pripade naplni pole WA procedura MTKOVA *
C    WY     - pole s kapacitou M prvku REAL*8; pole WY se neuziva pri *
C             JACOBI = .TRUE.                                         *
C ARITMETIKA SMISENA                                                  *
C EXTERNI PROCEDURY: - definicni procedura, kterou uzivatel konkreti- *
C                      zuje analyzovanou soustavu funkci              *
C                    - MTMNS3, MTMNS4                                 *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
        DOUBLE PRECISION A
C	
	IER=0
	IF (JACOBI) GO TO 35
	J=0
	DO 30 I=1,N
	IF (DIAGCX) J=0
	J=J+I
	A=SQRT (CX(J))
	IF (A.EQ.0.) GO TO 15
	X(I)=X(I)+A
	CALL F(X,Y,IER)
	IF (IER.NE.0) RETURN
	X(I)=X(I)-2.*A
	CALL F(X,WY,IER)
	IF (IER.NE.0) RETURN
	DO 10 L=1,M
10	WA((L-1)*N+I)=0.5*(Y(L)-WY(L))/A
	X(I)=X(I)+A
	GO TO 30
15	DO 20 L=0,M-1
20	WA(L*N+I)=0.
30	CONTINUE
	CALL F(X,Y,IER)
	GO TO 45
35	DO 40 I=1,M
40	Y(I)=0.
45	IF (DIAGCX) GO TO 55
	CALL MTMNS3 (WA,M,N,CX,CYX)
	GO TO 65
55	DO 60 I=1,M
	K=(I-1)*N
	DO 60 J=1,N
	L=K+J
60	CYX(L)=WA(L)*CX(J)
65	CALL MTMNS4 (CYX,M,N,WA,CY)
	RETURN
	END
