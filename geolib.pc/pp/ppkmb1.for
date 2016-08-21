C+
C PPKMB1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro tvorbu kombinaci                               *
C AUTOR: Jaroslava Horejcova, VUGTK, nyni Geodezie, s. p., Praha      *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE PPKMB1 (N,R,KMB)
	INTEGER N,R,KMB(1)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura vytvori kombinaci R-te tridy z N prvnich priroze- *
C    nych cisel a ulozi ji do pole KMB. Prvky kombinace jsou vzestup- *
C    ne setrideny.                                                    *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    N      - pocet prirozenych cisel, ze kterych jsou vytvareny kom- *
C             binace                                                  *
C    R      - trida kombinace                                         *
C VSTUPNI/VYSTUPNI PARAMETR:                                          *
C    KMB    - pole, do ktereho procedura ulozi R vzestupne setride-   *
C             nych prvku kombinace; pri prvnim volani procedury musi  *
C             byt v pozici KMB(1) ulozena 0; pro kazde dalsi volani   *
C             musi zustat v poli KMB zachovany hodnoty generovane     *
C             procedurou PPKMB1 pri bezprostredne predchazejicim vo-  *
C             lani; situaci, kdy neni mozno vytvorit pozadovanou kom- *
C             binaci, tj. plati R > N nebo byly jiz vycerpany vsechny *
C             kombinace, signalizuje vystupni hodnota KMB(1) = 0      *
C ARITMETIKA CELOCISELNA                                              *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	IF (R.GT.N) GO TO 3
	IF (KMB(1).NE.0) GO TO 1
	DO 10 I=1,R
10	KMB(I)=I
	RETURN
1	IF (KMB(R).GE.N) GO TO 2
	KMB(R)=KMB(R)+1
	RETURN
2	DO 20 I=R,2,-1
	IF (KMB(I-1).GE.(N-R+I-1)) GO TO 20
	KMB(I-1)=KMB(I-1)+1
	DO 15 J=I,R
15	KMB(J)=KMB(I-1)+J-I+1
	RETURN
20	CONTINUE
3	KMB(1)=0
	END



