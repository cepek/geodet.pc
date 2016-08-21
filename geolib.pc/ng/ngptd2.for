C+
C NGPTD2
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet souradnic bodu protinanim z delek      *
C AUTOR: Jaroslava Horejcova, VUGTK, nyni Geodezie, s. p., Praha,     *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE NGPTD2 (Y,X,S,IER)
	DIMENSION Y(4),X(4),S(2)
	INTEGER   IER
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura NGPTD2 vypocte souradnice bodu, urcenych vodorov- *
C    nymi vzdalenostmi od dvou danych bodu. V porovnani s procedurou  *
C    NGPTD1 rozeznava procedura NGPTD2 vetsi pocet typu singularity   *
C    a lisi se i jinym zpracovanim uloh s dvojim resenim.             *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETR:                                                   *
C    S      - pole pro ulozeni urcujicich delek                       *
C VYSTUPNI PARAMETR:                                                  *
C    IER    - IER = 0  regularni uloha (dvojznacne reseni)            *
C             IER = 1  singularita - dane body jsou totozne           *
C             IER = 2              - nektera z danych vzdalenosti ne- *
C                                    ni kladna                        *
C             IER =-1              - uloha nema reseni                *
C             IER =-2              - nejiste protnuti                 *
C VSTUPNI/VYSTUPNI PARAMETRY:                                         *
C    Y,X    - pole pro ulozeni souradnic; souradnice danych bodu musi *
C             byt pred vyvolanim procedury ulozeny do pozic Y(1),     *
C             X(1), Y(2), X(2); vypoctene souradnice ulozi procedura  *
C             do promennych Y(3), X(3), Y(4), X(4); pritom bod P3 le- *
C             zi vpravo a bod P4 vlevo od spojnice P1P2 pri pohledu   *
C             z bodu P1 na bod P2                                     *
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	IER=0
	IF (S(1).GT.0..AND.S(2).GT.0.) GO TO 4
	IER=2
	RETURN
4	DY=Y(2)-Y(1)
	DX=X(2)-X(1)
	S12=SQRT(DX*DX+DY*DY)
	IF (S12.NE.0.) GO TO 1
	IER=1
	RETURN
1	S1=S(1)/S12
	S2=S(2)/S12
	F=(1.+(S1+S2)*(S1-S2))/2.
	G=(S1+F)*(S1-F)
	IF (G.GE.0.) GO TO 2
	IER=-1
	RETURN
2	H=SQRT(G)
	IF (H.GE.0.15*S1*S2) GO TO 3
	IER=-2
	RETURN
3	S1=DX*H
	S2=DY*H
	Y(3)=Y(1)+DY*F+S1
	Y(4)=Y(3)-2.*S1
	X(3)=X(1)+DX*F-S2
	X(4)=X(3)+2.*S2
	END
