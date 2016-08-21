C+
C NGKRUA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet parametru kruznice ze souradnic dvou   *
C        bodu a obvodoveho uhlu                                       *
C AUTOR: Jaroslava Horejcova, VUGTK, nyni Geodezie, s. p., Praha,     *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE NGKRUA (Y,X,ALFA,R,IER)
	DOUBLE PRECISION Y(3),X(3),ALFA,R
	INTEGER IER	
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura vypocte parametry kruznice (souradnice stredu a   *
C    velikost polomeru), urcene dvema body a obvodovym uhlem nad te-  *
C    mito body.                                                       *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETR:                                                   *
C    ALFA   - orientovany pravotocivy obvodovy uhel v obloukove mire; *
C             pro pozorovatele hlediciho z vrcholu uhlu smeruje leve  *
C             (resp. prave) rameno uhlu na bod odpovidajici prvnim    *
C             (resp. druhym) pozicim v polich Y a X                   *
C VYSTUPNI PARAMETRY:                                                 *
C    R      - polomer kruznice                                        *
C    IER    - IER = 0  regularni uloha                                *
C             IER = 1  singularita - dane body jsou totozne           *
C             IER =-1  singularita - obvodovy uhel se lisi od celoci- *
C                      selneho nasobku PI o mene nez 10 gradu         *
C VSTUPNI/VYSTUPNI PARAMETRY:                                         *
C    Y,X    - pole pro ulozeni souradnic; souradnice danych bodu mu-  *
C             si byt pred vyvolanim procedury ulozeny do pozic Y(1),  *
C             X(1),Y(2),X(2); vypoctene souradnice stredu kruznice    *
C             ulozi procedura do promennych Y(3),X(3)                 *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C EXTERNI PROCEDURA: NGSMDA                                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	DOUBLE PRECISION R1,SA,SM,S12
C	
	SA=DSIN(ALFA)
	IF(DABS(SA).GE.15D-2) GO TO 1
	IER=-1
	RETURN
1	CALL NGSMDA(Y,X,SM,S12,IER)
	IF(IER.EQ.1) RETURN
	R1=S12/(2D0*SA)
	SM=SM-ALFA
	X(3)=X(1)-R1*DSIN(SM)
	Y(3)=Y(1)+R1*DCOS(SM)
	R=DABS(R1)
	END
