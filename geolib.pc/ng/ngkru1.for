C+
C NGKRU1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet parametru kruznice ze souradnic dvou   *
C        bodu a obvodoveho uhlu                                       *
C AUTOR: Jaroslava Horejcova, VUGTK, nyni Geodezie, s. p., Praha,     *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE NGKRU1 (Y,X,ALFA,R,IER)
	DIMENSION Y(3),X(3)
	REAL      ALFA,R
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
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
C EXTERNI PROCEDURA: NGSMD1                                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	SA=SIN(ALFA)
	IF(ABS(SA).GE.0.15) GO TO 1
	IER=-1
	RETURN
1	CALL NGSMD1(Y,X,SM,S12,IER)
	IF(IER.EQ.1) RETURN
	R1=S12/(2.*SA)
	SM=SM-ALFA
	X(3)=X(1)-R1*SIN(SM)
	Y(3)=Y(1)+R1*COS(SM)
	R=ABS(R1)
	END
