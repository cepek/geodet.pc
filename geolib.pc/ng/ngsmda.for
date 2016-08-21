C+
C NGSMDA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet smerniku a delky                       *
C AUTOR: Eliska Valkova, VUGTK, nyni RIS hl. m. Prahy                 *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE NGSMDA (Y,X,SM,D,IER)
	DOUBLE PRECISION Y(2),X(2),SM,D
	INTEGER IER
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura vypocte smernik spojnice danych bodu a vzdalenost *
C    obou bodu.                                                       *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    Y,X    - pole pro ulozeni souradnic danych bodu                  *
C VYSTUPNI PARAMETRY:                                                 *
C    SM     - vypocteny smernik vyjadreny v obloukove mire            *
C    D      - vypoctena delka (vzdalenost danych bodu)                *
C    IER    - IER = 0  regularni uloha                                *
C             IER = 1  singularita - dane body jsou totozne           *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	REAL*8 DY,DX
C	
	IER=0
	DY=Y(2)-Y(1)
	DX=X(2)-X(1)
	IF(DY.EQ.0D0.AND.DX.EQ.0D0) GOTO 1
	SM=DATAN2(DY,DX)
	IF(SM.LT.0D0) SM=SM+2D0*DATAN2(0D0,-1D0)
	D=DSQRT(DY*DY+DX*DX)
	RETURN
1       D=0D0
	SM=0D0
	IER=1
	RETURN
	END
