C+
C NGSMD1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet smerniku a delky                       *
C AUTOR: Eliska Valkova, VUGTK, nyni RIS hl. m. Prahy                 *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE NGSMD1 (Y,X,SM,D,IER)
	DIMENSION Y(2),X(2)
	REAL SM,D
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
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      IER=0
      DY=Y(2)-Y(1)
      DX=X(2)-X(1)
      IF ((DY.EQ.0).AND.(DX.EQ.0)) GO TO 1
      SM=ATAN2(DY,DX)
      IF (SM.LT.0)  SM=SM+2*ATAN2(0.,-1.)
      D=SQRT(DY*DY+DX*DX)
      RETURN
    1 D=0
      SM=0
      IER=1
      RETURN
      END
