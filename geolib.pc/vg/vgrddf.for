C+
C VGRDDF
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro fyzikalni redukci delky merene svetelnym       *
C        dalkomerem                                                   *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE VGRDDF(D,LAMBDA,T0,T,P0N0,P,E)
	DOUBLE PRECISION D
	REAL             LAMBDA,T0,T,P0N0,P,E
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura koriguje vstupni delku merenou svetelnym dalkome- *
C    rem o vliv aktualnich parametru atmosfery, tj. teploty vzduchu,  *
C    tlaku vzduchu a tlaku vodnich par, zjistenych v dobe mereni      *
C    zpravidla jako prumerne hodnoty pro stanovisko dalkomeru a sta-  *
C    novisko odrazneho systemu. Predpoklada se znalost efektivni vl-  *
C    nove delky dalkomeru a zakladnich atmosferickych podminek, pro   *
C    nez byly vyrobcem definovany modulacni vlnove delky, prip. lomo- *
C    va cisla, odpovidajiciho zakladnim atmosferickym podminkam.      *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    LAMBDA - efektivni vlnova delka v mikrometrech                   *
C    T0     - zakladni teplota vzduchu ve stupnich Celsia; na obsahu  *
C             T0 nezalezi pri P0N0 .LT. 0                             *
C    T      - aktualni (prumerna) teplota vzduchu ve stupnich Celsia  *
C    P0N0   - zakladni tlak vzduchu v torrech (P0N0 .GE. 0) nebo lo-  *
C             move cislo pro zakladni atmosfericke podminky, opatrene *
C             znamenkem minus (P0N0 .LT. 0)                           *
C    P      - aktualni (prumerny) tlak vzduchu v torrech              *
C    E      - aktualni (prumerny) tlak vodnich par v torrech          *
C VSTUPNI/VYSTUPNI PARAMETRY:                                         *
C    D      - merena delka, ulozena pred vyvolanim procedury v pro-   *
C             menne D, bude nahrazena delkou po fyzikalni redukci     *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              * 
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	REAL   N,NS,N0,L2,L4
C
	N(T,P,E)=(0.35942*NS*P-15.024*E)/(T+273.16)
	L2=LAMBDA*LAMBDA
	L4=L2*L2
	NS=287.604+4.8864/L2+0.068/L4
	IF(P0N0.LT.0.) N0=-P0N0
	IF(P0N0.GE.0.) N0= N(T0,P0N0,0.)
	D=1E-6*(N0-N(T,P,E))*D + D
	RETURN
	END
