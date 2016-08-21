C+
C NGSRKL
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Funkcni procedura pro relaci dvou klicu ve formatu GEOGEP/M  *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	INTEGER FUNCTION NGSRKL (KLIC1,KLIC2)
	INTEGER*1 KLIC1(5),KLIC2(5)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: NGSRKL vraci jednu z celociselnych hodnot -1, 0, 1 v zavis- *
C    losti na vysledku porovnani dvou zadanych klicu KLIC1 a KLIC2 ve *
C    formatu GEOGEP/M (viz dokumentaci procedury NGSKKL). Plati       *
C                 NGSRKL = -1, kdyz 'KLIC1' < 'KLIC2'                 *
C                 NGSRKL =  0, kdyz 'KLIC1' = 'KLIC2'                 *
C                 NGSRKL = +1, kdyz 'KLIC1' > 'KLIC2'.                *
C PREHLED PARAMETRU (oba parametry jsou vstupni)                      *
C    KLIC1  - prvni klic v relaci                                     *
C    KLIC2  - druhy klic v relaci                                     *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	INTEGER*1 I
C
	DO 10 I=1,5
	   IF (KLIC1(I)-KLIC2(I)) 20,10,30
10	CONTINUE
	NGSRKL=0
	RETURN
20	NGSRKL=-1
	RETURN
30	NGSRKL=1
	RETURN
	END
