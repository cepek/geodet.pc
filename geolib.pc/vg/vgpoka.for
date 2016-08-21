C+
C VGPOKA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Funkcni procedura pro vypocet polomeru krivosti elipsoidu    *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	DOUBLE PRECISION FUNCTION VGPOKA (FI,ALFA)
	DOUBLE PRECISION FI,ALFA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Pro bod P na povrchu Besselova elipsoidu zadany zemepisnou  *
C    sirkou, pocita funkcni procedura VGPOKA polomer krivosti norma-  *
C    loveho rezu ,jdouciho bodem P pod danym azimutem.                *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    FI     - zemepisna sirka bodu P vyjadrena v obloukove mire       *
C    ALFA   - azimut normaloveho rezu vyjadreny v obloukove mire      *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C POZNAMKA: Jmeno funkcni procedury VGPOKA musi byt ve volajicim pro- *
C           gramu deklarovano jako promenna typu DOUBLE PRECISION     *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	VGPOKA=0.6377397155D7/DSQRT(1D0-0.6674372230614D-2*DSIN(FI)**2)/
     1         (1D0+0.671921879797059459D-2*(DCOS(FI)*DCOS(ALFA))**2)
	RETURN
	END
