C+
C VGRDDA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro matematickou redukci delky (prevod sikme delky *
C        na elipsoid)                                                 *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE VGRDDA (DA,H1,H2,FI,ALFA,S)
	DOUBLE PRECISION DA,H1,H2,FI,ALFA,S
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura prevede delku prime spojnice dvou bodu P1, P2 o   *
C    vyskach H1, H2 nad povrchem Besselova elipsoidu na delku geode-  *
C    ticke krivky, spojujici paty normal obou bodu.                   *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    DA     - delka prime spojnice bodu P1 a P2                       *
C    H1,H2  - vyska bodu P1, resp. P2 nad povrchem Besselova elip-    *
C             soidu                                                   *
C    FI     - zemepisna sirka bodu P1 vyjadrena v obloukove mire      *
C             (FI .GT. 0; je-li FI .EQ. 0, dosadi procedura stredni   *
C             zemepisnou sirku pro uzemi CSSR)                        *
C    ALFA   - azimut spojnice bodu P1 a P2 vyjadreny v obloukove mire *
C             (ALFA .GE. 0; je-li ALFA .LT. 0,bude procedura pracovat *
C             se strednim polomerem krivosti - viz textovou cast do-  *
C             kumentace)                                              *
C VYSTUPNI PARAMETRY:                                                 *
C    S      - redukovana delka                                        *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C EXTERNI PROCEDURA: VGPOKA                                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
        DOUBLE PRECISION FIKOP,DASIN,X,VGPOKA,FI0,PIPUL,R
C KONSTANTY:
       	DATA FI0/0.862192650485198802D0/,PIPUL/0.157079632679489662D1/
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	DASIN(X)=DATAN(X/DSQRT(1D0-X*X))
	FIKOP=FI
	IF(FI.EQ.0D0) FIKOP=FI0
	IF(ALFA.GE.0D0) R=VGPOKA(FIKOP,ALFA)
	IF(ALFA.LT.0D0) R=DSQRT(VGPOKA(FIKOP,0D0)*VGPOKA(FIKOP,PIPUL))
	X=DSQRT((DA*DA-(H2-H1)**2)/(R+H1)/(R+H2))/2D0
	S=2D0*R*DASIN(X)
	RETURN
	END
