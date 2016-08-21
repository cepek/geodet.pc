C+
C NGPTXP
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Pomocna procedura pro porovnani vypocteneho a daneho uhlu    *
C AUTOR: Jaroslava Horejcova, VUGTK, nyni Geodezie, s. p., Praha,     *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE NGPTXP (Y,X,YP,XP,U,DIF,D,IER1)
	DOUBLE PRECISION Y(2),X(2),YP,XP,U,DIF,D(2)
	INTEGER          IER1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: NGPTXP slouzi proceduram NGPTXA a NGPTXR k vypoctu uhlu ze  * 
C    souradnic 3 bodu a k jeho porovnani s danym urcujicim uhlem (viz *
C    textovou cast dokumentace procedury NGPTXA, odstavec 4.3.).      *
C PREHLED PARAMETRU - viz textovou cast dokumentace procedury NGPTXA, *
C                     odstavec 4.3.3.                                 *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C EXTERNI PROCEDURA: NGSMDA                                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	DOUBLE PRECISION SM(2),Y2(2),X2(2),PI
C	
	PI=DATAN2(0D0,-1D0)
	Y2(1)=YP
	X2(1)=XP
	Y2(2)=Y(1)
	X2(2)=X(1)
	CALL NGSMDA (Y2,X2,SM(1),D(1),IER1)
	IF (IER1.NE.0.OR.D(1).LT.1D0) GO TO 2
	Y2(2)=Y(2)
	X2(2)=X(2)
	CALL NGSMDA (Y2,X2,SM(2),D(2),IER1)
	IF (IER1.NE.0.OR.D(2).LT.1D0) GO TO 2
	DIF=SM(2)-SM(1)
	IF (DIF.LT.0D0) DIF=DIF+2D0*PI
	DIF=DIF-U
	IF(DABS(DIF).LT.PI/2D0) RETURN
	IER1=4
	RETURN
2	IER1=2
	RETURN
	END
