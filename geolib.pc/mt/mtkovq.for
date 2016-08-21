C+
C MTKOVQ
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro tisk vysledku                                  *
C AUTOR: Jaroslava Horejcova, VUGTK, nyni Geodezie, s.p., Praha,      *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE MTKOVQ (TEXT,N,M,X,CX,DIAGCX,Y,CY,CYX,IPAR)
	LOGICAL*1 DIAGCX
	CHARACTER*(*) TEXT
	DOUBLE PRECISION X(1),Y(1)
	DIMENSION CX(1),CY(1),CYX(1)
	INTEGER N,M,IPAR
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: MTKOVQ je pomocna procedura, ktera tiskne volitelnou kombi- *
C    naci nekterych vstupnich a vystupnich parametru procedury        *
C    MTKOVA.                                                          *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    TEXT   - znakovy retezec s nazvem ulohy                          *
C    N      - pocet argumentu                                         *
C    M      - pocet funkci                                            *
C    IPAR   - parametricke peticiferne cislo slozene z nul a jedni-   *
C             cek; jednotlive cifry vyjadruji pozadavek na tisk ne-   *
C             ktere z matic (nektereho z vektoru) X,CX,Y,CY,CYX, viz  *
C             tab 3. v textove casti dokumentace                      *
C    DIAGCX - indikace typu matice CX; musi byt DIAGCX=.TRUE., je-li  *
C             dana matice diagonalni, jinak DIAGCX=.FALSE.            *
C    X      - vektor argumentu                                        *
C    CX     - kovariancni matice                                      *
C    Y      - vektor funkcnich hodnot                                 *
C    CY     - kovariancni matice                                      *
C    CYX    - kovariancni matice                                      *
C EXTERNI PROCEDURY: MTMTSA, MTMTS1, MTMTS2                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
        LOGICAL S
C	
100	FORMAT (1H1)
101	FORMAT (///)
102	FORMAT (/1X,A)
	S=.FALSE.
	WRITE(1,100)
	WRITE(1,102) TEXT
	IPA1=IPAR
	DO 11 I=5,1,-1
	IP=10**(I-1)
	IF (IPA1/IP.EQ.1) GO TO (6,5,4,2,1)I
	GO TO 11
1	CALL MTMTSA ('ARGUMENTY           ',1,N,X,1,S)
	GO TO 10
2	IF (DIAGCX) GO TO 3
	CALL MTMTS2 ('KOVAR. MATICE CX    ',N,CX,1,S)
	GO TO 10
3	CALL MTMTS1 ('KOVAR. MATICE CX    ',1,N,CX,1,S)
	GO TO 10
4	CALL MTMTSA ('FUNKCNI HODNOTY     ',1,M,Y,1,S)
	GO TO 10
5	CALL MTMTS2 ('KOVAR. MATICE CY    ',M,CY,1,S)
	GO TO 10
6	CALL MTMTS1 ('KOVAR. MATICE CYX   ',M,N,CYX,1,S)
10	WRITE(1,101)
  	IPA1=IPA1-IP
11	CONTINUE
	RETURN
	END
