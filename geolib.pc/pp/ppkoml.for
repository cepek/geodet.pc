C+
C PPKOML
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura PPKOML pro zarovnani znakoveho retezce doleva      *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE PPKOML (RETEZ,L,N)
	CHARACTER*1 RETEZ(*)
	INTEGER     L,N
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura upravi obsah pozic s  indexy 1 az L vstupniho/vy- *
C    stupniho jednorozmerneho znakoveho pole RETEZ tak, ze            *
C    - vypusti vsechny mezery,                                        *
C    - vznikly retezec o delce N umisti v poli  RETEZ od pozice s in- *
C      dexem 1 pocinaje (zarovna doleva),                             *
C    - do pozic pole RETEZ s indexy N+1 az L ulozi mezery.            *
C    Delku retezce N vraci PPKOML volajicimu programu.                *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETR:                                                   *
C    L      - delka vstupniho  retezce  (index  posledniho prvku pole *
C             RETEZ, ktery se ucastni operace)                        *
C VYSTUPNI PARAMETR:                                                  *
C    N      - pocet znaku retezce po vypusteni mezer                  *
C VSTUPNI/VYSTUPNI PARAMETR:                                          *
C    RETEZ  - pole, v nemz dojde k zarovnani retezce                  *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	INTEGER I  
C
	N=0
	DO 10 I=1,L
	   IF (RETEZ(I).EQ.' ') GOTO 10
	   N=N+1
	   IF (I.GT.N) THEN
	      RETEZ(N)=RETEZ(I)
	      RETEZ(I)=' '
	   ENDIF
10	CONTINUE
	END
