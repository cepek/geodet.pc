C+
C NGSJDZ
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro tvorbu uplneho jmena souboru                   *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE NGSJDZ (CESTA,JMENO,PRIP,UJMENO)
	CHARACTER CESTA*40,JMENO*8,PRIP*4,UJMENO*40
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: NGSJDZ prijima na vstupu tri znakove retezce, predstavujici *
C    v souladu s konvencemi operacniho systemu oznaceni disku a adre- *
C    sare (retezec CESTA), jmeno  souboru  (retezec JMENO) a  priponu *
C    jmena souboru (retezec PRIP), vytvari z nich uplne jmeno souboru *
C    a predava je volajicimu programu  prostrednictvim vystupniho pa- *
C    rametru.                                                         *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    CESTA  - oznaceni disku a  adresare podle konvenci MS-DOS, napr. *
C             C:, A:., A:\ALFA\BETA. Poslednim  znakem  muze byt znak *
C             '\'; uvedene priklady lze psat ekvivalentne takto: C:\, *
C             A:.\, A:\ALFA\BETA\. Retezec CESTA muze byt prazdny.    *
C    JMENO  - jmeno souboru; retezec o delce 1 az 8 znaku.            *
C    PRIP   - retezec tvoreny znakem '.' v 1. pozici a vlastni pripo- *
C             nou jmena  souboru o  delce 1  az 3 znaky. Retezec PRIP *
C             muze byt prazdny.                                       *
C VYSTUPNI PARAMETR:                                                  *
C    UJMENO - uplne jmeno  souboru, tvorene  obecne  oznacenim disku, *
C             jmenem adresare, vlastnim jmenem souboru a priponou.    *
C EXTERNI PROCEDURA: PPKOML                                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	INTEGER N,NJ
	CHARACTER POM*40
C
	CALL PPKOML (JMENO,8,NJ)
	UJMENO=JMENO(:NJ)//PRIP
	CALL PPKOML (CESTA,40,N)
	IF (N.GT.0) THEN
	   IF (CESTA(N:N).NE.'\') THEN
	      N=N+1
	      CESTA(N:N)='\'
	   ENDIF
	   POM=CESTA(:N)//UJMENO(:NJ+4)
	   UJMENO=POM
	ENDIF
	END
