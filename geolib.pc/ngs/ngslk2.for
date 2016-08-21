C+
C NGSLK2
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro presuny polozek ve strankach indexoveho soubo- *
C        ru                                                           *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE NGSLK2 (ZDROJ,PRVNI,POSLED,CIL,INDEXC)
	INTEGER*1 ZDROJ(1),CIL(1)
	INTEGER PRVNI,POSLED,INDEXC
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: NGSLK2 je  pomocna  procedura, slouzici  procedure NGSLK1 k *
C    presunum klicu (k) a ukazatelu  PI a  PD v pracovnich prostorech *
C    PP1 a PPN v oblasti /NGS2W/. Je volana pri  zatridovani klice do *
C    posloupnosti klicu ve  strance a pri  deleni stranek (viz uvodni *
C    text k rade procedur NGS). Kopirovana skupina  klicu a ukazatelu *
C    je definovana indexem prvniho a posledniho presouvaneho prvku ve *
C    zdrojovem poli; misto v cilovem poli urcuje index pro zapis prv- *
C    niho preneseneho prvku.                                          *
C PREHLED PARAMETRU						      *	
C VSTUPNI PARAMETRY:                                                  *
C    ZDROJ  - identifikator zdrojoveho pole                           *
C    PRVNI  - index prvni prenasene polozky v poli ZDROJ              *
C    POSLED - index posledni prenasene polozky v poli ZDROJ           *
C    INDEXC - index polozky v poli CIL, od niz pocinaje budou         *
C 	      ulozeny prenesene udaje                                 *
C VYSTUPNI PARAMETR:                                                  *
C    CIL    - identifikator ciloveho pole                             *
C EXTERNI PROCEDURA: PPRES1                                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	INTEGER PPOL,I1,I2
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C LOKALNI PROMENNE:                                                   *
C    PPOL  - pocet polozek                                            *
C    I1,I2 - indexy v polich ZDROJ a CIL                              *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
	PPOL=POSLED-PRVNI+1
	IF (PPOL.LE.0) RETURN
C PRESUN KLICU KL
	I1=(PRVNI-1)*5+3
	I2=(INDEXC-1)*5+3
	CALL PPRES1(ZDROJ,I1,CIL,I2,5*PPOL)
C PRESUN UKAZATELU PI
	I1=(PRVNI-1)*2+73
	I2=(INDEXC-1)*2+73
	CALL PPRES1(ZDROJ,I1,CIL,I2,2*PPOL)
C PRESUN UKAZATELU PD
	I1=(PRVNI-1)*4+101
	I2=(INDEXC-1)*4+101
	CALL PPRES1(ZDROJ,I1,CIL,I2,4*PPOL)
	RETURN
	END
