C+
C NGSKKL
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro konverzi klicu                                 *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE NGSKKL (KASCII,KBYTE,TYP)
	INTEGER*1 KASCII(10),KBYTE(5),TYP
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura  NGSKKL slouzi ke  vzajemnym  prevodum mezi dvema *
C    tvary (formaty) nejvyse desetiznakovych numerickych klicu, napr. *
C    cisel bodu. Prvni z nich je reprezentovan posloupnosti  10 znaku *
C    v kodu ASCII (znakovy tvar), druhy petici  celych cisel z inter- *
C    valu 0 az 99 (tzv. format GEOGEP/M).  Vzajemny vztah obou forma- *
C    tu je popsan v textove casti dokumentace.                        *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETR:                                                   *
C    TYP    - =1 ... prevod ASCII --> GEOGEP/M                        *
C	      =2 ... prevod GEOGEP/M --> ASCII                        *
C VSTUPNI/VYSTUPNI PARAMETRY:                                         *
C    KASCII - klic ve znakovem tvaru v poli o delce 10 bytu; je-      *
C	      li TYP=1, dojde v poli KASCII k redakci klice: bu-      *
C	      dou vypusteny vsechny znaky krome cislic, klic za-      *
C	      rovnan doprava a podle potreby doplneny uvodni me-      *
C             zery                                                    *
C    KBYTE  - klic ve formatu GEOGEP/M v poli o delce 5 bytu          *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	INTEGER*1 I,I1,I2,ZNAK
	LOGICAL*1 I1L
C
	IF (TYP.EQ.2) GOTO 50
C KONVERZE ASCII --> GEOGEP/M
C I1 ... CITAC ZPRACOVANYCH CISLIC
	I1=10
	DO 10 I=1,10
	   I2=11-I
	   ZNAK=KASCII(I2)
	   IF ((ZNAK.LT.#30).OR.(ZNAK.GT.#39)) GOTO 10
	   KASCII(I1)=ZNAK
	   I1=I1-1
10	CONTINUE
C DOPLNENI MEZER ZLEVA
	IF (I1.EQ.0) GOTO 30
	DO 20 I=1,I1
	   KASCII(I)=#20
20	CONTINUE
30	READ(KASCII,100) KBYTE
	RETURN
C KONVERZE GEOGEP/M --> ASCII
50	WRITE(KASCII,100) KBYTE
C NAHRADA VNITRNICH MEZER NULAMI A UVODNICH NUL MEZERAMI
	I1L=.TRUE.
	DO 60 I=1,10
	   ZNAK=KASCII(I)
	   IF (I1L.AND.(ZNAK.EQ.#30)) KASCII(I)=#20
	   IF (.NOT.I1L.AND.(ZNAK.EQ.#20)) KASCII(I)=#30
	   IF (ZNAK.GT.#30) I1L=.FALSE.
60	CONTINUE
C NULOVEMU KLICI ODPOVIDA RETEZEC MEZER
	RETURN
100	FORMAT (5I2)
	END
