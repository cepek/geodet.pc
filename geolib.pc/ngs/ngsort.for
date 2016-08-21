C+
C NGSORT
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro usporadany vyber informaci z datove zakladny   *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE NGSORT (IER)
	INTEGER*1 IER
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: NGSORT je  pomocna  procedura, umoznujici  procedure NGSDZ1 *
C    vyjimat z datove zakladny informacni  vety tak, ze klice v gene- *
C    rovane posloupnosti vet jsou vzestupne setrideny. Procedura pra- *
C    cuje nasledujicim zpusobem: pri prvni aktivaci predava volajici- *
C    mu programu (procedure NGSDZ1) vetu datoveho souboru s nejmensim *
C    klicem, pri dalsich volanich vzdy  vetu s nejblizsim vetsim kli- *
C    cem. Veta je  predavana ve spolecne  oblasti /NGS3W/. Do prvnich *
C    5 bytu oblasti je ukladan klic ve formatu GEOGEP/M (viz dokumen- *
C    taci procedury NGSKKL), do nasledujicich  bytu zbytek vety. Pred *
C    prvnim vyvolanim procedury (ma-li byt  nalezena veta s nejmensim *
C    klicem, je  treba v oblasti  /NGS6W/ anulovat  promennou UROVEN. *
C    Procedura predpoklada, ze prislusna datova  zakladna je otevrena *
C    (a aktivni). Struktura  spolecnych  oblasti  COMMON je popsana v *
C    kap. 3 textove casti dokumentace procedury NGSDZ1.               *
C PARAMETR PROCEDURY (vystupni):                                      *
C    IER - indikace chybovych stavu                                   *
C          =0 ... uspesne provedena operace,                          *
C          =1 ... - konec DZ:  v predchazejicich volanich proce-      *
C                   dury byly predany vsechny vety  registrovane      *
C                   v datovem souboru,                                *
C                 - signalizace prazdne DZ,                           *
C          =2 ... chyba pri cteni z disku                             *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	INTEGER*1 PPI(158),KL(70),VETA(256),ZB6(6),
     1	          ZB31(31),ZB98(98)
	INTEGER   LUNIND,LUNDAT,KOREN,
     1            PI(14),P0,CISSIS,DV,UROVEN,M,R,I,
     2            ZASOB3(5),ZASOB2(5)
        INTEGER*4 CISLOV,PD(14)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C LOKALNI PROMENNE:                                                   *
C    LUNIND - cislo logicke jednotky pro IS                           *
C    LUNDAT - cislo logicke jednotky pro DS                           *
C    KOREN  - cislo stranky v IS,  kde je  registrovana korenova      *
C             stranka                                                 *
C    ZB6,ZB31,ZB98 - casti oblasti COMMON, s nimiz procedura ne-      *
C                    pracuje                                          *
C    DV     - delka vety DS                                           *
C    UROVEN - uroven stranky IS zvetsena o 1 (index pro zasobni-      *
C             ky ZASOB2 a ZASOB3)                                     *
C    R      - index zpracovaneho klice ve strance IS                  *
C    ZASOB2 - zasobnik pro registraci cisel stranek IS                *
C    ZASOB3 - zasobnik pro registraci indexu R                        *
C    PPI    - pracovni prostor pro stranku IS                         *
C       cleneni(EQUIVALENCE):                                         *
C       M     - pocet klicu na strance                                *
C       KL    - prostor pro 14 klicu                                  *
C       PI    - prostor pro 14 ukazatelu v IS                         *
C       PD    - prostor pro 14 cisel vet v DS                         *
C       P0    - ukazatel v IS                                         *
C    VETA   - informacni veta DS                                      *
C    CISSIS - cislo stranky IS                                        *
C    CISLOV - cislo vety DS                                           *
C    I      - pomocna promenna                                        *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C 
C OBLASTI COMMON:
	COMMON /NGS1W/ LUNIND,LUNDAT,KOREN,ZB6,DV,ZB31
	COMMON /NGS3W/ VETA
	COMMON /NGS6W/ UROVEN,R,ZASOB2,ZASOB3
	COMMON /NGS4W/ PPI,ZB98
C
C CLENENI PRACOVNIHO PROSTORU PPI
	EQUIVALENCE (M,PPI(1)),(KL(1),PPI(3)),(PI(1),PPI(73)),
     1	            (PD(1),PPI(101)),(P0,PPI(157))
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	IER=0
	IF (UROVEN.EQ.0) GOTO 50
C
C DRUHE A DALSI VYVOLANI PROCEDURY
10	P0=PI(R)
15	IF (P0.NE.0) GOTO 40
20	IF (R.EQ.M) GOTO 30
	R=R+1
C
C CTENI VETY DS
	CISLOV=PD(R)
	READ (LUNDAT,REC=CISLOV,ERR=80) (VETA(I),I=1,DV)
C NAVRAT PRI ZJISTENI ZRUSENE VETY
	IF (VETA(1).LT.0) GOTO 10
	RETURN
C
C BYLY ZPRACOVANY VSECHNY KLICE NA STRANCE IS
C NAVRAT SMEREM KE KORENI B-STROMU
30	UROVEN=UROVEN-1
	IF (UROVEN.EQ.0) GOTO 70
	CISSIS=ZASOB2(UROVEN)
	R=ZASOB3(UROVEN)
C CTENI STRANKY IS
	READ (LUNIND,REC=CISSIS,ERR=80) PPI
	GOTO 20
C
C CESTA SMEREM K LISTUM B-STROMU
40	ZASOB2(UROVEN)=CISSIS
	ZASOB3(UROVEN)=R
	CISSIS=P0
	UROVEN=UROVEN+1
	GOTO 60
C
C PRVNI VYVOLANI PROCEDURY
50	IF (KOREN.EQ.0) GOTO 70
	UROVEN=1
	CISSIS=KOREN
C
C CTENI STRANKY IS
60	READ (LUNIND,REC=CISSIS,ERR=80) PPI
	R=0
	GOTO 15
C
C DZ VYCERPANA
70	IER=1
	RETURN
C
C CHYBA PRI CTENI Z DISKU
80	IER=2
	RETURN
	END
