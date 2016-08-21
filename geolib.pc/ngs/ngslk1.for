C+
C NGSLK1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro lokalizaci klice v indexovem souboru           *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE NGSLK1 (KLIC,CISLOV,FUNKCE,IER)
	INTEGER*1 KLIC(5),FUNKCE,IER
	INTEGER*4 CISLOV
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Ukolem procedury NGSLK1 je spravovat pro  potreby procedury *
C    NGSDZ1 indexovy soubor datove zakladny. Procedura muze byt vola- *
C    na dvema zpusoby, rozlisenymi hodnotou parametru 'FUNKCE'. Prvni *
C    (FUNKCE=1) slouzi k vyhledani klice v IS, druhy (FUNKCE=2) umoz- *
C    nuje ulozit do IS novy klic.  V obou  pripadech vraci  NGSLK1 ve *
C    vystupnim parametru 'CISLOV' cislo vety v datovem souboru, ktere *
C    prislusi zadanemu  klici. Kladne cislo vety je priznakem existu- *
C    jiciho, tj. v IS jiz  registrovaneho  klice.  Neni-li klic dosud *
C    registrovan, je hodnota parametru 'CISLOV' bud  nulova (pro pri- *
C    pad FUNKCE=1) nebo zaporna  (je-li FUNKCE=2); ve  druhem pripade *
C    byl klic procedurou zanesen  do IS a absolutni hodnota parametru *
C    'CISLOV' udava nejblizsi  neobsazene cislo vety v datovem soubo- *
C    ru, ktere se vyhrazuje pro zapis  odpovidajicich udaju.  Vlastni *
C    zapis zajistuje volajici  program (procedura  NGSDZ1). Procedura *
C    pracuje se dvema oblastmi COMMON;  jejich struktura je popsana v *
C    kap. 3 textove casti dokumentace procedury NGSDZ1.               *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    KLIC   - klic ve formatu  GEOGEP/M  v poli o delce 5 bytu (popis *
C             formatu GEOGEP/M viz dokumentaci procedury NGSKKL)      *
C    FUNKCE - =1 ... hledani klice                                    *
C 	      =2 ... ukladani klice                                   *
C VYSTUPNI PARAMETRY:                                                 *
C    CISLOV - cislo vety v datovem souboru, korespondujici klici; nu- *
C             lova a zaporna hodnota parametru maji specialni vyznam  *
C    IER    - indikace chybovych stavu                                *
C 	      =0 ... uspesne provedena operace                        *
C 	      =1 ... chyba pri cteni z disku                          *
C 	      =2 ... chyba pri zapisu na disk                         *
C	      =4 ... chybi prostor v indexovem souboru                *
C	      =6 ... chybna struktura index. souboru                  *
C EXTERNI PROCEDURY A FUNKCE: NGSLK2, NGSRKL, PPRES1                  *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
 	INTEGER*1 PP1(158),PPN(158),KL(70),KLN(70),PPU(11),
     1            KLICU(5),PPV(11),KLICV(5),ZB33(33)
	INTEGER   LUNIND,LUNDAT,KOREN,CITIND,UROVEN,CISLOS,R,IB,
     1	          ZASOB2(5),PI(14),MN,PIN(14),P0N,
     2	          PIU,PIV,P,P0,INDEXR,ZASOB3(5),M,N,NN
        INTEGER*4 PD(14),PDN(14),PDU,PDV,CITDAT
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C LOKALNI PROMENNE:                                                   *
C    LUNIND - cislo logicke jednotky pro indexovy soubor              *
C    LUNDAT - cislo logicke jednotky pro datovy soubor                *
C    KOREN  - cislo zaznamu (stranky) v IS, kde je registrovana       *
C 	      korenova stranka                                        *
C    CITIND - pocet zaznamu v IS                                      *
C    CITDAT - pocet vet ulozenych v datovem souboru                   *
C    CISLOS - cislo bezne stranky IS                                  *
C    INDEXR - index nejblizsiho mensiho klice ve strance CISLOS       *
C    ZASOB2 - zasobnik pro registraci cisel stranek                   *
C    ZASOB3 - zasobnik pro registraci indexu INDEXR                   *
C    UROVEN - uroven bezne stranky IS zvetsena o 1                    *
C    PP1    - pracovni prostor pro stranku IS                         *
C       cleneni (EQUIVALENCE):                                        *
C 	M     - pocet klicu na strance                                *
C 	KL    - prostor pro 14 klicu                                  *
C 	PI    - prostor pro 14 ukazatelu v IS                         *
C 	PD    - prostor pro 14 cisel vet v DS                         *
C       P0    - ukazatel k mensim klicum                              *
C    PPN    - pracovni prostor pro novou stranku  IS  pri deleni      *
C 	      obsazene stranky                                        *
C       cleneni (EQUIVALENCE):                                        *
C 	MN, KLN, PIN, PDN, P0N - analogicky vyznam jako v PP1         *
C    PPU    - pracovni prostor U pro registraci klice, ukazatele      *
C 	      v IS a cisla vety v DS                                  *
C 	cleneni (EQUIVALENCE):                                        *
C 	KLICU, PIU, PDU                                               *
C    PPV    - pracovni prostor V pro registraci klice, ukazatele      *
C 	      v IS a cisla vety v DS                                  *
C 	cleneni (EQUIVALENCE):                                        *
C       KLICV, PIV, PDV                                               *
C    M      - pocet klicu na strance                                  *
C    P, P0  - ukazatele v IS                                          *
C    R      - citac                                                   *
C    IB     - index                                                   *
C    NN     - max. pocet klicu na strance (NN=14)                     *
C    N      - NN/2 (N=7)                                              *
C    ZB33   - cast /NGS1W/, s niz procedura nepracuje                 *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C 
C OBLASTI COMMON:
	COMMON /NGS1W/ LUNIND,LUNDAT,KOREN,CITIND,CITDAT,ZB33
 	COMMON /NGS2W/ PP1,PPN
C
C CLENENI PRACOVNICH PROSTORU PP1, PPN, PPU A PPV
	EQUIVALENCE (M,PP1(1)),(KL(1),PP1(3)),(PI(1),PP1(73)),
     1	            (PD(1),PP1(101)),(P0,PP1(157)),
     2	            (MN,PPN(1)),(KLN(1),PPN(3)),
     3	            (PIN(1),PPN(73)),(PDN(1),PPN(101)),
     4              (P0N,PPN(157)),
     5	            (KLICU(1),PPU(1)),(PIU,PPU(6)),(PDU,PPU(8)),
     6	            (KLICV(1),PPV(1)),(PIV,PPV(6)),(PDV,PPV(8))
C
C KONSTANTY:
	DATA N,NN /7,14/
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	IER=0
	CISLOV=0
	IF (KOREN.GT.0) GOTO 10
C V IS NEJSOU REGISTROVANY ZADNE KLICE
	IF (FUNKCE.EQ.1) RETURN
	CITDAT=CITDAT+1
	CISLOV=-CITDAT
C PRIPRAVA POLOZKY PRO ULOZENI V IS
	CALL PPRES1(KLIC,1,KLICU,1,5)
	PIU=0
	PDU=CITDAT
C PRECHOD NA TVORBU KORENOVE STRANKY
	GOTO 140
C PRIPRAVA PRO CTENI KORENOVE STRANKY
10	UROVEN=0
	P=KOREN
C CTENI STRANKY PODLE UKAZATELE P
20	READ (LUNIND,REC=P,ERR=150) PP1
	CISLOS=P
	UROVEN=UROVEN+1
C HLEDANI KLICE V PP1
	DO 50 IB=1,M
	   IF (NGSRKL(KLIC,KL(5*IB-4))) 40,30,50
C KLIC NALEZEN
30	   CISLOV=PD(IB)
	   RETURN
C NALEZEN VETSI KLIC
40	   R=IB-1
	   IF (R.EQ.0) P=P0
	   IF (R.NE.0) P=PI(R)
	   INDEXR=R
	   GOTO 60
50	CONTINUE
	P=PI(M)
	INDEXR=M
60	IF (P.EQ.0) GOTO 70
C ULOZENI CISLA STRANKY A INDEXU INDEXR DO ZASOBNIKU
	IF (UROVEN.GE.6) GOTO 180
	IF (FUNKCE.EQ.1) GOTO 20
	ZASOB2(UROVEN)=CISLOS
	ZASOB3(UROVEN)=INDEXR
C PRECHOD KE CTENI STRANKY VYSSI UROVNE
	GOTO 20
C KLIC NENI DOSUD V IS REGISTROVAN
70	IF (FUNKCE.EQ.1) RETURN
	CITDAT=CITDAT+1
	CISLOV=-CITDAT
C PRIPRAVA POLOZKY PRO ULOZENI V IS
	CALL PPRES1(KLIC,1,KLICU,1,5)
	PIU=0
	PDU=CITDAT
C VYHLEDANI VHODNE STRANKY PRO ULOZENI POLOZKY
75	IF (M.GE.NN) GOTO 80
C ULOZENI DO STRANKY V PP1 ZA INDEXR-TOU POZICI
C VYTVORENI POTREBNEHO PROSTORU
	CALL NGSLK2(PP1,INDEXR+1,M,PP1,INDEXR+2)
C ULOZENI POLOZKY
	CALL PPRES1(KLICU,1,KL,5*INDEXR+1,5)
	PI(INDEXR+1)=PIU
	PD(INDEXR+1)=PDU
	M=M+1
C ZAPIS DOPLNENE STRANKY DO IS
	WRITE (LUNIND,REC=CISLOS,ERR=160) PP1
	RETURN
C DELENI STRANKY
80	IF (CITIND.GE.(32767-UROVEN)) GOTO 170
  	IF (INDEXR-N) 100,90,120
C NA NIZSI UROVEN SE PREDAVA POLOZKA U
90	CALL PPRES1(PPU,1,PPV,1,11)
	GOTO 110
C NA NIZSI UROVEN SE PREDAVA N-TA POLOZKA
100	CALL PPRES1(KL,5*N-4,KLICV,1,5)
	PIV=PI(N)
	PDV=PD(N)
C UPRAVA PP1
	CALL NGSLK2(PP1,INDEXR+1,INT(N-1),PP1,INDEXR+2)
C ULOZENI POLOZKY
	CALL PPRES1(KLICU,1,KL,5*INDEXR+1,5)
	PI(INDEXR+1)=PIU
	PD(INDEXR+1)=PDU
C PRENOS POSLEDNICH N POLOZEK NA NOVOU STRANKU
110	CALL NGSLK2(PP1,N+1,NN,PPN,1)
	GOTO 130
C NA NIZSI UROVEN SE PREDAVA (N+1)-NI POLOZKA
120	CALL PPRES1(KL,5*N+1,KLICV,1,5)
	PIV=PI(N+1)
	PDV=PD(N+1)
C UPRAVA PPN
	CALL NGSLK2(PP1,N+2,INDEXR,PPN,1)
	CALL NGSLK2(PP1,INDEXR+1,NN,PPN,INDEXR-N+1)
C ULOZENI POLOZKY
	CALL PPRES1(KLICU,1,KLN,5*(INDEXR-N)-4,5)
	IB=INDEXR-N
	PIN(IB)=PIU
	PDN(IB)=PDU
C ZAVER DELENI STRANKY
130	M=N
	MN=N
	P0N=PIV
	CITIND=CITIND+1
	PIV=CITIND
C ZAPIS ROZDELENYCH STRANEK DO IS
	WRITE (LUNIND,REC=CISLOS,ERR=160) PP1
	WRITE (LUNIND,REC=CITIND,ERR=160) PPN
C ZPRACOVANI POLOZKY PREDANE NA NIZSI UROVEN
	CALL PPRES1(PPV,1,PPU,1,11)
	UROVEN=UROVEN-1
	IF (UROVEN.EQ.0) GOTO 140
C PREVZETI CISLA STRANKY A INDEXU INDEXR Z VRCHOLU ZASOBNIKU
	CISLOS=ZASOB2(UROVEN)
	INDEXR=ZASOB3(UROVEN)
C CTENI STRANKY NIZSI UROVNE
	READ (LUNIND,REC=CISLOS,ERR=150) PP1
	GOTO 75
C VYTVORENI NOVE KORENOVE STRANKY
140	CITIND=CITIND+1
	M=1
	P0=KOREN
	KOREN=CITIND
C ULOZENI POLOZKY NA 1. POZICI V PP1
	CALL PPRES1(KLICU,1,KL,1,5)
	PI(1)=PIU
	PD(1)=PDU
C ZAPIS NOVE KORENOVE STRANKY DO IS
	WRITE (LUNIND,REC=CITIND,ERR=160) PP1
	RETURN
C CHYBA CTENI
150	IER=1
	RETURN
C CHYBA ZAPISU
160	IER=2
	RETURN
C IS JE OBSAZEN
170	CITDAT=CITDAT-1
	IER=4	
	RETURN
C PORUSENY IS
180	IER=6
	RETURN
	END
