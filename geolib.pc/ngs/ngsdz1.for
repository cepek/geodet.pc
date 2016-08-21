C+
C NGSDZ1      
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro praci s datovou zakladnou                      *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 	SUBROUTINE NGSDZ1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C PREHLED VSTUPNICH BODU A ODPOVIDAJICICH FORMALNICH PARAMETRU:       *
C    NGSDZI (LUNIS,DVDS,IER)                   - inicializace DZ      *
C    NGSDZO (LUNIS,LUNDS,VYMAZ,PVDS,IER)       - otevreni DZ          *
C    NGSDZC (IER)          - uzavreni aktivni DZ                      *
C    NGSDZD (IER)          - logicke zruseni aktivni DZ               *
C    NGSDZX (IER)          - zamena datovych zakladen                 *
C    NGSDZU (IER)          - aktualizace systemove stranky aktivni DZ *
C    NGSDZW (IER)          - zapis vety do aktivni DZ                 *
C    NGSDZR (ZRUS,IER)     - vyhledani a prip. zruseni vety v akt. DZ *
C    NGSDZM (IER)          - aktualizace vety v aktivni DZ            *
C    NGSDZS (IER)          - setrideny vystup                         *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	INTEGER*1 IER
	INTEGER   LUNIS,LUNDS,DVDS
	INTEGER*4 PVDS
	LOGICAL*2 VYMAZ,ZRUS
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: NGSDZ1 je ustredni  procedura rady  NGS knihovny GEOLIB/PC. *
C    Disponuje vsemi prostredky  potrebnymi k zakladani, uziti a pru- * 
C    beznemu vedeni datovych  zakladen o  strukture, popsane  v uvod- *
C    nich  odstavcich  textove casti  dokumentace. Zakladni parametry *
C    zpracovavanych datovych zakladen jsou: primy pristup k datum po- *
C    dle numerickeho klice  az  deseticiferneho, pevna delka vety ne- *
C    presahujici 256 bytu a minimalni  zarucena kapacita 229 314 vet. *
C    Procedury lze uzit k paralelni praci se dvema datovymi zakladna- *
C    mi, z nichz  jedna je  v danem  okamziku aktivni a druha pasivni *
C    (odlozena). Procedura  NGSDZ1 ma  10 vstupnich bodu; jejich pro- *
C    strednictvim muze aplikacni program vyzadat nekterou z 11 opera- *
C    ci (viz prehled vstupnich bodu  uvedeny vyse). Jednotlive opera- *
C    ce, uzite algoritmy a pozadavky na volajici program jsou detail- *
C    ne komentovany v dokumentaci procedury, ktera obsahuje i podrob- *
C    ny popis vsech oblasti  COMMON, uzivanych procedurami rady NGS a *
C    dalsi informace.                                                 *
C PREHLED PARAMETRU (podle vstupnich bodu)                            *
C NGSDZI (LUNIS,DVDS,IER) - inicializace DZ                           *
C *****************************************                           *
C VSTUPNI PARAMETRY:                                                  *
C    LUNIS  - cislo volne (pracovni) logicke  jednotky, kterou proce- *
C             dura pouzije k inicializaci DZ; po  ukonceni operace se *
C             jednotka uvolnuje k dalsimu pouziti                     *
C    DVDS   - delka vety datoveho souboru (v bytech)  vcetne klice ve *
C             formatu GEOGEP/M o delce 5 bytu (viz dokumentaci proce- *
C             dury NGSKKL)                                            *
C VYSTUPNI PARAMETR:                                                  *
C    IER    - indikace chybovych stavu:                               *
C             IER=0 uspesne provedena operace,                        *
C             IER=1 chybna  hodnota parametru DVDS (DVDS<1 nebo DVDS> *
C                   >256),                                            *
C             IER=2 chyba pri zapisu systemove stranky                *
C SPOLECNE OBLASTI: pred vyzadanim operace je treba v oblasti /NGS5W/ *
C                   definovat veliciny DISKIS a JM (viz odst. 4.1 do- *
C                   kumentace)                                        *
C NGSDZO (LUNIS,LUNDS,VYMAZ,PVDS,IER) - otevreni DZ                   *
C *************************************************                   *
C VSTUPNI PARAMETRY:                                                  *
C    LUNIS  - cislo logicke jednotky pro indexovy soubor              *
C    LUNDS  - cislo logicke jednotky pro datovy soubor                *
C    VYMAZ  - charakteristika DZ:                                     *
C             VYMAZ=.TRUE.  DZ bude otevrena jako docasna,            *
C             VYMAZ=.FALSE. DZ bude otevrena jako trvala              *
C    PVDS   - maximalni pocet vet datoveho souboru                    *
C VYSTUPNI PARAMETR:                                                  *
C    IER    - indikace chybovych stavu:                               *
C             IER=0 uspesne provedena operace,                        *
C             IER=1 DZ neexistuje  nebo chyba pri  otevreni DZ, prip. *
C                   chyba pri cteni systemove stranky,                *
C SPOLECNE OBLASTI: pred vyzadanim operace je treba v oblasti /NGS5W/ *
C                   definovat veliciny DISKIS, DISKDS a JM (viz odst. *
C                   4.2 dokumentace)                                  *
C NGSDZC (IER) - uzavreni aktivni DZ                                  *
C **********************************                                  *
C VYSTUPNI PARAMETR:                                                  *
C    IER    - indikace chybovych stavu                                *
C             IER=0 uspesne provedena operace,                        *
C             IER=1 chyba pri uzavreni DZ,                            *
C             IER=2 chyba pri zapisu systemove stranky                *
C NGSDZD (IER) - logicke zruseni aktivni DZ                           *
C *****************************************                           *
C VYSTUPNI PARAMETR:                                                  *
C    IER    - indikace chybovych stavu                                *
C             IER=0 uspesne provedena operace,                        *
C             IER=2 chyba pri zapisu systemove stranky                *
C NGSDZX (IER) - zamena datovych zakladen                             *
C *****************************************                           *
C VYSTUPNI PARAMETR:                                                  *
C    IER    - parametr s formalnim vyznamem (pri navratu do volajici- *
C             ho programu je vzdy IER=0)                              *
C NGSDZU (IER) - aktualizace systemove stranky aktivni DZ             *
C *******************************************************             *
C VYSTUPNI PARAMETR:                                                  *
C    IER    - indikace chybovych stavu                                *
C             IER=0 uspesne provedena operace,                        *
C             IER=2 chyba pri zapisu systemove stranky                *
C NGSDZW (IER) - zapis vety do aktivni DZ                             *
C ***************************************                             *
C VYSTUPNI PARAMETR:                                                  *
C    IER    - indikace chybovych stavu                                *
C             IER=0 uspesne provedena operace,                        *
C             IER=1 chyba pri cteni z DZ,                             *
C             IER=2 chyba pri zapisu do DZ,                           *
C             IER=4 veta nebyla ulozena do DZ (dosazena mez stanovena *
C                   parametrem  PVDS  pri otevreni  DZ nebo vycerpana *
C                   kapacita DZ),                                     *
C             IER=5 veta nebyla ulozena, protoze v DZ existuje veta s *
C                   duplicitnim klicem,                               *
C             IER=6 kolaps DZ                                         *
C SPOLECNE OBLASTI: pred  vyzadanim  operace  je treba ukladanou vetu *
C                   pripravit v oblasti /NGS3W/, pricemz  v prvnich 5 *
C                   bytech musi byt zapsan  klic ve formatu GEOGEP/M; *
C                   je-li v DZ nalezena duplicitni veta (IER=5), bude *
C                   odevzdana volajicimu programu v oblasti /NGS4W/   *
C NGSDZR (ZRUS,IER) - vyhledani a prip. zruseni vety v aktivni DZ     *
C ***************************************************************     *
C VSTUPNI PARAMETR:                                                   *
C    ZRUS   - pozadavek na zruseni vety:                              *
C             ZRUS=.TRUE.  existujici veta s danym klicem bude logic- *
C                          ky zrusena,                                *
C             ZRUS=.FALSE. obsah aktivni DZ se nemeni                 *
C VYSTUPNI PARAMETR:                                                  *
C    IER    - indikace chybovych stavu                                *
C             IER=0 uspesne provedena operace - hledana veta je vola- *
C                   jicimu programu k dispozici v oblasti /NGS3W/,    *
C             IER=1 chyba pri cteni z DZ,                             *
C             IER=2 chyba pri zapisu do datoveho souboru v souvislos- *
C                   ti se zrusenim vety,                              *
C             IER=4 hledana veta nebyla nalezena,                     *
C             IER=6 kolaps DZ                                         *
C SPOLECNE OBLASTI: pred vyzadanim operace je treba do prvnich 5 bytu *
C                   oblasti /NGS3W/ ulozit  ve formatu  GEOGEP/M klic *
C                   hledane  vety; uspesne nalezenou vetu najde vola- *
C                   jici program v teze oblasti                       *
C NGSDZM (IER) - aktualizace vety v aktivni DZ                        *
C ********************************************                        *
C VYSTUPNI PARAMETR:                                                  *
C    IER    - indikace chybovych stavu                                *
C             IER=0 uspesne provedena operace,                        *
C             IER=2 chyba pri zapisu do datoveho souboru,             *
C             IER=4 operace byla vyzadana v nedovolenem kontextu (ni- *
C                   koliv  bezprostredne po  uspesnem  vyhledani vety *
C                   nebo po identifikaci vety s duplicitnim klicem),  *
C SPOLECNE OBLASTI: v okamziku vyzadani operace musi byt veta s aktu- *
C                   alizovanym obsahem  pripravena  v oblasti /NGS3W/ *
C                   (v prvnich 5  bytech  oblasti  je klic ve formatu *
C                   GEOGEP/M)                                         *
C NGSDZS (IER) - setrideny vystup                                     *
C *******************************                                     *
C VYSTUPNI PARAMETR:                                                  *
C    IER    - indikace chybovych stavu                                *
C             IER=0 uspesne  provedena  operace  (volajicimu programu *
C                   byla predana  logicky nezrusena veta aktivni DZ), *
C             IER=1 chyba pri cteni DZ,                               *
C             IER=7 DZ je prazdna nebo  vycerpana (pri posledni akti- *
C                   vaci byla predana  "posledni" veta DZ, tj. veta s *
C                   nejvetsim klicem)                                 *
C SPOLECNE OBLASTI: veta vynata z DZ je procedurou ukladana do oblas- *
C                   ti /NGS3W/; v prvnich 5 bytech je klic ve formatu *
C                   GEOGEP/M                                          *
C EXTERNI PROCEDURY: NGSIOZ, NGSLK1, NGSORT, PPRES1                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	INTEGER*1 OBL1(20),PP(158),PP1(158),KLIC(5),ODLOZ(20),
     1  	  VETA(256),EXVETA(256),ZB22(22)
	INTEGER LUNIND,LUNIS1,LUNDAT,KOREN,CITIND
     1          I,DV,DVDS1,UROVEN
        INTEGER*4 CITDAT,MAXPV,CISLOV
	LOGICAL*1 ZAMEK,ZAPIS
	LOGICAL*2 DOCAS
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C LOKALNI PROMENNE:                                                   *
C    LUNIND,LUNDAT,KOREN,CITIND,CITDAT,DV,DOCAS,MAXPV -               *
C           - charakteristiky aktivni DZ                              *
C    KLIC   - klic ve formatu GEOGEP/M                                *
C    VETA   - informacni veta DS	                              *
C    EXVETA - existujici veta DZ                                      *
C    LUNIS1,DVDS1 - cislo log. jednotky a delka vety DS (predani pro- *
C		    cedure NGSIOZ)                                    *
C    ODLOZ  - charakteristiky odlozene DZ                             *
C    PP     - procedurou NGSDZ1 neuzivany prostor                     *
C    PP1    - pracovni prostor uzivany pri zamene DZ                  *
C    CISLOV - cislo vety DS                                           *
C    ZAMEK  - prepinac pro povoleni aktualizace vety                  *
C    ZAPIS  - interni prepinac                                        *
C    UROVEN - viz procedura NGSORT                                    *
C    ZB22   - cast /NGS6W/, s niz procedura nepracuje                 *
C    OBL1   - pole pro prekryti oblasti COMMON                        *
C    I      - pomocna promenna                                        *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C 
C OBLASTI COMMON:
	COMMON /NGS1W/ LUNIND,LUNDAT,KOREN,CITIND,CITDAT,DV,
     1                 DOCAS,MAXPV,ODLOZ,CISLOV,ZAMEK	
	COMMON /NGS2W/ PP,PP1
	COMMON /NGS3W/ VETA
	COMMON /NGS4W/ EXVETA
	COMMON /NGS6W/ UROVEN,ZB22
	COMMON /NGS7W/ LUNIS1,DVDS1
C
	EQUIVALENCE (LUNIND,OBL1(1)),(KLIC(1),VETA(1))
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C INICIALIZACE DZ
	ENTRY NGSDZI (LUNIS,DVDS,IER)
	ZAMEK=.TRUE.
        LUNIS1=LUNIS
        DVDS1=DVDS
        CALL NGSIOZ (INT1(1),IER)
        RETURN
C
C OTEVRENI DZ
	ENTRY NGSDZO (LUNIS,LUNDS,VYMAZ,PVDS,IER)
	ZAMEK=.TRUE.
	UROVEN=0
C ODLOZENI AKTIVNI DZ
	CALL PPRES1(OBL1,1,ODLOZ,1,20)
C DEFINICE PARAMETRU AKTIVNI DZ
	LUNIND=LUNIS
	LUNDAT=LUNDS
	DOCAS=VYMAZ
	MAXPV=PVDS
	CALL NGSIOZ(INT1(2),IER)
	RETURN
C
C UZAVRENI DZ
	ENTRY NGSDZC (IER)
	ZAMEK=.TRUE.
	UROVEN=0
	CALL NGSIOZ(INT1(3),IER)
	RETURN
C
C ZRUSENI AKTIVNI DZ
	ENTRY NGSDZD (IER)
	ZAMEK=.TRUE.
	UROVEN=0
	CALL NGSIOZ(INT1(4),IER)
	RETURN
C
C ZAMENA DZ
	ENTRY NGSDZX (IER)
	ZAMEK=.TRUE.
	UROVEN=0
	CALL PPRES1(ODLOZ,1,PP1,1,20)
	CALL PPRES1(OBL1,1,ODLOZ,1,20)
	CALL PPRES1(PP1,1,OBL1,1,20)
	IER=0
	RETURN
C
C AKTUALIZACE SYSTEMOVE STRANKY
	ENTRY NGSDZU (IER)
	ZAMEK=.TRUE.
	CALL NGSIOZ(INT1(5),IER)
	RETURN
C
C ZAPIS VETY
	ENTRY NGSDZW (IER)
	UROVEN=0
	IER=4
	ZAMEK=.TRUE.
	IF (CITDAT.GE.MAXPV) RETURN
	ZAPIS=.FALSE.
C ZAPIS KLICE DO IS
	CALL NGSLK1(KLIC,CISLOV,INT1(2),IER)
	IF (IER.GT.0) RETURN
	IF (CISLOV.LT.0) ZAPIS=.TRUE.
	CISLOV=IABS(CISLOV)
	IF (ZAPIS) GOTO 116
C EXISTUJICI, PRIP. ZRUSENA VETA
	READ (LUNDAT,REC=CISLOV,ERR=220) (EXVETA(I),I=1,DV)
	IF (EXVETA(1).LT.0) GOTO 116
C DRIVE ZAPSANA A NEZRUSENA VETA
	ZAMEK=.FALSE.
	IER=5
	RETURN
C ZAPIS (PREPIS) VETY
116	WRITE (LUNDAT,REC=CISLOV,ERR=230) (VETA(I),I=1,DV)
	RETURN
C
C VYHLEDANI VETY, ZRUSENI VETY
	ENTRY NGSDZR (ZRUS,IER)
	ZAMEK=.TRUE.
C VYHLEDANI KLICE V IS
	CALL NGSLK1(KLIC,CISLOV,INT1(1),IER)
	IF (IER.GT.0) RETURN
	IER=4
	IF (CISLOV.EQ.0) RETURN
C KLIC JE REGISTROVAN V IS
   	READ (LUNDAT,REC=CISLOV,ERR=220) (VETA(I),I=1,DV)
	IF (VETA(1).LT.0) RETURN
C KLIC JE REGISTROVAN V DS
	IER=0
	IF (ZRUS) THEN
C          ZRUSENI VETY
	   VETA(1)=VETA(1)-100
	   WRITE (LUNDAT,REC=CISLOV,ERR=230) (VETA(I),I=1,DV)
	   VETA(1)=VETA(1)+100
	ELSE
	   ZAMEK=.FALSE.
	ENDIF
	RETURN
C
C AKTUALIZACE VETY
	ENTRY NGSDZM (IER)
	IER=4
	IF (ZAMEK) RETURN
	ZAMEK=.TRUE.
	IER=0
	GOTO 116
C
C SETRIDENY VYSTUP
	ENTRY NGSDZS (IER)
	ZAMEK=.TRUE.
	CALL NGSORT(IER)
	IF (IER.EQ.0) RETURN
	IF (IER.EQ.1) GOTO 240
	GOTO 220
C
C CHYBA CTENI
220	IER=1
	RETURN
C CHYBA ZAPISU
230	IER=2
	RETURN
C PREDANY VSECHNY VETY
240	IER=7
	RETURN 	
	END
