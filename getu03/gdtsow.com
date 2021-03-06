C
C STAVOVA OBLAST PROGRAMU GETU03
C=======================================================================
C
C   KONSTANTY
C
C ITT     LOGICKA JEDNOTKA PRIRAZENA TERMINALU
C GSGDDZ  LOGICKA JEDNOTKA SOUBORU DATOVE ZAKLADNY
C GSGDPT  LOGICKA JEDNOTKA ZARIZENI PROTOKOLU
C GSGDIS  LOGICKA JEDNOTKA SOUBORU INFORMACI O SITI
C GDGDVV  LOGICKA JEDNOTKA SOUBORU OBSAHUJICIHO VYSLEDKY VYROVNANI
      INTEGER  ITT,GSGDDZ,GSGDPT,GSGDIS,GSGDVV
C*    PARAMETER  (ITT=1,GSGDDZ=2,GSGDPT=3,GSGDIS=4)
      COMMON  /ITT/ ITT /GSGDDZ/ GSGDDZ /GSGDPT/ GSGDPT /GSGDVV/ GSGDVV
C
C MAXBOD  MAXIMALNI POCET BODU SITE
C MAX     PRACOVNI DIMENZE POLI V SEZNAMU BODU
C MAXMER  KAPACITA SEZNAMU MERENYCH PRVKU
      INTEGER  MAXBOD,MAX,MAXMER
      PARAMETER  (MAXBOD=150,MAX=MAXBOD+1,MAXMER=1000)
C
C NXDIM   DIMENZE POLE PRO UKLADANI INDEXU NEZNAMYCH, KTERE VSTUPUJI
C         DO PODMINKY [X]=MIN.
C NSDIM   DIMENZE POLE PRO UKLADANI INDEXU LINEARNE ZAVISLYCH SLOUPCU
C         V MATICI SOUSTAVY ROVNIC OPRAV
C CPDIM   DIMENZE POMOCNEHO POLE PRO ULOZENI JEDNOHO SLOUPCE MATICE
C         SOUSTAVY ROVNIC OPRAV
      INTEGER  NXDIM,NSDIM,CPDIM
      PARAMETER  (NXDIM=3*MAXBOD+1,NSDIM=3*MAXBOD)
      PARAMETER  (CPDIM=3*MAXBOD+MAXMER)
C
C ISREC   DELKA ZAZNAMU SOUBORU INFORMACI O SITI (RECORDSIZE)
C SOWDIM  DIMENZE POMOCNEHO POLE, KTERE MUSI POKRYVAT CELOU STAVOVOU
C         OBLAST PROGRAMU GETU03 ( COMMON /GDTSOW/ )
C SOWREC  POCET ZAZNAMU VYHRAZENYCH PRO ULOZENI STAVOVE OBLASTI
C VIRDIM  DIMENZE VIRTUALNIHO PRACOVNIHO POLE PRO UKLADANI MATICE
C         SOUSTAVY ROVNIC OPRAV
      INTEGER  ISREC,SOWDIM,SOWREC
      INTEGER*4  VIRDIM
      PARAMETER  (ISREC=16,SOWDIM=7333,VIRDIM=70000)
      PARAMETER  (SOWREC=(SOWDIM+ISREC-1)/ISREC)
C
C RO      PREVADI OBLOUKOVOU MIRU NA GRADY: GRADY=RADIANY*RO
C DVEPI   DELKA KRUHU V RADIANECH
      DOUBLE PRECISION  RO,DVEPI
      PARAMETER (RO=63.6619772367581343D0,DVEPI=6.28318530717958648D0)
C
C======================================================================
C
C   SYSTEMOVE PARAMETRY
C
C M0       APRIORNI JEDNOTKOVA STREDNI CHYBA [MM/CC] (1E1)
C TOLABS   TOLERANCE PRO TESTOVANI ABSOLUTNICH CLENU ROVNIC OPRAV
C          - ROZDIL DELEK, RESP. PRICNA ODCHYLKA [MM] (1E3)
C TOL      TOLERANCE PRO IDENTIFIKACI LINEARNE ZAVISLYCH SLOUPCU
C          V ORTOGONALIZOVANYCH MATICICH (1E-2)
C
      REAL  M0,TOLABS,TOL
C
C=======================================================================
C
C   PARAMETRY SOUBORU INFORMACI O SITI (IS) - PARSOU
C
C SOW     POLE SLOUZICI PREDEVSIM PRO NACTENI RESP. ZAPIS STAVOVE
C         OBLASTI Z DISKU RESP. NA DISK (DIMENZE  SOWDIM  MUSI BYT
C         ZVOLENA TAK, ABY POLE  SOW  POKRYVALO CELOU STAVOVOU OBLAST)
C SS      STAVOVE SLOVO PROGRAMU, UDAVA ETAPU RESENI ULOHY:
C          SS            STAV SOUBORU IS             POCBOD  POCPOZ
C           0  SOUBOR IS ZALOZEN, NEDEFINOVANA SIT      0       0
C           1  SIT DEFINOVANA POUZE SVYMI BODY         >0       0
C              (V SEZBOD REGISTROVAN ALESPON 1 BOD)
C           2  SIT DEFINOVANA SVYMI BODY A MERENYMI    >0      >0
C              PRVKY, KTERE JE TREBA REDIGOVAT
C           3  SIT DEFINOVANA SVYMI BODY A REDIGO-     >0      >0
C              VANYMI MERENYMI PRVKY
C           4  SIT DEFINOVANA BODY, REDIGOVANYMI       >0      >0
C              MERENYMI PRVKY A VYSLEDKY 1. ORTO-
C              GONALIZACE
C KK      VYSTUPNI PARAMETR VYBRANYCH PROCEDUR PROGRAMU, JE POUZIVAN
C         VE VYPOCTENEM PREPINACI  GOTO  PO NAVRATU Z PROCEDURY
C PK      POSTUPOVY KLIC
C DVYT    DATUM VYTVORENI SOUBORU INFORMACI O SITI
C DUZA    DATUM POSLEDNIHO UZAVRENI SOUBORU INFORMACI O SITI
C ISEZT   CISLO SEZNAMU DATOVE ZAKLADNY ZADANE Z TERMINALU
C         (JE POROVNAVANO S HODNOTOU  ISEZ  SOUBORU IS)
C PRASDZ  LOGICKA PROMENNA, KTERA UDAVA PRACUJE-LI SE S DATOVOU
C         ZAKLADNOU NEBO NE
C PRASIS  LOGICKA PROMENNA, KTERA UDAVA EXISTUJE-LI SOUBOR IS
C LPT     LOGICKA PROMENNA, POZADAVEK NA VOLITELNY TISK DO PROTOKOLU
C LTT     LOGICKA PROMENNA, POZADAVEK NA VOLITELNY TISK NA TERMINAL
C SOUBT   JMENO SOUBORU DATOVE ZAKLADNY VLOZENE Z TERMINALU
C         (POROVNAVA SE S OBSAHEM POLE  SOUB  SOUBORU IS)
C ULOHA   JMENO ZPRACOVAVANE ULOHY
C JMENO   JMENO SOUBORU INFORMACI O SITI
C SOTEXT  TEXTOVY RETEZEC IDENTIFIKUJICI PROGRAM (PO PRVNIM TISKU
C         NA TERMINAL JE RETEZEC VYMAZAN)
C
      INTEGER*4  SOW(SOWDIM)
      INTEGER  SS,KK,PK,DVYT(3),DUZA(3),ISEZT
      LOGICAL  PRASDZ,PRASIS,LPT,LTT
      INTEGER*1  SOUBT(40),ULOHA(44),JMENO(40),SOTEXT(40)
C
C=======================================================================
C
C   PARAMETRY SITE - PARSIT
C
C ISEZ    CISLO SEZNAMU DATOVE ZAKLADNY (ZADANE PRI VYTVORENI
C         SOUBORU INFORMACI O SITI)
C PEVBOD  POCET PEVNYCH BODU SITE
C URCBOD  POCET URCOVANYCH BODU SITE
C POCBOD  CELKOVY POCET BODU SITE (POCBOD=PEVBOD+URCBOD)
C POCSME  POCET SMERU
C POCDEL  POCET DELEK
C POCPOZ  POCET POZOROVANI (POCPOZ=POCSME+POCDEL)
C POCORP  POCET ORIENTACNICH POSUNU (POCET OSNOV SMERU)
C POCVAC  POCET VYBOCUJICICH ABSOLUTNICH CLENU
C SOUB    JMENO SOUBORU DATOVE ZAKLADNY (ZADANE PRI VYTVORENI
C         SOUBORU INFORMACI O SITI)
C
      INTEGER  ISEZ,PEVBOD,URCBOD,POCBOD,POCSME,POCDEL,POCPOZ,POCORP,
     /  POCVAC
      INTEGER*1  SOUB(40)
C
C=======================================================================
C
C   SEZNAM BODU - SEZBOD
C
C SBPOC    POCET BODU ULOZENYCH V SEZNAMU
C CB1,CB2,CB3  SLOZKY INTERNIHO VYJADRENI CISLA BODU
C SBY,SBX  SOURADNICE
C SBZ      ORIENTACNI POSUN V OBLOUKOVE MIRE <0,2PI)
C          (NEEXISTUJICI OR. POSUN: SBZ = -1D0)
C CHAB     CHARAKTERISTIKA BODU
C CHAS     CHARAKTERISTIKA SOURADNIC
C TYPB     TYP BODU ('P' - PEVNY BOD / 'U' - URCOVANY BOD /
C          ' ' - NEDEFINOVAN)
C IZOBOD   IDENTIFIKACE IZOLOVANYCH BODU ('I' - IZOLOVANY BOD /
C          'N' - NEIZOLOVANY BOD)
C
      INTEGER  SBPOC,CB1(MAX),CB2(MAX),CB3(MAX)
      DOUBLE PRECISION  SBY(MAX),SBX(MAX),SBZ(MAX)
      INTEGER*1  CHAB(MAX),CHAS(MAX),TYPB(MAX),IZOBOD(MAX)
C
C=======================================================================
C
C   SEZNAM MERENYCH VELICIN - SEZMER
C
C SMPOC    POCET MERENYCH VELICIN ULOZENYCH V SEZNAMU
C CST      PRACOVNI CISLO STANOVISKA
C CCI      PRACOVNI CISLO CILE
C PRVEK    MERENY SMER (OBLOUKOVA MIRA <0,2PI) ) NEBO DELKA (M)
C TYPP     TYP PRVKU ('S' - SMER / 'D' - DELKA)
C STRCH    STREDNI CHYBA ( CC / MM )
C VAHA     +- ODMOCNINA Z VAHY MERENE VELICINY;
C          ZAPORNA HODNOTA SIGNALIZUJE VYBOCUJICI ABSOLUTNI CLEN
C CHAZ     CHARAKTERISTIKA ZAMERY
C
      INTEGER  SMPOC,CST(MAXMER),CCI(MAXMER)
      DOUBLE PRECISION  PRVEK(MAXMER)
      REAL  STRCH(MAXMER),VAHA(MAXMER)
      INTEGER*1  TYPP(MAXMER),CHAZ(MAXMER)
C
C======================================================================
C
C   PARAMETRY SOUSTAVY ROVNIC OPRAV
C
C POCNEZ  POCET NEZNAMYCH (POCNEZ=POCORP+2*URCBOD)
C MARED   POCET RADKU MATICE SOUSTAVY ROVNIC OPRAV (MARED=POCPOZ+POCNEZ)
C NARED   POCET SLOUPCU MATICE SOUSTAVY ROVNIC OPRAV (NARED=POCNEZ+1)
C DEFEKT  DEFEKT MATICE SOUSTAVY ROVNIC OPRAV
C NXMIN   POCET NEZNAMYCH, PRO NEZ MA BYT [X]=MIN.
C INDX    POLE PRO UKLADANI INDEXU NEZNAMYCH, KTERE VSTUPUJI
C         DO PODMINKY [X]=MIN.
C INDS    POLE PRO UKLADANI INDEXU LINEARNE ZAVISLYCH SLOUPCU V MATICI
C         SOUSTAVY ROVNIC OPRAV
C PVV     [PVV]
C SUMVVS  [PVV] SMERY
C SUMVVD  [PVV] DELKY
C NADBPO  POCET NADBYTECNYCH POZOROVANI
C MAXDLT  MAXIMALNI POKLES [PVV]
C INDELT  INDEX POZOROVANI, JEHOZ VYPUSTENI VEDE K MAXIMALNIMU POKLESU
C         [PVV]; SOUCASNE INDEX MAXIMALNI NORMOVANE NEBO STUDENTIZOVANE
C         OPRAVY
C SUMQVS  PODIL SMERU NA CELKOVEM POCTU NADBYTECNYCH POZOROVANI
C SUMQVD  PODIL DELEK NA CELKOVEM POCTU NADBYTECNYCH POZOROVANI;
C         PLATI: NADBPO = SUMQVS + SUMQVD
C M0AKT   HODNOTA AKTUALNI JEDNOTKOVE STREDNI CHYBY (M0 NEBO M0')
C TYPM0   TYP M0AKT ("APRIORNI" NEBO "EMPIR.")
C PRST    KONFIDECNI PRAVDEPODOBNOST [%]
C M01     EMPIRICKA JEDNOTKOVA STREDNI CHYBA M0'
C M02     M0'' (PREDIKACE CHYBY PO VYPUSTENI POZOROVANI)
C KPRST   KOEFICIENT INTERNI KONFIDENCNI OBLASTI PRO PRST
C POB     POCET OPERNYCH BODU
C INTERT  POLE, VE KTEREM JSOU V PRUBEHU ANALYZY, TISKU VYSLEDKU
C         A VARIANTNIHO VYROVNANI VOLNE SITE ULOZENY TYPY BODU (P/U/O)
C NAZVAR  NAZEV VARIANTY RESENI - RETEZEC, KTERYM UZIVATEL MUZE OZNACO-
C         VAT VARIANTY RESENI VOLNE SITE
C
      INTEGER  POCNEZ,MARED,NARED,DEFEKT,NXMIN,INDX(NXDIM),INDS(NSDIM),
     /  NADBPO, INDELT,POB
      REAL  PVV,SUMVVS,SUMVVD,MAXDLT,SUMQVS,SUMQVD,M0AKT,PRST,
     /  M01,M02,KPRST
      INTEGER*1  TYPM0(8),INTERT(MAX),NAZVAR(25)
C
C=======================================================================
C
      EQUIVALENCE (SBY,SOW)
      COMMON /GDTSOW/ SBY,SBX,SBZ,PRVEK,
     / M0,TOLABS,TOL,
     / PRASDZ,PRASIS,LPT,LTT,STRCH,VAHA,
     / PVV,SUMVVS,SUMVVD,MAXDLT,SUMQVS,SUMQVD,M0AKT,PRST,
     / M01,M02,KPRST,
     / SS,KK,PK,DVYT,DUZA,
     / ISEZ,PEVBOD,URCBOD,POCBOD,POCSME,POCDEL,POCPOZ,POCORP,POCVAC,
     / SBPOC,CB1,CB2,CB3,
     / SMPOC,CST,CCI,
     / POCNEZ,MARED,NARED,DEFEKT,NXMIN,INDX,INDS,NADBPO,INDELT,POB,
     / ULOHA,
     / SOUB,
     / CHAB,CHAS,TYPB,IZOBOD,
     / TYPP,CHAZ,
     / TYPM0,INTERT,NAZVAR
      COMMON /GDTSOX/ ISEZT,SOUBT,JMENO,SOTEXT
C
C=======================================================================
C
