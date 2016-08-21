C+
C NGSIOZ
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro globalni operace s datovou zakladnou           *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE NGSIOZ (FUNKCE,IER)
	INTEGER*1 FUNKCE,IER
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: NGSIOZ je pomocna  procedura, podporujici  proceduru NGSDZ1 *
C    pri realizaci globalnich operaci s datovymi zakladnami. V zavis- *
C    losti na zvolenem cisle operace umoznuje zakladny inicializovat, *
C    otevirat a uzavirat, logicky  je rusit  a v indexovych souborech *
C    datovych zakladen prubezne aktualizovat  systemovou stranku (viz *
C    dokumentaci procedury NGSDZ1).                                   *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETR:                                                   *
C    FUNKCE - typ pozadovane operace                                  *
C             =1 ... inicializace DZ                                  *
C	      =2 ... otevreni existujici DZ                           *
C	      =3 ... zavreni aktivni DZ                               *
C             =4 ... logicke zruseni aktivni DZ                       *
C             =5 ... aktualizace systemove stranky aktivni DZ         *
C VYSTUPNI PARAMETR:                                                  *
C    IER    - indikace chybovych stavu (viz dokumentaci procedury     *
C             NGSDZ1)                                                 *
C SPOLECNE OBLASTI: viz textovou cast dokumentace a kap. 3 dokumenta- *
C                   ce procedury NGSDZ1                               *
C EXTERNI PROCEDURA: NGSJDZ                                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	INTEGER LUNIND,LUNIS1,KOREN,KOREN1,CITIND,CITIS1,LUNDAT,
     1  	DV,DVDS1
	INTEGER*4 CITDAT,CITDS1
	CHARACTER DISKIS*40,DISKDS*40,JM*8,UJMENO*40,STAT*6
	LOGICAL*2 DOCAS
	INTEGER*1 ZB29(29)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C LOKALNI PROMENNE:                                                   *
C    LUNIND - cislo logicke jednotky pro IS                           *
C    LUNIS1 - cislo log. jednotky uzivane pri inicializaci            *
C    LUNDAT - cislo log. jednotky pro DS                              *
C    KOREN  - cislo stranky v IS, kde je registrovana korenova stran- *
C             ka                                                      *
C    CITIND - pocet stranek IS                                        *
C    CITDAT - pocet vet v DS                                          *
C    DV     - delka vety DS                                           *
C    KOREN1,CITIS1,CITDS1,DVDS1 - udaje  pro  inicializaci  systemove *
C                                 stranky IS                          *
C    DOCAS  - priznak pro vymaz DZ po jejim uzavreni                  *
C    ZB29   - cast /NGS1W/, s niz procedura nepracuje                 *
C    JM,UJMENO - jmeno a uplne jmeno DZ                               *
C    DISKIS,DISKDS - oznaceni disku a adresare pro IS, resp. DS       *
C    STAT   - pomocna promenna                                        *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C 
C OBLASTI COMMON:
	COMMON /NGS1W/ LUNIND,LUNDAT,KOREN,CITIND,CITDAT,DV,
     1	               DOCAS,ZB29	
	COMMON /NGS5W/ DISKIS,DISKDS,JM
	COMMON /NGS7W/ LUNIS1,DVDS1
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	IER=0
	GOTO (10,30,50,62,64), FUNKCE
C
C INICIALIZACE
10	IF ((DVDS1.GT.256).OR.(DVDS1.LT.1)) GOTO 90
	CALL NGSJDZ (DISKIS,JM,'.NDX',UJMENO)
	OPEN (UNIT=LUNIS1, ACCESS='DIRECT', FORM='UNFORMATTED',
     1	      ERR=90,RECL=158, FILE=UJMENO, STATUS='NEW')		
	KOREN1=0
	CITIS1=1
	CITDS1=0
	WRITE (LUNIS1,REC=1,ERR=95)
     1         KOREN1,CITIS1,CITDS1,DVDS1
        CLOSE (LUNIS1,ERR=90)
	RETURN
C
C OTEVRENI IS A DS
30	CALL NGSJDZ (DISKIS,JM,'.NDX',UJMENO)
	OPEN (UNIT=LUNIND, ACCESS='DIRECT', FORM='UNFORMATTED',
     1	      ERR=90,RECL=158, FILE=UJMENO, STATUS='OLD')
	READ (LUNIND,REC=1,ERR=90) KOREN,CITIND,CITDAT,DV
	CALL NGSJDZ (DISKDS,JM,'.DAT',UJMENO)
	OPEN (UNIT=LUNDAT, ACCESS='DIRECT', FORM='UNFORMATTED',
     1	      ERR=90, RECL=DV, FILE=UJMENO, STATUS='UNKNOWN')
	RETURN
C
C UZAVRENI DS A IS
50	IF (DOCAS) THEN
	   STAT='DELETE'
	ELSE
	   STAT='KEEP'
	ENDIF
	CLOSE (LUNDAT,ERR=90,STATUS=STAT)
60	WRITE (LUNIND,REC=1,ERR=95) KOREN,CITIND,CITDAT,DV
	CLOSE (LUNIND,ERR=90,STATUS=STAT)
	RETURN
C
C LOGICKE ZRUSENI DZ
62	KOREN=0
	CITIND=1
	CITDAT=0
C
C AKTUALIZACE SYSTEMOVE STRANKY IS
64	WRITE (LUNIND,REC=1,ERR=95) KOREN,CITIND,CITDAT,DV
	RETURN
C
C CHYBA CTENI NEBO CHYBA PRI OTEVIRANI, UZAVIRANI, PRIP. INICI-
C ALIZACI DZ
90	IER=1
	RETURN
C CHYBA ZAPISU
95	IER=2
	RETURN
	END
