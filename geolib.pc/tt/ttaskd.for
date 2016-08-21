C+
C TTASKD 
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro realizaci dialogu (odpoved: DOUBLE PRECISION)  *
C AUTOR: Ales Cepek, VUGTK                                            *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE TTASKD(ITT,TEXT,IOUT,POLE,IDIM)
      INTEGER     ITT,IOUT,IDIM
      CHARACTER   TEXT(1)
      REAL*8      POLE(IDIM)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Popisovana procedura patri k souboru procedur TTASKx, pomo- *
C    ci nichz lze jednoduse sestavovat programy dialogoveho typu (viz *
C    dokumentaci procedury TTASK). Procedura vypisuje na obrazovce    *
C    terminalu zadany text vyzvy (3. typu), cte odpoved vlozenou ope- *
C    ratorem z klavesnice terminalu, analyzuje ji a predava volajici- *
C    mu programu. Odpoved tvori v zavislosti na volbe procedury pos-  *
C    loupnost nejvyse 40 numerickych hodnot ve tvaru slucitelnem s    *
C    fortranskou konverzi D (TTASKD), I (TTASKN), F (TTASKR), prip.   *
C    posloupnost cisel typu INTEGER*4 (TTASKJ). Jednotlive udaje v    *
C    odpovedi mohou byt oddeleny znakem " , " nebo nejmene jednou     *
C    mezerou. Prectena data se ukladaji do vystupniho pole s prvky    *
C    typu DOUBLE PRECISION (TTASKD), INTEGER (TTASKN), INTEGER*4      *
C    (TTASKJ), resp. REAL (TTASKR). Skutecny pocet zpracovanych udaju *
C    predavaji procedury prostrednictvim dalsiho vystupniho parame-   *
C    tru, kterym alternativne signalizuji vyskyt nepripustnych odpo-  *
C    vedi  (odpoved tvori nadmerny pocet hodnot nebo byla porusena    *
C    kompatibilita dat s prislusnou konverzi), prip. typ tzv. global- *
C    ni odpovedi (viz dokumentace procedury TTASK).                   *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    ITT    - cislo logicke jednotky, na kterou bude vypsan text      *
C             vyzvy a ze ktere bude prectena odpoved operatora.       *
C    TEXT   - retezec (vyzva), ktery bude vypsan na zarizeni ITT.     *
C             Text vyzvy se uvozuje a zakoncuje znakem " ' ".         *
C             Poslednim vyznamnym znakem vyzvy musi byt znak " : ".   *
C             V textu vyzvy muze byt uvedena implicitni hodnota       *
C             odpovedi (na obrazovku se vypisuje za znakem " : "      *
C             ukoncujicim text vyzvy jedna mezera a je-li specifiko-  *
C             vana, implicitni hodnota odpovedi - kurzor je nastaven  *
C             na prvni znak implicitni odpovedi). Nepovinna implicit- *
C             ni hodnota odpovedi se uvadi uzavrena mezi dvema znaky  *
C             " / " na zacatku retezce (od prvni pozice).             *
C    IDIM   - maximalni pocet hodnot, ktere mohou tvorit odpoved      *
C             (IDIM<=40). Odpovi-li operator vice nez IDIM hodnotami, *
C             je hlasena chyba.                                       *
C VYSTUPNI PARAMETRY:                                                 *
C    IOUT   - IOUT = -4  nepripustna odpoved,                         *
C             IOUT = -3  operator uvedl v odpovedi vice hodnot nez    *
C                        povoluje parametr IDIM,                      *
C             IOUT = -2  zakoncovaci odpoved (klavesa CTRL Z)         *
C             IOUT = -1  prvni znak odpovedi je " ? " (nouzova odpo-  *
C                        ved),                                        *
C             IOUT =  0  prazdna odpoved,                             *
C             IOUT >  0  skutecny pocet zpracovanych hodnot.          *
C    POLE   - pole, do ktereho byly ulozeny hodnoty, ktere zapsal     *
C             uzivatel po vyzve procedury. Tvori-li odpoved mene nez  *
C             IDIM hodnot, je zbyvajicim prvkum pole prirazena hodno- *
C             ta 0. Je-li IOUT rovno nektere z hodnot (-3, -1, 0),    *
C             zustane obsah pole nezmenen; hodnoty prvku pole jsou    *
C             stejne jako pred volanim procedury.                     *
C EXTERNI PROCEDURY A FUNKCE: TTASKS, TTLENF, TTASKU                  *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
C OBLAST COMMON:
      CHARACTER   LPOLE*80
      COMMON      /TTASKW/ LPOLE
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      CALL TTASKU(ITT,TEXT,IOUT,IDIM)
      IF(IOUT.LE.0)  RETURN
      READ(LPOLE,100,ERR=200)  (POLE(I),I=1,IDIM)
  100 FORMAT(40D30.0)
      RETURN
  200 IOUT = -4
      RETURN
C
      END
