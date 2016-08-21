C+
C TTASKS
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro realizaci dialogu (odpoved: znakovy retezec)   *
C AUTOR: Ales Cepek, VUGTK                                            *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE TTASKS(ITT,TEXT,IOUT,LPOLE,IDIM)
      INTEGER    ITT,IOUT,IDIM
      CHARACTER  TEXT(1),LPOLE(IDIM)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura je soucasti souboru procedur TTASKx, zabezpecuji- *
C    cich standardni funkce v programech uloh dialogoveho typu (tisk  *
C    vyzvy, cteni a analyza odpovedi apod.). TTASKS vypisuje na obra- *
C    zovce terminalu zadany text vyzvy, cte odpoved vlozenou operato- *
C    rem z klavesnice, analyzuje ji a predava volajicimu programu.    *
C    Odpoved tvori znakovy retezec (jeden radek textu); pocet ctenych *
C    a zpracovavanych znaku je volitelny. Precteny text se uklada do  *
C    do vystupniho pole. Pocet vyznamnych znaku v konkretni odpovedi  *
C    predava procedura TTASKS prostrednictvim dalsiho parametru,      *
C    kterym alternativne signalizuje i pripadny vyskyt nektere z tzv. *
C    globalnich odpovedi (viz dokumentaci procedury TTASK). Za nevyz- *
C    namne znaky jsou v teto souvislosti povazovany mezery na konci   *
C    znakoveho retezce.                                               *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    ITT    - cislo logicke jednotky, na kterou ma byt vypsan text    *
C             vyzvy a ze ktere ma byt prectena odpoved operatora.     *
C    TEXT   - retezec (text vyzvy), ktery ma byt vypsan na zarizeni   *
C             ITT. Text vyzvy se uvozuje a zakoncuje znakem " ' ".    *
C             Poslednim vyznamnym znakem vyzvy musi byt znak ":".     *
C             V textu vyzvy muze byt uvedena implicitni hodnota       *
C             odpovedi (na obrazovku se vypisuje za znakem ":" ukon-  *
C             cujicim text vyzvy jedna mezera a je-li specifikovana   *
C             implicitni hodnota odpovedi - kurzor je nastaven na     *
C             prvni znak implicitni odpovedi). Nepovinna implicitni   *
C             hodnota odpovedi se uvadi uzavrena mezi dvema znaky     *
c             "/" na zacatku retezce (od prvni pozice).               *
C    IDIM   - pocet znaku, ktere procedura TTASKS precte ze zarizeni  *
C             ITT (IDIM<=255). Zapise-li operator na obrazovku vice   *
C             nez IDIM znaku, budou prebytecne znaky ztraceny. Pokud  *
C             bylo zapsano mene nez IDIM znaku, bude retezec zprava   *
C             doplnen mezerami.                                       *
C VYSTUPNI PARAMETRY:                                                 *
C    LPOLE  - pole, do ktereho procedura ulozi precteny text odpovedi.*
C    IOUT   - IOUT = -4  chyba pri cteni (prikaz READ identifikoval   *
C                        chybu dat, doslo ke skoku na navesti         *
C                        ERR=...),                                    *
C             IOUT = -2  zakoncovaci odpoved (v procedure TTASKS byl  *
C                        pri provadeni prikazu READ zjisten konec     *
C                        dat, rizeni bylo predano na navesti END=...),*
C             IOUT = -1  prvni vyznamny znak odpovedi je "?" (nouzova *
C                        odpoved),                                    *
C             IOUT =  0  prazdna odpoved,                             *
C             IOUT >  0  pocet vyznamnych znaku tvoricich odpoved.    *
C EXTERNI: TTLENF                                                     *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
C OBLAST COMMON:
      CHARACTER*80  PTEXT
      COMMON     /TTASKW/ PTEXT
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      INTEGER    TTLENF
      CHARACTER*80  IMPL
C
      NTXT = TTLENF(TEXT)
      IF(TEXT(1).EQ.'/') THEN
         IMPL = ' '
         IPOC = 0
         DO 110 I=2,NTXT
            IF(TEXT(I).EQ.'/')  GOTO 119
            IPOC = IPOC + 1
110         IMPL(IPOC:IPOC) = TEXT(I)
119      CONTINUE
         IZAC = IPOC + 3
      ELSE
         IMPL = ' '
         IPOC = 1
         IZAC = 1
      END IF
C
C ESCAPE SEQUENCE
      IMPL(IPOC+1:IPOC+1) = CHAR(27)
      IMPL(IPOC+2:IPOC+2) = CHAR(91)
      WRITE(IMPL(IPOC+3:IPOC+4),'(I2.2)')  IPOC
      IMPL(IPOC+5:IPOC+5) = CHAR(68)
      NPOC = IPOC + 5
C
      PTEXT = ' '
      N = 0
      DO 100 J=IZAC,NTXT
         N = N + 1
100      PTEXT(N:N) = TEXT(J)
      WRITE(ITT,300)  PTEXT(:N), IMPL(:NPOC)
300   FORMAT(1X,A,': ',A\)
400   READ (ITT,301,ERR=500,END=600)  (LPOLE(J),J=1,IDIM)
301   FORMAT(255A1)
      IOUT = 0
      DO 450 I=1,IDIM
450      IF(LPOLE(I).NE.' ')  IOUT = I
      N = IOUT + 1
      DO 330 I=N,IPOC
         IF(LPOLE(I).EQ.' ')  LPOLE(I) = IMPL(I:I)
330      IF(LPOLE(I).NE.' ')  IOUT = I
      IF(LPOLE(1).EQ.'?')   IOUT = -1
      RETURN
500   IOUT = -4
      RETURN
600   IOUT = -2
      RETURN
C
      END
