C+
C TTASK
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro realizaci dialogu (vyzva 1. a 2. typu)         *
C AUTOR: Ales Cepek, VUGTK                                            *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE TTASK(ITT,TEXT,IOUT)
      INTEGER    ITT, IOUT
      CHARACTER  TEXT(1)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura TTASK vypisuje na obrazovce text vyzvy  1. nebo   *
C    2. typu, cte odpoved vlozenou operatorem prostrednictvim klaves- *
C    nice, analyzuje ji a vysledek analyzy, vyjadreny hodnotou        *
C    vystupniho parametru predava volajicimu programu.                *
C                                                                     *
C    Priklad vyzvy 1. typu:                                           *
C                                                                     *
C      CALL TTASK(5,'/A/POZADUJES TISK MEZIVYSLEDKU? (A/N): ',IOUT)   *
C                                                                     *
C    Priklad vyzvy 2. typu:                                           *
C                                                                     *
C      CALL TTASK(5,'ZVOL CINNOST (1-8): ',IOUT)                      *
C                                                                     *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    ITT    - cislo logicke jednotky, na kterou bude vypsan text      *
C             vyzvy a ze ktere bude prectena odpoved uzivatele.       *
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
C VYSTUPNI PARAMETR:                                                  *
C    IOUT   - IOUT = -4  nepripustna odpoved:                         *
C                        - vyzva 1. typu - odpoved neni shodna        *
C                                          s zadnym klicem,           *
C                        - vyzva 2. typu - odpoved je cislo mimo      *
C                                          zadany interval nebo neni  *
C                                          typu INTEGER,              *
C             IOUT = -3  uzivatel odpovedel vice nez jednou hodnotou  *
C                        (pouze pro vyzvu 2. typu),                   *
C             IOUT = -2  zakoncovaci odpoved (klavesa CTRL Z),        *
C             IOUT = -1  prvni vyznamny znak zapsany uzivatelem na    *
C                        ITT je " ? "  (nouzova odpoved),             *
C             IOUT =  0  uzivatel odpovedel pouze stisknutim klavesy  *
C                        RETURN  (prazdna odpoved),                   *
C             IOUT >  0  pripustna odpoved:                           *
C                        - vyzva 1. typu - odpoved je shodna s        *
C                                          IOUT-tym klicem seznamu,   *
C                        - vyzva 2. typu - cislo, kterym odpovedel    *
C                                          uzivatel na vyzvu programu.*
C EXTERNI PROCEDURY A FUNKCE: TTASKN, TTASKS, TTLENF, TTASKU          *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      CHARACTER*80  ODP, VYZVA
      CHARACTER     ZNAK
      INTEGER       TTLENF
C
      VYZVA = ' '
      N = TTLENF(TEXT)
      DO 100 I=1,N
100      VYZVA(I:I) = TEXT(I)
      I1 = 0
      I2 = 0
      DO 110 I=1,LEN(VYZVA)
         ZNAK = VYZVA(I:I)
	 IF(ZNAK.EQ.'(')  I1 = I
	 IF(ZNAK.EQ.')')  I2 = I
110   CONTINUE	
      IN = I2 - I1 - 1
      IF(I1.EQ.0.OR.I2.EQ.0.OR.IN.LE.0) THEN
         IOUT = -4
         RETURN
      END IF
C
      VYZVA(:I1) = ' '
      VYZVA(I2:) = ' '
      J = 0
      DO 200 I=1,LEN(VYZVA)
         ZNAK = VYZVA(I:I)
         IF(ZNAK.NE.' ') THEN
            VYZVA(I:I) = ' '
            J = J + 1
            VYZVA(J:J) = ZNAK
         END IF
200   CONTINUE
C
      IF(VYZVA(1:2).EQ.'1-') THEN
         IF(VYZVA(3:).EQ.' ') THEN
            IOUT = -4
            RETURN
         END IF
         N = INDEX(VYZVA,' ')
         VYZVA(1:2) = ' '
         VYZVA(N:N) = ','
         READ(VYZVA,1,ERR=2)  MAXN
1        FORMAT(I80)
         IF(MAXN.GT.0) THEN
            CALL TTASKN(ITT,TEXT,IOUT,N,1)
            IF(IOUT.LE.0)  RETURN
            IF(N.GE.1.AND.N.LE.MAXN) THEN
               IOUT = N
               RETURN
            END IF
         END IF
2        IOUT = -4
      ELSE
         CALL TTASKS(ITT,TEXT,IOUT,ODP,LEN(ODP))
         IF(IOUT.LE.0)  RETURN
         K = INDEX(VYZVA,' ')
         VYZVA(K:) = '/'
C KONVERZE MALYCH PISMEN NA VELKA (ASCII)
         DO 305 I=1,LEN(VYZVA)
         IF(ODP  (I:I).GE.'a')  ODP  (I:I) = CHAR(ICHAR(ODP  (I:I))-32)
305      IF(VYZVA(I:I).GE.'a')  VYZVA(I:I) = CHAR(ICHAR(VYZVA(I:I))-32)
         IOUT = 1
         I1 = 1
300      I2 = INDEX(VYZVA,'/')
         IF(I2.EQ.0) THEN
            IOUT = -4
            RETURN
         END IF
         IF(VYZVA(I1:I2-1).EQ.ODP) RETURN
         IOUT = IOUT + 1
         VYZVA(I1:I2) = ' '
         I1 = I2 + 1
         GOTO 300
      END IF
C
      END
