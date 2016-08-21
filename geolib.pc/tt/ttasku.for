C+
C TTASKU
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro predzpracovani odpovedi                        *
C AUTOR: Ales Cepek, VUGTK                                            *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE TTASKU(ITT,TEXT,IOUT,IDIM)
      INTEGER    ITT, IOUT, IDIM
      CHARACTER  TEXT(1)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura TTASKU vola proceduru TTASKS, ktera vypise text   *
C    vyzvy na zvolenem logickem zarizeni, precte odpoved operatora    *
C    (80 znaku) a ulozi ji do oblasti COMMON /TTASKW/. Procedura      *
C    TTASKU dale zpracuje mezery pouzite ve funkci oddelovace         *
C    numerickych udaju a urci skutecny pocet udaju v odpovedi.        *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    ITT    - cislo logicke jednotky pro vypis vyzvy a nacteni odpo-  *
C             vedi.                                                   *
C    TEXT   - retezec obsahujici text vyzvy.                          *
C    IDIM   - maximalni pocet udaju, ktere mohou tvorit odpoved uzi-  *
C             vatele.                                                 *
C VYSTUPNI PARAMETRY:                                                 *
C    IOUT   - IOUT = -3  odpoved uzivatele obsahuje vice nez IDIM     *
C                        hodnot,                                      *
C             IOUT = -2  zakoncovaci odpoved,                         *
C             IOUT = -1  prvni znak odpovedi je "?" (nouzova odpoved),*
C             IOUT =  0  prazdna odpoved,                             *
C             IOUT >  0  skutecny pocet hodnot v odpovedi.            *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
C OBLAST COMMON:
      INTEGER    ID80
      PARAMETER (ID80 = 80)
      CHARACTER  POLE(ID80)
      COMMON     /TTASKW/ POLE
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      CHARACTER  ODDEL
      PARAMETER (ODDEL=',')
      CHARACTER  ZNAK
C
      CALL TTASKS(ITT,TEXT,IOUT,POLE,ID80)
      IF(IOUT.LE.0)  RETURN
C
C NAHRADA ZNAKU TABELATOR MEZERAMI
      DO 10 I=1,ID80
10       IF(POLE(I).EQ.'	')   POLE(I) = ' '
C
C SKUTECNY POCET UDAJU V ODPOVEDI
      IODP = 0
C
C VYNECHANI UVODNICH MEZER
      IND = 1
0102  IF(IND.GT.ID80)  GOTO 0199
      IF(POLE(IND).NE.' ')  GOTO 0199
         POLE(IND) = ODDEL
         IND = IND + 1
         GOTO 0102
0199  CONTINUE
      IZN = 0
C
C ZPRACOVANI VSTUPNIHO RETEZCE
0202  IF(IND.GT.ID80)  GOTO 0299
         IODP = IODP + 1
0502     IF(IND.GT.ID80)  GOTO 0599
         IF(POLE(IND).EQ.' ')  GOTO 0599
            ZNAK = POLE(IND)
            IF(ZNAK.EQ.',')  IODP = IODP + 1
            POLE(IND) = ODDEL
            IND = IND + 1
            IZN = IZN + 1
            POLE(IZN) = ZNAK
            GOTO 0502
0599     CONTINUE
0303     IF(IND.GT.ID80)  GOTO 0399
            IZN = IZN + 1
            POLE(IZN) = ODDEL
            POLE(IND) = ODDEL
            IND = IND + 1
0402        IF(IND.GT.ID80)  GOTO 0499
            IF(POLE(IND).NE.' ')  GOTO 0499
               POLE(IND) = ODDEL
               IND = IND + 1
               GOTO 0402
0499        CONTINUE
0399     CONTINUE
         GOTO 0202
0299  CONTINUE
C
      IF(IODP.LE.IDIM)  THEN
         IOUT = IODP
      ELSE
         IOUT = -3
      END IF
      RETURN
C
      END
