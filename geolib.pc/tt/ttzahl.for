C+
C TTZAHL
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypis zahlavi odstavcu textu na terminal       *
C AUTOR: Ales Cepek, VUGTK                                            *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE TTZAHL(ITT,TEXT)
      CHARACTER  TEXT(1)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura tiskne specifikovany retezec na volitelne logicke *
C    zarizeni (obrazovku) - retezec musi byt ukonce dvojteckou, ktera *
C    se netiskne nebo znakem NULL. Retezec je doplnen zleva, resp.    *
C    zprava retezcem '******  ' resp. '  ******' a je posunut na      *
C    stred obrazovky. Pred  vypisovanym  retezcem  je  vynechan  je-  *
C    den prazdny radek, za retezcem dva prazdne radky.                *
C VSTUPNI PARAMETRY:                                                  *
C    ITT    - cislo logickeho zarizeni (obrazovka)                    *
C    TEXT   - znakovy retezec                                         *
C EXTERNI FUNKCE: TTLENF                                              *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
C
      INTEGER    TTLENF
      CHARACTER  PTEXT(8)
      DATA       PTEXT /6*'*',' ',' '/
C
      I = MIN0(60,TTLENF(TEXT))
      J = (62-I)/2
      WRITE(ITT,1)  (PTEXT(8), L=1,J),
     /              (PTEXT(L), L=1,8),
     /              ( TEXT(L), L=1,I),
     /              (PTEXT(L), L=8,1,-1)
1     FORMAT(/1X,80A1)
      WRITE(ITT,1)
C
      END
