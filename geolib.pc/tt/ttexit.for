C+
C TTEXIT
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro ukonceni programu                              *
C AUTOR: Ales Cepek, VUGTK                                            *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE TTEXIT(ITT,JMENO)
      INTEGER  ITT
      CHARACTER JMENO*6
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura tiskne na volitelne logicke zarizeni (terminal)   *
C    zpravu o ukonceni cinnosti programu a provede prikaz STOP.       *
C    Soucasti zpravy o ukonceni cinnosti programu je jmeno programu   *
C    a cislo programove verze.                                        *
C POZADAVKY NA VOLAJICI PROGRAM:                                      *
C    Ve volajicim programu musi byt deklarovana spolecna oblast       *
C    COMMON /TTEXIW/ s delkou 6 bytu a pred vyvolanim procedury       *
C    naplnena retezcem, reprezentujicim cislo verze programu.         *
C VSTUPNI PARAMETRY:                                                  *
C    ITT    - cislo logicke jednotky, na kterou bude vypsana zprava   *
C             "PROGRAM jmenop verzep", kde jmenop je 6 znaku ASCII    *
C             uvedenych v parametru JMENO a verzep je sestiznakovy    *
C             retezec ulozeny volajicim programem v oblasti           *
C             COMMON /TTEXIW/ ("cislo verze").                        *
C    JMENO  - retezec obsahujici jmeno programu.                      *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
C OBLAST COMMON:
      CHARACTER VERZE*6
      COMMON /TTEXIW/ VERZE
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      WRITE(ITT,1) JMENO, VERZE
1     FORMAT(// ' PROGRAM ',A,' ',A,' '/)
      STOP
C
      END
