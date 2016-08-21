C+
C MTMTS1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro tisk matice                                    *
C AUTOR: Eliska Valkova, VUGTK, nyni RIS hl. m. Prahy                 *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE MTMTS1 (NAZEV,RAD,SLOUP,A,IZ,STR)
	CHARACTER*(*) NAZEV
        LOGICAL STR
	DIMENSION A(1)
	INTEGER RAD,SLOUP,IZ
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura tiskne volitelnym zarizenim matici typu (M x N)   *
C    ulozenou po radcich v urcenem, zpravidla jednorozmernem poli.    *
C    Forma tisku je popsana v textove casti dokumentace procedury     *
C    MTMTS1 (odst. 2.).                                               *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    NAZEV  - znakove pole nebo textovy retezec obsahujici alfanume-  *
C             ricky text, ktery ma byt uveden v zahlavi tisku         *
C    RAD    - pocet radku matice                                      *
C    SLOUP  - pocet sloupcu matice                                    *
C    A      - pole, zpravidla jednorozmerne, v nemz je po radcich     *
C             ulozena dana matice; je-li tistena matice ulozena ve    *
C             vicerozmernem poli, prostuduj 1. odst. textove dokumen- *
C             tace procedury MTMCT1                                   *
C    IZ     - cislo logicke jednotky, kterou bude matice tistena      *
C    STR    - specifikace pozadavku na posun papiru v tiskarne; je-li *
C             STR = .TRUE., bude pred tiskem papir posunut na pocatek *
C             nove stranky, v opacnem pripade (STR = .FALSE.) je pre- *
C             chod na novou stranku potlacen                          *
C ZOBRAZENI PRVKU MATICE S JEDNODUCHOU DELKOU SLOVA                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
C TISK NA NOVOU STRANKU,KDYZ STR=.TRUE.
      IF (STR) WRITE (IZ,10)
   11 WRITE (IZ,'(1X,A\)') NAZEV
      IF ((RAD.NE.1).AND.(SLOUP.NE.1)) GO TO 20
      WRITE(IZ,'(/)')
      DO 21 I=1,RAD*SLOUP
   21 WRITE (IZ,22)I,A(I)
   22 FORMAT (I5,E17.7)
      RETURN
   20 M=1
      DO 3 I=1,RAD
      WRITE (IZ,4)I
    4 FORMAT (//I5\)
      DO 3 J=1,SLOUP
      IF (MOD(J,5).EQ.1) WRITE (IZ,6)
    6 FORMAT (1H )
      WRITE (IZ,7)J,A(M)
    7 FORMAT (I5,E17.7\)
    3 M=M+1
      WRITE (IZ,6)
      RETURN
   10 FORMAT (1H1)
      END
