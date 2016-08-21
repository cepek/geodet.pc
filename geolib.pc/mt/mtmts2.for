C+
C MTMTS2
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro tisk trojuhelnikove matice                     *
C AUTOR: Eliska Valkova, VUGTK, nyni RIS hl. m. Prahy                 *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MTMTS2 (NAZEV,N,A,IZ,STR)
      CHARACTER*(*) NAZEV
      LOGICAL STR
      DIMENSION A(1)
      INTEGER N,IZ
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura tiskne volitelnym zarizenim dolni trojuhelnikovou *
C    cast ctvercove matice stupne N, ulozenou po radcich ve zpravidla *
C    jednorozmernem poli. Forma tisku je popsana v textove casti do-  *
C    kumentace procedury MTMTS2 (odst. 2.).                           *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    NAZEV  - znakove pole nebo textovy retezec obsahujici alfanume-  *
C             ricky text, ktery ma byt uveden v zahlavi tisku         *
C    N      - stupen matice                                           *
C    A      - pole, zpravidla jednorozmerne, v nemz je po radcich     *
C             ulozena dolni trojuhelnikova cast matice urcena k tisku *
C    IZ     - cislo logicke jednotky, kterou bude matice tistena      *
C    STR    - specifikace pozadavku na posun papiru v tiskarne; je-li *
C             STR = .TRUE., bude pred tiskem papir posunut na pocatek *
C             nove stranky, v opacnem pripade (STR = .FALSE.) je pre- *
C             chod na novou stranku potlacen                          *
C ZOBRAZENI PRVKU MATICE S JEDNODUCHOU DELKOU SLOVA                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
C TISK NA NOVOU STRANKU KDYZ STR=.TRUE.
      IF (STR) WRITE (IZ,10)
   11 WRITE (IZ,'(1X,A\)') NAZEV
      M=1
      DO 3 I=1,N
      WRITE(IZ,4)I
    4 FORMAT (//I5\)
      DO 3 J=1,I
      IF (MOD(J,5).EQ.1) WRITE (IZ,5)
    5 FORMAT (1H )
      WRITE (IZ,6) J,A(M)
    6 FORMAT (I5,E17.7\)
    3 M=M+1
      WRITE (IZ,5)
      RETURN
   10 FORMAT (1H1)
      END
