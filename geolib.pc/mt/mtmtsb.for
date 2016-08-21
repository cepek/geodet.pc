C+
C MTMTSB
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro tisk trojuhelnikove matice                     *
C AUTOR: Eliska Valkova, VUGTK, nyni RIS hl. m. Prahy                 *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MTMTSB (NAZEV,N,A,IZ,STR)
      CHARACTER*(*) NAZEV
      LOGICAL STR
      DOUBLE PRECISION A(1)
      INTEGER N,IZ
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Viz proceduru MTMTS2.                                       *
C PREHLED PARAMETRU - viz proceduru MTMTS2                            *
C ZOBRAZENI PRVKU MATICE S DVOJNASOBNOU DELKOU SLOVA                  *
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
    6 FORMAT (I5,E22.14\)
    3 M=M+1
      WRITE (IZ,5)
      RETURN
   10 FORMAT (1H1)
      END
