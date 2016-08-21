C+
C MTMTSA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro tisk matice                                    *
C AUTOR: Eliska Valkova, VUGTK, nyni RIS hl. m. Prahy                 *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE MTMTSA (NAZEV,RAD,SLOUP,A,IZ,STR)
	CHARACTER*(*) NAZEV
        LOGICAL STR
	DOUBLE PRECISION A(1)
	INTEGER RAD,SLOUP,IZ
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Viz proceduru MTMTS1.                                       *
C PREHLED PARAMETRU - viz proceduru MTMTS1                            *
C ZOBRAZENI PRVKU MATICE S DVOJNASOBNOU DELKOU SLOVA                  *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
C TISK NA NOVOU STRANKU,KDYZ STR=.TRUE.
      IF (STR) WRITE (IZ,10)
   11 WRITE (IZ,'(1X,A\)') NAZEV
      IF ((RAD.NE.1).AND.(SLOUP.NE.1)) GO TO 20
      WRITE (IZ,'(/)')
      DO 21 I=1,RAD*SLOUP
   21 WRITE (IZ,22)I,A(I)
   22 FORMAT (I5,E22.14)
      RETURN
   20 M=1
      DO 3 I=1,RAD
      WRITE (IZ,4)I
    4 FORMAT (//I5\)
      DO 3 J=1,SLOUP
      IF (MOD(J,5).EQ.1) WRITE (IZ,6)
    6 FORMAT (1H )
      WRITE (IZ,7)J,A(M)
    7 FORMAT (I5,E22.14\)
    3 M=M+1
      WRITE (IZ,6)
      RETURN
   10 FORMAT (1H1)
      END
