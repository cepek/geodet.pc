C+
C TTASKR
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro realizaci dialogu (odpoved: REAL)              *
C AUTOR: Ales Cepek, VUGTK                                            *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE TTASKR(ITT,TEXT,IOUT,POLE,IDIM)
      INTEGER     ITT,IOUT,IDIM
      CHARACTER   TEXT(1)
      REAL*4      POLE(IDIM)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura realizuje dialog s numerickou odpovedi (pro typ   *
C    parametru POLE) - funkce a popis parametru viz proceduru TTASKD. *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      CHARACTER   LPOLE*80
      COMMON      /TTASKW/ LPOLE
C-
      CALL TTASKU(ITT,TEXT,IOUT,IDIM)
      IF(IOUT.LE.0)  RETURN
      READ(LPOLE,100,ERR=200)  (POLE(I),I=1,IDIM)
  100 FORMAT(40F13.0)
      RETURN
  200 IOUT = -4
      RETURN
C
      END
