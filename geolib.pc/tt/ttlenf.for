C+
C TTLENF
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro urceni delky retezce                           *
C AUTOR: Ales Cepek, VUGTK                                            *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      INTEGER FUNCTION TTLENF(RET)
      CHARACTER RET(1)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: TTLENF je celociselna funkcni procedura, ktera urcuje delku *
C    znakoveho retezce ukonceneho dvojteckou (":") nebo znakem NULL   *
C    (byte, jehoz binarni hodnota je 0). Znak ":" ma pri urcovani     *
C    delky retezce prednost pred znakem NULL. Zakoncovaci znak se     *
C    nepocita do delky retezce.                                       *
C VSTUPNI PARAMETR:                                                   *
C    RET    - alfanumericky retezec ukonceny dvojteckou nebo znakem   *
C             NULL.                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
C
      I=0
100   I = I + 1
      IF (I.EQ.81)  GOTO 199
      IF (ICHAR(RET(I)).EQ.0) GOTO 199
      IF (RET(I).NE.':') GOTO 100
199   TTLENF = I - 1
      RETURN
C
      END
