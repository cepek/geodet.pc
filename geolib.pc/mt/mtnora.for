C+
C MTNORA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Funkcni procedura pro vypocet kritickych hodnot normalniho   *
C        rozdeleni                                                    *
C AUTOR: Ales Cepek, VUGTK                                            *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      DOUBLE PRECISION FUNCTION MTNORA(ALFA)
      DOUBLE PRECISION ALFA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Pro zadanou pravdepodobnost pocita procedura kritickou hod- *
C    notu normovaneho normalniho rozdeleni. Vysledek vypoctu je pre-  *
C    davan jako funkcni hodnota typu DOUBLE PRECISION.                *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETR:                                                   *
C    ALFA   - pravdepodobnost, pro kterou se urcuje kriticka hodnota; *
C             hodnota parametru ALFA musi lezet v otevrenem intervalu *
C             (0, 1); procedura netestuje pripustnost hodnoty aktual- *
C             niho parametru                                          *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C EXTERNI PROCEDURA: MTNORB                                           *
C POZNAMKA: Jmeno funkcni procedury MTNORA musi byt ve volajicim      *
C    programu deklarovano jako promenna typu DOUBLE PRECISION.        *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      DOUBLE PRECISION A,Z,G,F
C      
      A=ALFA
      IF(A-0.5D0)2,2,1
    1 A=0.1D1-A
    2 Z=DSQRT(-0.2D1*DLOG(A))
      Z=Z-((0.747395D1*Z+0.494877D3)*Z+0.163772D4)/
     /(((Z+0.1179407D3)*Z+0.908401D3)*Z+0.659935D3)
      CALL MTNORB(Z,.TRUE.,F,G)
      F=(F-A)/G
      MTNORA=(((((0.75D0*Z*Z+0.875D0)*F+Z)*Z+
     /0.5D0)*F/0.3D1+0.5D0*Z)*F+0.1D1)*F+Z
      IF(ALFA.GT.0.5D0)MTNORA=-MTNORA
      RETURN
      END
