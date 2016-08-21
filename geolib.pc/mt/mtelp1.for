C+
C MTELP1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet parametru stredni elipsy chyb          *
C AUTOR: Jaroslava Horejcova, VUGTK, nyni Geodezie, s. p., Praha,     *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE MTELP1 (CYY,CYX,CXX,A,B,ALFA,IER)
	INTEGER IER
	REAL    CYY,CYX,CXX,A,B,ALFA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura slouzi k vypoctu delky poloos a smerniku hlavni   *
C    poloosy stredni elipsy chyb pro bod o souradnicich (Y, X), je-   *
C    jichz kovariancni matice C je dana. Matice C musi byt pozitivne  *
C    definitni nebo semidefinitni.                                    *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    CYY,CYX,CXX - prvky kovariancni matice C                         *
C VYSTUPNI PARAMETRY:                                                 *
C    A, B   - poloosy stredni elipsy chyb                             *
C    ALFA   - smernik hlavni poloosy stredni elipsy chyb              *
C    IER    - IER = 0  regularni uloha                                *
C             IER = 1  matice C neni pozitivne (semi)definitni        *
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      IER=0
      C=SQRT((CXX-CYY)**2+4*CYX*CYX)
      B=(CYY+CXX-C)/2
      IF(B.LT.0) GO TO 20
      A=SQRT(B+C)
      B=SQRT(B)
      IF(C.EQ.0) GO TO 10
      ALFA=ATAN2(2*CYX,CXX-CYY)/2
      IF(ALFA.LT.0) ALFA=ALFA+ATAN2(0.,-1.)
      RETURN
10    ALFA=0
      RETURN
20    IER=1
      RETURN
      END
