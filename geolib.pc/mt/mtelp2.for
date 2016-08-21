C+
C MTELP2
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet parametru stredni elipsy chyb          *
C AUTOR: Ales Cepek, VUGTK,                                           *
C        Frantisek Charamza, VUGTK                                    * 
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE MTELP2 (CYY,CYX,CXX,A,B,ALFA,IER)
	INTEGER IER   
	REAL    CYY,CYX,CXX,A,B,ALFA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura MTELP2, stejne jako procedura MTELP1, slouzi k    *
C    vypoctu delky poloos a smerniku hlavni poloosy stredni elipsy    *
C    chyb pro bod o souradnicich (Y, X), jejichz pozitivne definitni  *
C    nebo semidefinitni kovariancni matice C je dana. Na rozdil od    *
C    MTELP1 neproveruje procedura MTELP2 zminenou vlastnost matice C  *
C    a tim umoznuje urcovat parametry stredni elipsy chyb i v tech    *
C    pripadech, kdy je tato vlastnost porusena v dusledku konecne     *
C    presnosti uzite aritmetiky.                                      *
C PREHLED PARAMETRU - specifikace i vyznam parametru jsou stejne jako *
C                     u procedury MTELP1; vyjimku tvori parametr IER, *
C                     jehoz hodnota je pri vystupu z procedury vzdy   *
C                     rovna nule                                      *
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      IER=0
      C=SQRT((CXX-CYY)**2+4*CYX*CYX)
      B=(CYY+CXX-C)/2
      IF(B.LT.0E0) B=0E0
      A=SQRT(B+C)
      B=SQRT(B)
      IF(C.EQ.0E0) GO TO 10
      ALFA=ATAN2(2*CYX,CXX-CYY)/2
      IF(ALFA.LT.0E0) ALFA=ALFA+ATAN2(0.,-1.)
      RETURN
10    ALFA=0
      RETURN
      END
