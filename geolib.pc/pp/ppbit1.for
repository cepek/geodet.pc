C+
C PPBIT1 PPBIC1 PPBIS1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedury pro bitove operace                                 *
C AUTOR: Ales Cepek, VUGTK                                            *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      INTEGER FUNCTION PPBIT1(ZONA,BIT)
      INTEGER*1  ZONA(1)
      INTEGER    BIT
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedury umoznuji provadet tri bitove operace na volitel-  *
C    nych bitech v uzivatelem specifikovane zone. Uzivatelskou zonou  *
C    rozumime posloupnost sousedicich slabik (bytu) v pameti; jedno-  *
C    tlivym bitum prisluseji poradova cisla 0, 1, 2, ... .  Funkcni   *
C    procedura PPBIT1 testuje obsah oznaceneho bitu v uzivatelske zo- *
C    ne, procedura PPBIC1 nuluje oznaceny bit a procedura PPBIS1  na- *
C    stavuje vybrany bit na hodnotu 1.                                *
C VYVOLANI PROCEDUR:                                                  *
C      INTEGER  PPBIT1                                                *
C         ......................                                      *
C      ....  PPBIT1(ZONA,BIT)  ....                                   *
C      CALL  PPBIC1(ZONA,BIT)                                         *
C      CALL  PPBIS1(ZONA,BIT)                                         *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETR:                                                   *
C    BIT    - poradove cislo bitu v oblasti ZONA                      *
C VSTUPNI/VYSTUPNI PARAMETR:                                          *
C    ZONA   - oblast v uzivatelskem programu, ve ktere bude provedena *
C             prislusna bitova operace                                *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
C
      INTEGER INDEX, IBIT
      LOGICAL BTEST
C
      INDEX = BIT/8 + 1
      IBIT  = MOD(BIT,8)
      IF(BTEST(ZONA(INDEX),IBIT)) THEN
         PPBIT1 = 1
      ELSE
         PPBIT1 = 0
      END IF
      RETURN
      END
C
      SUBROUTINE PPBIS1(ZONA,BIT)
      INTEGER*1  ZONA(1)
      INTEGER    BIT
      INTEGER INDEX, IBIT
C      
      INDEX = BIT/8 + 1
      IBIT  = MOD(BIT,8)
      ZONA(INDEX) = IBSET(ZONA(INDEX),IBIT)
      RETURN
C
      ENTRY PPBIC1(ZONA,BIT)
      INDEX = BIT/8 + 1
      IBIT  = MOD(BIT,8)
      ZONA(INDEX) = IBCLR(ZONA(INDEX),IBIT)
      RETURN
C
      END
