C+
C NGPTVA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet souradnic bodu protinanim vpred        *
C AUTOR: Frantisek Charamza, VUGTK,                                   *
C        Eliska Valkova, VUGTK, nyni RIS hl. m. Prahy                 *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE NGPTVA (Y,X,SIGMA,IER)
      DOUBLE PRECISION Y(3),X(3),SIGMA(2)
      INTEGER          IER
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura vypocte souradnice bodu protinanim vpred ze smer- *
C    niku merenych na dvou danych bodech. Pokud neexistuje prusecik   *
C    urcujicich smeru (orientovanych poloprimek), vypocte procedura   *
C    souradnice pruseciku primek do smeru vlozenych (tj. souradnice   *
C    fiktivniho pruseciku smeru).                                     *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETR:                                                   *
C    SIGMA  - pole pro ulozeni urcujicich smerniku (v obloukove mire) *
C VYSTUPNI PARAMETR:                                                  *
C    IER    - IER = 0  regularni uloha                                *
C             IER = 1  singularita - uhel protinani je mensi nez pri- *
C                                    blizne 10 gradovych stupnu       *
C             IER =-1  singularita - neexistuje prusecik urcujicich   *
C                                    smeru                            *
C                                  - smery se protinaji v nekterem    *
C                                    z danych bodu                    *
C                                  - dane body jsou totozne           *
C VSTUPNI/VYSTUPNI PARAMETRY:                                         *
C    Y,X    - pole pro ulozeni souradnic; souradnice danych bodu musi *
C             byt pred vyvolanim procedury ulozeny do pozic Y(1),     *
C             X(1), Y(2), X(2); vypoctene souradnice ulozi procedura  *
C             do promennych Y(3), X(3)                                *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      DOUBLE PRECISION S(2),C(2),CS,DY
      REAL*8           JM
C      
      IER=0
      DO 1 I=1,2
      S(I)=DSIN(SIGMA(I))
    1 C(I)=DCOS(SIGMA(I))
      I=1
      IF (DABS(S(2)).LT.DABS(S(1))) I=2
      J=3-I
      CS=C(I)*S(J)
      JM=CS-(S(I)*C(J))
      IF (DABS(JM).LT.0.15) GO TO 5
      DY=(S(1)*S(2)*(X(J)-X(I))-CS*(Y(J)-Y(I)))/JM
      Y(3)=Y(J)+DY
      X(3)=X(J)+(DY*C(J))/S(J)
      IF (DY*S(J).LE.0) IER=-1
      IF ((X(3)-X(I))*C(I).LE.0) IER=-1
      RETURN
    5 IER=1
      END
