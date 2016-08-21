C+
C NGPRP1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet souradnic pruseciku dvou primek        *
C AUTOR: Frantisek Charamza, VUGTK,                                   *
C        Eliska Valkova, VUGTK, nyni RIS hl. m. Prahy                 *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE NGPRP1 (Y,X,ITYP,IER)
      INTEGER   ITYP,IER
      DIMENSION Y(5),X(5)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura vypocte souradnice pruseciku dvou primek, danych  *
C    rovinnymi souradnicemi dvojic bodu P1 (Y1,X1), P2 (Y2,X2) a      *
C    P3 (Y3,X3), P4 (Y4,X4).                                          *
C PREHLED PARAMETRU                                                   *
C VYSTUPNI PARAMETRY:                                                 *
C    ITYP   - parametr, kterym procedura blize urcuje polohu pruseci- *
C             ku; hodnota parametru je rovna                          *
C             ITYP = 0  lezi-li prusecik vne usecek P1P2 i P3P4       *
C             ITYP = 1  lezi-li prusecik na usecce P1P2 vne usecky    *
C                       P3P4                                          *
C             ITYP = 2  lezi-li prusecik na usecce P3P4 vne usecky    *
C                       P1P2                                          *
C             ITYP = 3  lezi-li prusecik na usecce P1P2 i P3P4        *
C    IER    - IER = 0  regularni uloha                                *
C             IER = 1  singularita - body jedne nebo obou urcujicich  *
C                      dvojic jsou totozne                            *
C             IER =-1  singularita - primky se protinaji pod uhlem    *
C                      mensim nez priblizne 20 sedesatinnych vterin   *
C VSTUPNI/VYSTUPNI PARAMETRY:                                         *
C    Y,X    - pole pro ulozeni souradnic; souradnice danych bodu mu-  *
C             si byt pred vyvolanim procedury ulozeny do pozic Y(1)   *
C             az Y(4), resp. X(1) az X(4); vypoctene souradnice pru-  *
C             seciku ulozi procedura do promennych Y(5), X(5)         *
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      DIMENSION DY(3),DX(3)
C      
      IER=0
      DO 1 I=1,2
      DY(I)=Y(I+1)-Y(1)
    1 DX(I)=X(I+1)-X(1)
      DY(3)=Y(4)-Y(3)
      DX(3)=X(4)-X(3)
      A=DY(1)*DX(3)-DY(3)*DX(1)
      ALFA=(DX(1)**2+DY(1)**2)*(DX(3)**2+DY(3)**2)
      IF (ALFA) 2,3,2
    3 IER=1
      RETURN
    2 ALFA=A**2/ALFA
      IF (ALFA.LT.1E-8) GO TO 4
      B=((DY(2)*DX(1))-(DX(2)*DY(1)))/A
      Y(5)=Y(3)+B*DY(3)
      X(5)=X(3)+B*DX(3)
      ALFA=((DY(2)*DX(3))-(DX(2)*DY(3)))/A
      ITYP=0
      IF ((ALFA.GE.0).AND.(ALFA.LE.1)) ITYP=1
      IF ((B.GE.0).AND.(B.LE.1)) ITYP=ITYP+2
      RETURN
    4 IER=-1
      RETURN
      END
