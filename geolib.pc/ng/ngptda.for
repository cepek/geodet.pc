C+
C NGPTDA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet souradnic bodu protinanim z delek      *
C AUTOR: Frantisek Charamza, VUGTK,                                   *
C        Eliska Valkova, VUGTK, nyni RIS hl. m. Prahy                 *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE NGPTDA (Y,X,S,ISIG,IER)
      DOUBLE PRECISION Y(3),X(3),S(2)
      INTEGER          ISIG,IER
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura NGPTDA vypocte souradnice bodu, urceneho dvema    *
C    vodorovnymi vzdalenostmi od dvou danych bodu.                    *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    S      - pole pro ulozeni urcujicich delek                       *
C    ISIG   - parametr pro vyber jednoho reseni u dvojznacneho zadani *
C             ISIG = 1  ma-li hledany bod lezet vpravo od spojnice    *
C                       danych bodu (pro pozorovatele hlediciho z bo- *
C                       du P1 na bod P2)                              *
C             ISIG =-1  ma-li hledany bod lezet vlevo od uvedene      *
C                       spojnice                                      *
C VYSTUPNI PARAMETR:                                                  *
C    IER    - IER = 0  regularni uloha                                *
C             IER = 1  singularita - dane body jsou totozne           *
C             IER =-1  singularita - uloha nema reseni                *
C VSTUPNI/VYSTUPNI PARAMETRY:                                         *
C    Y,X    - pole pro ulozeni souradnic; souradnice danych bodu musi *
C             byt pred vyvolanim procedury ulozeny do pozic Y(1),     *
C             X(1), Y(2), X(2); vypoctene souradnice ulozi procedura  *
C             do promennych Y(3), X(3)                                *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      DOUBLE PRECISION DY,DX,S12,S1,S2,F,G,H
C      
      IER=0
      DY=Y(2)-Y(1)
      DX=X(2)-X(1)
      S12=DSQRT(DY**2+DX**2)
      IF (S12.EQ.0) GO TO 1
      S1=S(1)/S12
      S2=S(2)/S12
      F=(1+(S1+S2)*(S1-S2))/2
      G=(S1+F)*(S1-F)
      IF (G.LT.0) GO TO 2
      H=ISIG*DSQRT(G)
      Y(3)=Y(1)+(DY*F)+(DX*H)
      X(3)=X(1)+(DX*F)-(DY*H)
      RETURN
    1 IER=1
      RETURN
    2 IER=-1
      RETURN
      END
