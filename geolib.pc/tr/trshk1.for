C+
C TRSHK1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet klice rovinne shodnostni transformace  *
C AUTOR: Ales Cepek, VUGTK,                                           *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
        SUBROUTINE TRSHK1 (YXZSTA,YXZNOV,N1,N,IER,A,YXZ0,ODCH,
     /                     M0,INDEX,M0RED)
        IMPLICIT REAL (A-Z)
        INTEGER   N1,N,IER,INDEX
        DIMENSION YXZSTA(N1,2),YXZNOV(N1,2),A(2,2),
     /            YXZ0(2),ODCH(N1,2)
	REAL      M0,M0RED
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura slouzi k vypoctu klice shodnostni transformace v  *
C    rovine vyrovnanim podle metody nejmensich ctvercu. Klic se poci- *
C    ta z pravouhlych souradnic volitelneho poctu N identickych bodu, *
C    danych ve dvou rovinnych souradnicovych soustavach. Vedle prvku  *
C    transformacniho klice pocita procedura opravy souradnic z vyrov- *
C    nani a stredni souradnicovou chybu klice M0. Dale identifikuje   *
C    bod klice, jehoz pripadne vypusteni by vedlo k maximalnimu po-   *
C    klesu souctu ctvercu oprav a tedy k nejmensi hodnote chyby M0RED *
C    takto redukovaneho klice.                                        *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    YXZSTA - pole pro ulozeni souradnic identickych bodu v soustave, *
C             ze ktere se transformuje                                *
C    YXZNOV - pole pro ulozeni souradnic identickych bodu v soustave, *
C             do ktere se transformuje                                *
C    N1     - pocet radku (mez prvniho indexu) v polich pro ulozeni   *
C             souradnic identickych bodu                              *
C    N	    - pocet identickych bodu (2 .LE. N .LE. N1) - klic se     *
C             pocita ze souradnic ulozenych v prvnich N radcich poli  *
C             YXZSTA a YXZNOV                                         *
C VYSTUPNI PARAMETRY:                                                 *
C    A      - pole pro ulozeni transformacni matice                   *
C    YXZ0   - pole pro ulozeni prvku translacniho vektoru             *
C    ODCH   - pole oprav souradnic bodu klice                         *
C    M0     - stredni souradnicova chyba klice                        *
C    INDEX  - index bodu, jehoz vypusteni by vedlo k maximalnimu po-  *
C             klesu souctu ctvercu oprav                              *
C    M0RED  - stredni souradnicova chyba klice, redukovaneho vypuste- *
C             nim bodu s indexem INDEX                                *
C    IER    - IER = 0 regularni uloha                                 *
C             IER = 1 singularita - nedovoleny pocet identickych      *
C                     bodu                                            *
C             IER =-1 singularita - vsechny identicke body v sousta-  *
C                     ve, ze ktere se transformuje, jsou totozne      *
C             IER =-2 singularita - nulova matice podobnostni trans-  *
C                     formace (viz textovou cast dokumentace)         *
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
C EXTERNI PROCEDURA: TRLIN1                                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	INTEGER I,IIR
	REAL    SQRT
	LOGICAL TEST
C 
      F2(YST,XST)  = YST*YST + XST*XST
      TEST(JMENOV) = ABS(JMENOV).LT.1E-5
      IER = 1
      IF(N.LT.2.OR.N.GT.N1)  RETURN
      YST = 0
      XST = 0
      YNT = 0
      XNT = 0
      FLN = N
      DO 100 I=1,N
         YST = YST + YXZSTA(I,1)
         XST = XST + YXZSTA(I,2)
         YNT = YNT + YXZNOV(I,1)
100      XNT = XNT + YXZNOV(I,2)
      YST = YST/FLN
      XST = XST/FLN
      YNT = YNT/FLN
      XNT = XNT/FLN
      SA1 = 0
      SA2 = 0
      SUMSS = 0
      DO 200 I=1,N
         YSD = YXZSTA(I,1) - YST
         XSD = YXZSTA(I,2) - XST
         YND = YXZNOV(I,1) - YNT
         XND = YXZNOV(I,2) - XNT
         SA1 = SA1 + YSD*YND + XSD*XND
         SA2 = SA2 + XSD*YND - YSD*XND
200      SUMSS = SUMSS + F2(YSD,XSD)
      IER = -1
      IF(TEST(SUMSS))  RETURN
      POMS   = SQRT(F2(SA1,SA2))
      IER = -2
      IF(POMS.EQ.0)  RETURN
      A(1,1) = SA1/POMS
      A(1,2) = SA2/POMS
      A(2,2) = A(1,1)
      A(2,1) =-A(1,2)
      YXZ0(1) = YNT - YST*A(1,1) - XST*A(1,2)
      YXZ0(2) = XNT + YST*A(1,2) - XST*A(1,1)
      IER = 0
      M0RED = 0
      CALL TRLIN1(YXZ0,A,YXZSTA,ODCH,N1,2,N,IIR)
      INDEX = 1
      DELTA = 0
      SUMSS = 1/SUMSS
      POMS  = N - 1
      SA2   = POMS/FLN
      SA1   = SUMSS/SA2
      SUMAVV= 0
      DO 300 I=1,N
         FY = ODCH(I,1) - YNT
         FX = ODCH(I,2) - XNT
         ODCH(I,1) = ODCH(I,1) - YXZNOV(I,1)
         ODCH(I,2) = ODCH(I,2) - YXZNOV(I,2)
         VY = ODCH(I,1)
         VX = ODCH(I,2)
         V2 = F2(VY,VX)
         SUMAVV = SUMAVV + V2
         JMENOV = SA2 - SUMSS*F2(FY,FX)
         IF(TEST(JMENOV))  GO TO 300
         DELTAI = (V2-SA1*(VY*FY+VX*FX)**2)/JMENOV
         IF(DELTAI.LE.DELTA)  GO TO 400
            DELTA = DELTAI
            INDEX = I
400      CONTINUE
300   CONTINUE
      M0 = SUMAVV
      M0 = SQRT(M0/(2*N-3))
      IF(SUMAVV.LE.DELTA.OR.N.LE.2)  GO TO 500
         M0RED = SUMAVV - DELTA
         M0RED = SQRT(M0RED/(2*N-5))
500   CONTINUE
      RETURN
C
      END
