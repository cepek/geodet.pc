C+
C TRSHKA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet klice rovinne shodnostni transformace  *
C AUTOR: Ales Cepek, VUGTK,                                           *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
        SUBROUTINE TRSHKA (YXZSTA,YXZNOV,N1,N,IER,A,YXZ0,ODCH,
     /                     M0,INDEX,M0RED)
        IMPLICIT DOUBLE PRECISION (A-Z)
        INTEGER   N1,N,IER,INDEX
        DOUBLE PRECISION YXZSTA(N1,2),YXZNOV(N1,2),A(2,2),
     /            YXZ0(2),ODCH(N1,2)
	REAL      M0,M0RED
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: viz zdrojovy text procedury TRSHK1                          *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY: viz TRSHK1                                       *
C VYSTUPNI PARAMETRY: viz TRSHK1                                      *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C EXTERNI PROCEDURA: TRLINA                                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	INTEGER I,IIR
	REAL    SQRT
	LOGICAL TEST
C 
      F2(YST,XST)  = YST*YST + XST*XST
      TEST(JMENOV) = DABS(JMENOV).LT.1D-10
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
      POMS   = DSQRT(F2(SA1,SA2))
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
      CALL TRLINA(YXZ0,A,YXZSTA,ODCH,N1,2,N,IIR)
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
