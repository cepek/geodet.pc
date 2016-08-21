C ORTOGONALIZACE PLNE MATICE
C=======================================================================
C
      SUBROUTINE GTORTM(A,M,N,NORTON,NX,INDX,TOLMIN,IDF)
C
C OBDELNIKOVA MATICE TYPU (M, N)
C NORTON  POCET ORTONORMALNICH SLOUPCU MATICE ( 1, ... ,NORTON )
C NX      POCET PRVKU SUBVEKTORU NEZNAMYCH, JEHOZ DELKA MA BYT
C         MINIMALIZOVANA
C INDX    POLE PRO REGISTRACI INDEXU NEZNAMYCH, KTERE VSTUPUJI DO
C         MINIMALIZACNI PODMINKY V PRIPADE NENULOVEHO DEFEKTU
C TOLMIN  TOLERANCE PRO IDENTIFIKACI LINEARNE ZAVISLYCH SLOUPCU
C IDF     DEFEKT MATICE A
C
      INTEGER  M, N, NORTON, NX, INDX(1)
C$INCLUDE: 'GTREAL.INC'
      DOUBLE PRECISION
     /         A(M,N), TOLMIN
C
      INTEGER  I, J, K, K1, L, N1
C$INCLUDE: 'GTREAL.INC'
      DOUBLE PRECISION
     /         S
C
      N1  = N - 1
      IDF = N1
      DO 100 I=1,N1
      IF(I.LT.NORTON)  GOTO 500
            S = 0.0
            DO 200 K=1,NX
               J = INDX(K)
               S = S + A(J,I)**2
200         CONTINUE
            S = SQRT(S)
            IF(ABS(S).LE.TOLMIN)  GOTO 100
            IDF = IDF - 1
            DO 250 J=1,M
               A(J,I) = A(J,I)/S
250         CONTINUE
500      CONTINUE
            K  = NORTON + 1 
            K1 = I  + 1
            IF(K.GT.K1)  K1 = K
            DO 400 K=K1,N
               S = 0.0
               DO 300 L=1,NX
                  J = INDX(L)
                  S = S + A(J,I)*A(J,K)
300            CONTINUE
               DO 350 J=1,M
                  A(J,K) = A(J,K) - S*A(J,I)
350            CONTINUE
400         CONTINUE
100   CONTINUE
C
      END
