C GIVENSOVA TRANSFORMACE - ZPRACOVANI JEDNOHO RADKU
C=======================================================================
C
      SUBROUTINE GTRAD1(NONZ,INDC,ROW,ABSCL,POCNEZ,RMASK,TOLEPS,DIAG,X,
     /                  RADEK,XRNZ,XNZSUB,NZSUB,RNEP)
C
C NONZ    POCET NENULOVYCH KOEFICIENTU V RADKU
C INDC    POLE NENULOVYCH INDEXU
C ROW     NENULOVE KOEFICIENTY
C ABSCL   ABSOLUTNI CLEN
C POCNEZ  POCET NEZNAMYCH  (SLOUPCU MATICE A)
C RMASK   MASKA ZPRACOVANYCH RADKU
C TOLEPS  TOLERANCE PRO IDENTIFIKACI NULOVYCH DIAGONALNICH PRVKU
C DIAG    DIAGONALNI PRVKY MATICE R
C X       VEKTOR NEZNAMYCH X
C RADEK   PRACOVNI POLE PRO ULOZENI BEZNEHO RADKU (MATICE A)
C XRNZ    VEKTOR SMERNIKU NA SEZNAMY NENULOVYCH PRVKU V RADCICH R
C XNZSUB  VEKTOR SMERNIKU DO NZSUB
C NZSUB   KOMPRIMOVANY VEKTOR INDEXU NENULOVYCH PRVKU R
C RNEP    NEDIAGONALNI NENULOVE PRVKY ROZKLADU R
C
      INTEGER  NONZ, INDC(1), POCNEZ, RMASK(1), 
     /         XRNZ(1), XNZSUB(1), NZSUB(1)
C$INCLUDE: 'GTREAL.INC'
      DOUBLE PRECISION
     /         ROW(1), ABSCL, TOLEPS, DIAG(1), X(1), RADEK(1),
     /         RNEP(1)
C
C-----------------------------------------------------------------------
C
      INTEGER  MININD, J, K, JMIN, POC, KON, IND
C$INCLUDE: 'GTREAL.INC'
      DOUBLE PRECISION
     /         W, C, S, R, RX, RY
C
      MININD = POCNEZ
      DO 300 J=1,NONZ
         K = INDC(J)
         IF(MININD.LT.K)  GOTO 300
            MININD = K
            JMIN   = J
300   CONTINUE
400   CONTINUE
         IF(RMASK(MININD).NE.0)  GOTO 500
            RMASK(MININD) = 1
            W = ROW(JMIN)
            DIAG(MININD) = W
            X(MININD) = ABSCL
            ABSCL = 0.0
            IF(NONZ.LE.1)  GOTO 499               
            DO 505 J=1,NONZ
               IF(J.EQ.JMIN)  GOTO 505
                  K = INDC(J)
                  RADEK(K) = ROW(J)
505         CONTINUE
            POC = XRNZ(MININD)
            KON = XRNZ(MININD+1) - 1
            IND = XNZSUB(MININD)
            DO 501 J=POC,KON
               K = NZSUB(IND)
               IND = IND + 1
               RNEP(J)  = RADEK(K)
               RADEK(K) = 0.0
501         CONTINUE
            GOTO 499
500      CONTINUE
C        -----------------------------------------
C        VYPOCET PARAMETRU TRANSFORAMCE (COS, SIN)
C        -----------------------------------------
         CALL GTRPAR(DIAG(MININD),ROW(JMIN),TOLEPS,C,S,R)
         DIAG(MININD) = C*DIAG(MININD) + S*ROW(JMIN)
         W = X(MININD)
         X(MININD) = C*X(MININD) + S*ABSCL
         ABSCL = -S*W + C*ABSCL 
         POC = XRNZ(MININD)
         KON = XRNZ(MININD+1) - 1
         IF(POC.GT.KON)  GOTO 499
         IF(NONZ.LE.1)  GOTO 503
            INDC(JMIN) = INDC(1)
            ROW(JMIN) = ROW(1)
            DO 502 J=2,NONZ
               K = INDC(J)
               RADEK(K) = ROW(J)
502         CONTINUE
503      CONTINUE
         NONZ = 0
         IND = XNZSUB(MININD)
         MININD = POCNEZ
         DO 504 J=POC,KON
            K = NZSUB(IND)
            IND = IND + 1
            NONZ = NONZ + 1
            RX = RNEP(J)
            RY = RADEK(K)
            RADEK(K) = 0.0               
            RNEP(J)   =  C*RX + S*RY
            ROW(NONZ) = -S*RX + C*RY
            INDC(NONZ) = K
            IF(MININD.LT.K)  GOTO 504
               MININD = K
               JMIN = NONZ
504      CONTINUE
         GOTO 400
499   CONTINUE
      RETURN
C
      END
