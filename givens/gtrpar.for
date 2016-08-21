C VYPOCET PARAMETRU GIVENSOVY TRANSFORMACE
C (LAWSON & HANSON, SOLVING LEAST SQUARES PROBLEMS, PRENTICE-HALL, 1974)
C=======================================================================
C
      SUBROUTINE GTRPAR(A,B,TOLEPS,C,S,R)
C
C$INCLUDE: 'GTREAL.INC'
      DOUBLE PRECISION
     /       A, B, TOLEPS, C, S, R, X, Y
C
C A, B   VSTUPNI PARAMETRY
C TOLEPS TOLERANCE PRO IDENTIFIKACI NULOVYCH PRVKU
C C, S   PARAMETRY TRANSFORMACE
C R      VYSTUPNI PARAMETR (R=A**2+B**2)
C
      IF(ABS(A).LE.ABS(B))  GOTO 10
      X = B/A
      Y = SQRT(1.0 + X**2)
      C = SIGN(1.0/Y,A)
      S = C*X
      R = ABS(A)*Y
      RETURN
C
10    IF(ABS(B).LE.TOLEPS)  GOTO 30
      X = A/B
      Y = SQRT(1.0 + X**2)
      S = SIGN(1.0/Y,B)
      C = S*X
      R = ABS(B)*Y
      RETURN
C
30    C = 1.0
      S = 0.0
      R = 0.0
      RETURN
C
      END
