C+
C MTSTU1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Funkcni procedura pro vypocet kritickych hodnot Studentova   *
C        rozdeleni                                                    *
C AUTOR: Ales Cepek, VUGTK                                            *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      REAL FUNCTION MTSTU1(PALFA,N)
      INTEGER N
      REAL    PALFA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Funkcni procedura pocita pro zadanou pravdepodobnost a po-  *
C    cet stupnu volnosti kritickou hodnotu Studentova rozdeleni. Vy-  *
C    sledek vypoctu je predavan volajicimu programu jako funkcni hod- *
C    nota typu REAL.                                                  *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    PALFA  - pravdepodobnost, pro kterou se urcuje kriticka hodnota  *
C             Studentova rozdeleni; hodnota parametru PALFA musi le-  *
C             zet v otevrenem intervalu (0, 1); procedura netestuje   *
C             pripustnost hodnoty aktualniho parametru                *
C    N      - pocet stupnu volnosti                                   *
C ARITMETIKA SMISENA                                                  *
C EXTERNI PROCEDURY A FUNKCE: MTNORA, MTNORB                          *
C POZNAMKA: Jmeno funkcni procedury MTSTU1 musi byt ve volajicim      *
C    programu deklarovano jako promenna typu REAL.                    *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      DOUBLE PRECISION MTNORA,AA
      PARAMETER (PIPUL = 1.5707963268)
C      
      ALFA=PALFA
      IF(ALFA.GT.0.5) ALFA=1.0-ALFA
      ALFA=2*ALFA
      MTSTU1=0.0
      IF(PALFA.EQ.0.5) RETURN
      IF(N.GT.1)GOTO 1
      A=PIPUL*ALFA
      MTSTU1=COS(A)/SIN(A)
      IF(PALFA.GT.0.5)  MTSTU1=-MTSTU1
      RETURN
    1 IF(N.GT.2)GOTO 2
      MTSTU1=SQRT(2.0/(ALFA*(2.0-ALFA))-2.0)
      IF(PALFA.GT.0.5)  MTSTU1=-MTSTU1
      RETURN
    2 R=N
      A=1.0/(R-0.5)
      B=48.0/A**2
      C=((20700.0*A/B-98.0)*A-16.0)*A+96.36
      D=((94.5/(B+C)-3.0)/B+1.0)*
     /SQRT(PIPUL*A)*R
      X=D*ALFA
      Y=X**(2.0/R)
      IF(Y.LE.A+0.05)GOTO 6
      AA=DBLE(0.5*ALFA)
      X=-MTNORA(AA)
      Y=X**2
      IF(N.LT.5)C=C+0.3*(R-4.5)*(X+0.6)
      C=(((0.05*D*X-5.0)*X-7.0)*X-2.0)*X+B+C
      Y=(((((0.4*Y+6.3)*Y+36.0)*Y+94.5)/C
     /-Y-3.0)/B+1.0)*X
      Y=A*Y**2
      IF(Y-0.002)3,3,4
    3 Y=0.5*Y**2+Y
      GOTO 5
    4 Y=EXP(Y)-1.0
    5 MTSTU1=SQRT(R*Y)
      IF(PALFA.GT.0.5)  MTSTU1=-MTSTU1
      RETURN
    6 Y=((1.0/(((R+6.0)/(R*Y)-0.089*D-
     /0.822)*(R+2.0)*3.0)+0.5/(R+4.0))
     /*Y-1.0)*(R+1.0)/(R+2.0)+1.0/Y
      GOTO 5
      END
