C+
C MTNORB
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet hodnoty frekvencni a distribucni       *
C        funkce normalniho rozdeleni                                  *
C AUTOR: Ales Cepek, VUGTK                                            *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MTNORB(X,TYP,F,G)
      LOGICAL TYP     
      DOUBLE PRECISION X,F,G
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura pocita v danem bode x hodnotu frekvencni funkce   *
C    normovaneho normalniho rozdeleni a dale, podle volby uzivatele,  *
C    bud hodnotu D(x) distribucni funkce normovaneho normalniho roz-  *
C    deleni, nebo hodnotu 1 - D(x).                                   *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    X      - argument funkce rozdeleni                               *
C    TYP    - parametr specifikujici, ktere hodnoty bude procedura    *
C             pocitat                                                 *
C VYSTUPNI PARAMETRY:                                                 *
C    F      - F = D(x)      pri nastaveni TYP = .FALSE.               *
C             F = 1 - D(x)  pri nastaveni TYP = .TRUE.                *
C    G      - hodnota frekvencni funkce                               *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      DOUBLE PRECISION MAXD,MIND
      LOGICAL TYPV
      DOUBLE PRECISION B,S,T,P1,P2,Q1,Q2,A1,A2,X2,Y,R
      PARAMETER (MAXD=1D30, MIND=1D-30)
C      
      G=0.3989422804014327D0
      IF(X)2,1,2
    1 F=0.5D0
      RETURN
    2 TYPV=X.GT.0.0D0
      TYPV=TYP.AND.TYPV.OR..NOT.TYP.AND..NOT.TYPV
      B=DABS(X)
      X2=X*X
      G=G*DEXP(-0.5D0*X2)
      R=G/B
      IF(R)15,15,3
    3 R=0.35D1
      IF(TYPV)R=0.232D1
      IF(B-R)4,4,9
    4 Y=G*B
      F=Y
      S=Y
      R=0.3D1
    5 Y=Y*X2/R
      F=F+Y
      IF(F-S)7,7,6
    6 S=F
      R=R+0.2D1
      GOTO 5
    7 IF(TYPV)GOTO 8
      F=F+0.5D0
      RETURN
    8 F=0.5D0-F
      RETURN
    9 A1=0.2D1
      A2=0.0D0
      T=X2+0.3D1
      P1=G
      Q1=B
      P2=(T-0.1D1)*G
      Q2=T*B
      R=P1/Q1
      F=P2/Q2
      IF(TYPV)GOTO 10
      R=0.1D1-R
      F=0.1D1-F
   10 T=T+0.4D1
      A1=A1-0.8D1
      A2=A1+A2
      S=A2*P1+T*P2
      P1=P2
      P2=S
      S=A2*Q1+T*Q2
      Q1=Q2
      Q2=S
      IF(Q2-MAXD)12,12,11
   11 Q1=Q1*MIND
      Q2=Q2*MIND
      P1=P1*MIND
      P2=P2*MIND
   12 S=R
      R=F
      F=P2/Q2
      IF(TYPV)GOTO 13
      F=0.1D1-F
   13 IF(R-F)10,14,10
   14 IF(S-F)10,16,10
   15 F=0.1D1
      IF(TYPV)F=0.0D0
   16 RETURN
      END
