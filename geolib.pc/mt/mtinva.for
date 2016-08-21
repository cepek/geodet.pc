C+
C MTINVA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro inverzi symetricke pozitivne definitni matice  *
C AUTOR: Eliska Valkova, VUGTK, nyni RIS hl. m. Prahy                 *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MTINVA(A,N,W,IER)
      DOUBLE PRECISION A(1),W(1)
      INTEGER N,IER
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura slouzi k in situ inverzi symetricke pozitivne de- *
C    finitni matice A Gaussovou-Jordanovou metodou; dolni trojuhelni- *
C    kovou cast matice A, ulozenou po radcich v jednorozmernem poli,  *
C    prepise dolni trojuhelnikovou casti inverzni matice.             *
C VSTUPNI PARAMETR:                                                   *
C    N      - stupen matice A                                         *
C VYSTUPNI PARAMETR:                                                  *
C    IER    - IER = 0  regularni vystup                               *
C             IER = 1  singularita - matice A neni pozitivne definit- *
C                                    ni                               *
C VSTUPNI/VYSTUPNI PARAMETR:                                          *
C    A      - jednorozmerne pole s prvky typu DOUBLE PRECISION; pred  *
C             vyvolanim procedury musi byt v poli A po radcich uloze- *
C             na dolni trojuhelnikova cast dane matice A. Pri vystupu *
C             z procedury bude ve stejnem prostoru analogicky ulozena *
C             dolni trojuhelnikova cast inverzni matice               *
C PRACOVNI  POLE:                                                     *
C    W      - prostor k prechodnemu ukladani vektoru o N slozkach ty- *
C             pu DOUBLE PRECISION                                     *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      DOUBLE PRECISION P,Q
      IER=1
      DO 1 K=N,1,-1
      P=A(1)
      IF (P.LE.0) RETURN
      IF (N.GT.1) GO TO 9
      IF (N.EQ.1) IER=0
      A(1)=1/P
      RETURN
    9 II=1
      DO 2 I=2,N,1
      M=II
      II=II+I
      Q=A(M+1)
      IF (I-K)3,3,4
    4 W(I)=Q/P
      GO TO 5
    3 W(I)=-Q/P
    5 IF((M+2)-II)8,8,2
    8 DO 6 IJ=M+2,II,1
    6 A(IJ-I)=A(IJ)+Q*W(IJ-M)
    2 CONTINUE
      M=M-1
      A(II)=1/P
      DO 7 I=2,N,1
    7 A(M+I)=W(I)
    1 CONTINUE
      IER=0
      END
