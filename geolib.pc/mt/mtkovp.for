C+
C MTKOVP
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro interaktivni vstup dat                         *
C AUTOR: Jaroslava Horejcova, VUGTK, nyni Geodezie, s.p., Praha,      *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MTKOVP (ID,N,M,X,CX,DIAGCX,KONEC,TEXT)
      DOUBLE PRECISION X(1)
      DIMENSION        CX(1)
      LOGICAL*1        DIAGCX,KONEC
      CHARACTER*40     TEXT 
      INTEGER          ID,N,M
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: MTKOVP je pomocna procedura, zajistujici vstup dat, ktera   *
C    se dale zpracovavaji procedurou MTKOVA. Vstupni data tvori zej-  *
C    mena pocet argumentu, pocet funkci, vektor argumentu a jeho ko-  *
C    variancni matice. Vstup se provadi dialogovym zpusobem z klaves- *
C    nice.                                                            *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETR:                                                   *
C    ID     - identifikacni cislo slouzici k orientaci operatora pri  *
C             vicenasobnem volani procedury MTKOVP z ruznych mist     *
C             hlavniho programu                                       *
C VYSTUPNI PARAMETRY:                                                 *
C    N      - pocet argumentu                                         *
C    M      - pocet funkci                                            *
C    X      - jednorozmerne pole N prvku DOUBLE PRECISION, do ktereho *
C             procedura ulozi vektor argumentu                        *
C    CX     - jednorozmerne pole s prvky typu REAL, do ktereho proce- *
C             dura ulozi kovariancni matici vektoru argumentu         *
C    DIAGCX - je-li kovariancni matice CX diagonalni, potom DIAGCX =  *
C             .TRUE., v opacnem pripade DIAGCX = .FALSE.              *
C    KONEC  - parametr slouzici k indikaci konce dat; nastavuje se na *
C             hodnotu .TRUE., chce-li operator ukoncit vypocet davky  *
C             uloh                                                    *
C    TEXT   - znakove pole, do ktereho procedura ulozi nazev ulohy    *
C EXTERNI PROCEDURY: TTASK, TTASKN, TTASKS, TTROL1                    *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
1     FORMAT (' VSTUP C.',I3)
2     FORMAT (' INDEX,ARGUMENT:  '\)
3     FORMAT (' I,CII:  '\)
4     FORMAT (I2,'. ARGUMENT:  '\)
5     FORMAT (F15.0)
6     FORMAT (I2,F15.0)
7     FORMAT (' C',I2,I3,':  '\)
8     FORMAT (2I2,F15.0)
9     FORMAT (' I,J,CIJ:  '\)
      WRITE(5,1) ID
101   CALL TTASK (5,'MA BYT ZAHAJEN DIALOG PRO VSTUP DAT ? (A/N):'
     1             ,ISS)
      GO TO (101,101,101,101,202,10,202) ISS+5
202   KONEC = .TRUE.
      RETURN
10    CONTINUE
203   CALL TTASKS (5,'VLOZ NAZEV ULOHY (MAX 40 ZNAKU): ',ISS,TEXT,40)
      IF (ISS.LT.0) GO TO 203
11    CALL TTASK (5,'BUDOU VKLADANY /MENENY/ HODNOTY ARGUMENTU ? (A/N):'
     1           ,ISS)
      GO TO (11,11,11,11,45,12,45),ISS+5
12    CALL TTASKN (5,'POCET ARGUMENTU:  ',ISS,N,1)
      IF (ISS.LE.0) GO TO 12
204   CALL TTASKN (5,'POCET FUNKCI   :  ',ISS,M,1)
      IF (ISS.LE.0) GO TO 204
13    CALL TTASK (5,'BUDOU VKLADANY VSECHNY ARGUMENTY ? (A/N):',ISS)
      IP=1
14    IF (ISS.EQ.0.OR.ISS.EQ.2) GO TO (30,100,75,55,65,85,88),IP
      IF (ISS.EQ.1) GO TO (15,46,48,49,59,77,89),IP
      GO TO (13,45,46,47,55,76,85),IP
15    DO 20 I=1,N
      WRITE(5,4) I
20    READ(5,5) X(I)
      GO TO 45
30    DO 40 I=1,N+1
      WRITE(5,2)
40    READ (5,6,END=45) J,X(J)
45    CALL TTASK
     1  (5,'BUDE VKLADANA /MENENA/ KOVARIANCNI MATICE CX ? (A/N):',ISS)
      IP=2
      GO TO 14
46    CALL TTASK (5,'JE CX DIAGONALNI ? (A/N):',ISS)
      IP=3
      GO TO 14
48    DIAGCX=.TRUE.
47    CALL TTASK
     1  (5,'BUDOU VKLADANY VSECHNY DIAGONALNI PRVKY CII ? (A/N):',ISS)
      IP=4
      GO TO 14
49    DO 50 I=1,N
      WRITE(5,7) I,I
50    READ(5,5) CX(I)
      GO TO 100
55    CALL TTASK (5,'NEVLOZENE PRVKY SE MAJI ANULOVAT ? (A/N):',ISS)
      IP=5
      GO TO 14
59    DO 60 I=1,N
60    CX(I)=0.
65    DO 70 I=1,N+1
      WRITE(5,3)
70    READ (5,6,END=100) J,CX(J)
      GO TO 100
75    DIAGCX=.FALSE.
76    CALL TTASK (5,'BUDOU VKLADANY VSECHNY PRVKY CIJ ? (A/N):',ISS)
      IP=6
      GO TO 14
77    DO 80 I=1,N
      DO 80 J=1,I
      WRITE(5,7) I,J
80    READ(5,5) CX(I*(I-1)/2+J)
      GO TO 100
85    CALL TTASK (5,'NEVLOZENE PRVKY SE MAJI ANULOVAT ? (A/N):',ISS)
      IP=7
      GO TO 14
89    DO 86 I=1,N*(N+1)/2
86    CX(I)=0.
88    DO 90 II=1,N*(N+1)/2
      WRITE(5,9)
      READ (5,8,END=100) I,J,CP
      K=MAX0 (I,J)
      J=MIN0 (I,J)
90    CX(K*(K-1)/2+J)=CP
100   KONEC=.FALSE.
      RETURN
      END
