C+
C TRLINA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro linearni transformaci souradnic                *
C AUTOR: Frantisek Charamza, VUGTK,                                   *
C        Eliska Valkova, VUGTK, nyni RIS hl. mesta Prahy              *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE TRLINA (YXZ0,A,YXZSTA,YXZNOV,N1,IDIM,IPOCET,IER)
	INTEGER          N1,IDIM,IPOCET,IER
	DOUBLE PRECISION YXZ0(IDIM),A(IDIM,IDIM),YXZSTA(N1,IDIM),
     /            YXZNOV(N1,IDIM)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura slouzi k linearni transformaci rovinnych nebo     *
C    prostorovych pravouhlych souradnic bodovych skupin pomoci daneho *
C    transformacniho klice. Jednim vyvolanim procedury lze transfor-  *
C    movat souradnice volitelneho poctu bodu.                         *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    YXZ0   - pole pro ulozeni prvku translacniho vektoru             *
C    A      - pole pro ulozeni transformacni matice                   *
C    YXZSTA - pole pro ulozeni souradnic, jez maji byt transformovany *
C    N1     - pocet radku (mez prvniho indexu) v polich pro ulozeni   *
C             souradnic YXZSTA a YXZNOV                               *
C    IDIM   - specifikace typu transformace - musi byt IDIM = 2 pro   *
C             rovinnou transformaci a IDIM = 3 pro transformaci v     *
C             prostoru                                                *
C    IPOCET - pocet bodu (IPOCET.LE.N1), jejichz souradnice maji byt  *
C             transformovany; do transformacni operace vstupuje prv-  *
C             nich IPOCET radku poli YXZSTA a YXZNOV                  *
C VYSTUPNI PARAMETRY:                                                 *
C    YXZNOV - pole, do ktereho procedura ulozi vypoctene souradnice   *
C    IER    - IER = 0 regularni uloha                                 *
C             IER = 1 singularita - nedovolena hodnota parametru IDIM *
C                                 - je pozadovana transformace sou-   *
C                                   radnic vice nez N1 bodu           *
C                                   (IPOCET.GT.N1)                    *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      DOUBLE PRECISION POM(3),S 
C      
      IER=1
      IF(IDIM.LT.2) RETURN
      IF (IDIM.GT.3) RETURN
      IF (IPOCET.GT.N1) RETURN
      IER=0
      DO 1 K=1,IPOCET
      DO 2 I=1,IDIM
      S=0
      DO 3 J=1,IDIM
    3 S=S+A(I,J)*YXZSTA(K,J)
    2 POM(I)=S
      DO 4 I=1,IDIM
    4 YXZNOV(K,I)=YXZ0(I)+POM(I)
    1 CONTINUE
      END
