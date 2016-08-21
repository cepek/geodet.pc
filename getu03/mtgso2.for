C+
C MTGSO2
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro reseni soustav rovnic oprav algoritmem GSO     *
C AUTOR: Ales Cepek, VUGTK,                                           *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C VERZE: 1.01                                                         *
C DATUM: 1989-11-14   parametr  A  deklarovan jako [HUGE];            *
C        upravena deklarace IMPLICIT INTEGER*4 ,  INTEGER*4 ADIM,     *
C        a deklarace celociselnych parametru na INTEGER*2;            *
C        doplnena pomocna promenna M14 typu INTEGER*4                 *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MTGSO2(A[HUGE],ADIM,M1,N1,N2,N4,MODE,SC,EPS,TOL,
     /                  NX,DF,PROC,IER,INDX,INDS,CP)
        IMPLICIT INTEGER*4 (A-Z)
	INTEGER*4  ADIM
        INTEGER*2  M1,N1,N2,N4,MODE,NX,DF,IER,INDX(1),INDS(1)
	LOGICAL  SC
	REAL     A(ADIM),EPS,TOL
	DOUBLE PRECISION CP(1)
	EXTERNAL PROC
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura MTGSO2 je modifikaci obecne procedury MTGSO1,     *
C    specializovanou pro aplikaci v programu pro vyrovnani mistnich   *
C    trigonometrickych siti GETU03 (programovy system GEODET/PC).     *
C    Procedura MTGSO2 zachovava vsechny funkce procedury MTGSO1,      *
C    shodny je i zpusob jejiho pouziti. Jedinym rozdilem mezi obema   *
C    procedurami je odlisna vnitrni organizace vlastniho ortogonali-  *
C    zacniho procesu: procedura MTGSO2 optimalizuje poradi ortogona-  *
C    lizace sloupcu matice rovnic oprav.                              *
C PREHLED PARAMETRU  - viz MTGSO1                                     *
C ARITMETIKA SMISENA                                                  *
C EXTERNI PROCEDURA: viz MTGSO1                                       *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      LOGICAL  PRV
      REAL     EPS1,TOL1,MAXIM
      DOUBLE PRECISION  SK,SKP,NORM,NORMP
      INTEGER*4  INDEX,INIP,INJP	
      PARAMETER  (MAXINT=32767)
      INTEGER*4  M14
C INDEX PRVKU V POLI A
      INDEX(INIP,INJP) = (INJP-1)*MN1 + INIP
C
C=======================================================================
C
C   OPERACNI CAST PROCEDURY MTGSO2
C
      M14   = M1
C CELKOVY POCET SLOUPCU SOUSTAVY ROVNIC
      N12   = N1 + N2
C CELKOVY POCET RADKU ROZSIRENE MATICE
      MN1   = M1 + N1
C INDEX PRVNIHO RADKU DOLNI SUBMATICE
      M11   = M1 + 1
C INTERNI SPECIFIKACE VARIANTY RESENI
      MODE1 = MODE
C INTERNI TOLERANCE LINEARNE ZAVISLYCH SLOUPCU
      TOL1  = TOL
C POMOCNA PROMENNA PRO TISK INDEXU NOREM
      ZMENA = 0
      IER   = 0
      IF(SC) THEN
C JE POZADOVANA VSTUPNI NORMALIZACE
         EPS1 = 1E3*EPS
         IF(TOL.LT.EPS1)  TOL1 = EPS1
      END IF
C
C   PRVNI ORTOGONALIZACE
C
      IF(MODE1.LT.3) THEN
C DEFEKT MATICE SOUSTAVY ROVNIC OPRAV
         DF  = 0
C INTERNI PRIZNAK PRVNI ORTOGONALIZACE
         PRV = .TRUE.
         ASSIGN 1 TO RTSORT
C VOLANI INTERNI ORTOGONALIZACNI PROCEDURY
         GOTO 10
      END IF
1     CONTINUE
C
C   DRUHA ORTOGONALIZACE
C
      IF(MODE1.GT.1.AND.DF.GT.0) THEN
         MODE1 = 3
         IF(NX.LT.DF) THEN
C RESENI NENI JEDNOZNACNE DEFINOVANO
            IER = 1
            RETURN
         END IF
C DEFINUJE KONEC CYKLU
         INDX(NX+1) = N1+1
C INTERNI PRIZNAK DRUHE ORTOGONALIZACE
         PRV = .FALSE.
         ASSIGN 2 TO RTSORT
C VOLANI INTERNI ORTOGONALIZACNI PROCEDURY
         GOTO 10
      END IF
C KONEC OPERACNI CASTI PROCEDURY MTGSO2
2     RETURN
C
C=======================================================================
C
C   INTERNI ORTOGONALIZACNI PROCEDURA
C
10    CONTINUE
      IF(PRV) THEN
C POCET ORTOGONALIZOVANYCH SLOUPCU
         N = N1
      ELSE
         N = DF
      END IF
C
C   VSTUPNI NORMALIZACE
C
      IF(SC) THEN
         IF(PRV) THEN
            HMEZ = M1 - 1
C NORMALIZACE ZACINA PRVNIM SLOUPCEM
            DMEZ = 1
C CYKLUS PRO VSECHNY ORTOGONALIZOVANE SLOUPCE
            DO 100 J=1,N
C HLEDA SE V ABS. HODNOTE NEJVETSI PRVEK
               MAXIM = 0E0
               DO 110 I=DMEZ,DMEZ+HMEZ
110               IF(ABS(A(I)).GT.MAXIM)  MAXIM = ABS(A(I))
               IF(MAXIM.GT.0E0) THEN
                  DO 120 I=DMEZ,DMEZ+HMEZ
120                  A(I) = A(I)/MAXIM
C PRVEK NA DIAGONALE
                  A(DMEZ+HMEZ+J) = 1E0/MAXIM
               END IF
C ZACATEK NASLEDUJICIHO SLOUPCE
               DMEZ = DMEZ + MN1
100         CONTINUE
         ELSE
C CYKLUS PRO VSECHNY ORTOGONALIZOVANE SLOUPCE
            DO 200 J=1,N
C INDEX ZPRACOVAVANEHO SLOUPCE
               INDSJ = ABS(INDS(J))
               MAXIM = 0E0
               DO 210 K=1,MAXINT
                  IF(INDX(K).GT.INDSJ)  GOTO 215
                  I = INDEX(M14+INDX(K),INDSJ)
210               IF(ABS(A(I)).GT.MAXIM)  MAXIM = ABS(A(I))
215            CONTINUE
               IF(MAXIM.GT.0E0) THEN
                  K = INDEX(M11,INDSJ)
                  DO 220 I=K,K+INDSJ-1
220                  A(I) = A(I)/MAXIM
               END IF
200         CONTINUE
         END IF
      END IF
C
C   VLASTNI ORTOGONALIZACE
C
C CYKLUS PRO VSECHNY ORTOGONALIZOVANE SLOUPCE
      DO 300 J=1,N
C        VYTVORENI KOPIE BEZNEHO VEKTORU V POLI CP, VYPOCET SKALARNIHO
C        SOUCINU (CP*CP)
         IF(PRV) THEN
            IMAX = M1 + J
C  1. SLOUPEC NEORTOGONALNI K J-TEMU
            KMIN = MAX(N4,J) + 1
C J-TY SLOUPEC ZACINA PRVKEM A(K+1)
            K = (J-1)*MN1
            DO 410 I=1,IMAX
C A(I,J)
410            CP(I) = A(K+I)
            SK = 0D0
            DO 420 I=1,M1
420            SK = SK + CP(I)**2
            NORM = SQRT(SK)
C           ZMENA PORADI ORTOGONALIZACE SLOUPCU PRO SOURADNICE BODU
            IF(MOD(J-N4,2).EQ.1.AND.NORM.GE.TOL1) THEN
C SLOUPEC J+1 ZACINA PRVKEM A(K)
               K = K + MN1 + 1
               SKP = 0D0
C SKALARNI SOUCIN PRO NASLEDUJICI SLOUPEC
               DO 430 I=K,K+M1-1
430               SKP = SKP + DBLE(A(I))**2
               NORMP = SQRT(SKP)
               IF(NORMP.GE.TOL1) THEN
                  IF(ABS(A(INDEX(M1+J+1,J+1)))/NORMP.LT.
     /            0.7*ABS(A(INDEX(M1+J,J)))/NORM) THEN
C SLOUPEC J ZACINA PRVKEM A(K+1)
                     K = (J-1)*MN1
C SLOUPEC J+1 ZACINA PRVKEM A(K1+1)
                     K1 = K + MN1
C VYMENA SLOUPCU J A J+1
                     DO 440 I=1,M11+J
                        KI = K + I
                        KI1 = K1 + I
C A(I,J+1)
                        CP(I) = A(KI1)
C A(I,J)
                        A(KI1) = A(KI)
440                     A(KI) = CP(I)
                     NORM = NORMP
                     ZMENA = 1
                     IMAX = IMAX + 1
                  END IF
               END IF
            END IF
         ELSE
            INDSJ = ABS(INDS(J))
            IMAX = M1 + INDSJ
            K = (INDSJ-1)*MN1
            DO 500 I=M11,IMAX
C A(I,INDS(J))
500            CP(I) = A(K+I)
            SK = 0D0
            DO 510 I=1,MAXINT
               IF(INDX(I).GT.INDSJ)  GOTO 515
510            SK = SK + CP(M1+INDX(I))**2
515         CONTINUE
            NORM = SQRT(SK)
         END IF
         IF(PRV) THEN
            K = J + ZMENA
         ELSE
            IF(INDS(J).GT.0) THEN
               K = INDSJ
            ELSE
               K = INDSJ - 1
            END IF
         END IF
         CALL PROC(K,NORM,TOL1)
         IF(NORM.LT.TOL1) THEN
C           LINEARNE ZAVISLY SLOUPEC
            IF(MODE1.EQ.3) THEN
               IER = 1
               RETURN
            END IF
C CITAC LINEARNE ZAVISLYCH SLOUPCU
            DF = DF + 1
            IF(ZMENA.EQ.-1) THEN
C DOSLO K VYMENE SLOUPCU
               INDS(DF) = -J
            ELSE
               INDS(DF) =  J
            END IF
            K1 = (J-1)*MN1
            DO 550 I=K1+1,K1+M1
C VYMAZ LINEARNE ZAVISLEHO SLOUPCE
550            A(I) = 0E0
         ELSE
            IF(PRV) THEN
C J-TY SLOUPEC ZACINA PRVKEM A(K1+1)
               K1 = (J-1)*MN1
               DO 600 I=1,IMAX
                  CP(I) = CP(I)/NORM
600               A(K1+I) = CP(I)
C K-TY SLOUPEC ZACINA PRVKEM A(K1+1)
               K1 = (KMIN-1)*MN1
               DO 610 K=KMIN,N12
                  SK = 0D0
C SK. SOUCIN SLOUPCU J A K
                  DO 620 I=1,M1
C A(I,K)
620                  SK = SK + CP(I)*A(K1+I)
                  DO 630 I=1,IMAX
                     K2 = K1 + I
630                  A(K2) = A(K2) - SK*CP(I)
                  K1 = K1 + MN1
610            CONTINUE
            ELSE
C SLOUPEC INDS(J) ZACINA A(K1+1)
               K1 = (INDSJ-1)*MN1
               DO 700 I=M11,IMAX
                  CP(I) = CP(I)/NORM
C LIN. ZAVISLY SLOUPEC VYNULOVAN
700               A(K1+I) = 0E0
C K-TY SLOUPEC ZACINA A(K1+1)
               K1 = 0
               DO 710 K=1,N12
                  SK = 0D0
                  DO 720 I=1,MAXINT
                     IF(INDX(I).GT.INDSJ)  GOTO 725
C CISLO RADKU V MATICI A
                     K2 = M1 + INDX(I)
720                  SK = SK + CP(K2)*A(INDEX(K2,K))
725               CONTINUE
                  DO 730 I=M11,IMAX
                     K2 = K1 + I
730                  A(K2) = A(K2) - SK*CP(I)
                  K1 = K1 + MN1
710            CONTINUE
               K1 = (INDSJ-1)*MN1
            END IF
         END IF
         IF(ZMENA.EQ.1) THEN
            ZMENA = -1
         ELSE
            ZMENA =  0
         END IF
300   CONTINUE
      GOTO RTSORT
C
      END
