C+
C MTGSO2
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro reseni soustav rovnic oprav algoritmem GSO     *
C AUTOR: Ales Cepek, VUGTK,                                           *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MTGSO2(A,ADIM,M1,N1,N2,N4,MODE,SC,EPS,TOL,
     /                  NX,DF,PROC,IER,INDX,INDS,CP)
        IMPLICIT INTEGER (A-Z)
	INTEGER  ADIM,M1,N1,N2,N4,MODE,NX,DF,IER,INDX(1),INDS(1)
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
      INTEGER  INDEX,INIP,INJP	
      PARAMETER  (MAXINT=32767)
      INDEX(INIP,INJP) = (INJP-1)*MN1 + INIP   
C INDEX PRVKU V POLI A
C
C=======================================================================
C
C   OPERACNI CAST PROCEDURY MTGSO2
C
      N12   = N1 + N2      
C CELKOVY POCET SLOUPCU SOUSTAVY ROVNIC
      MN1   = M1 + N1      
C CELKOVY POCET RADKU ROZSIRENE MATICE
      M11   = M1 + 1       
C INDEX PRVNIHO RADKU DOLNI SUBMATICE
      MODE1 = MODE         
C INTERNI SPECIFIKACE VARIANTY RESENI
      TOL1  = TOL          
C INTERNI TOLERANCE LINEARNE ZAVISLYCH SLOUPCU
      ZMENA = 0            
C POMOCNA PROMENNA PRO TISK INDEXU NOREM
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
         DF  = 0           
C DEFEKT MATICE SOUSTAVY ROVNIC OPRAV
         PRV = .TRUE.      
C INTERNI PRIZNAK PRVNI ORTOGONALIZACE
         ASSIGN 1 TO RTSORT
         GOTO 10           
C VOLANI INTERNI ORTOGONALIZACNI PROCEDURY
      END IF
1     CONTINUE
C
C   DRUHA ORTOGONALIZACE
C
      IF(MODE1.GT.1.AND.DF.GT.0) THEN
         MODE1 = 3
         IF(NX.LT.DF) THEN
            IER = 1        
C RESENI NENI JEDNOZNACNE DEFINOVANO
            RETURN
         END IF
         INDX(NX+1) = N1+1 
C DEFINUJE KONEC CYKLU
         PRV = .FALSE.     
C INTERNI PRIZNAK DRUHE ORTOGONALIZACE
         ASSIGN 2 TO RTSORT
         GOTO 10           
C VOLANI INTERNI ORTOGONALIZACNI PROCEDURY
      END IF
2     RETURN               
C KONEC OPERACNI CASTI PROCEDURY MTGSO2
C
C=======================================================================
C
C   INTERNI ORTOGONALIZACNI PROCEDURA
C
10    CONTINUE
      IF(PRV) THEN
         N = N1            
C POCET ORTOGONALIZOVANYCH SLOUPCU
      ELSE
         N = DF
      END IF
C
C   VSTUPNI NORMALIZACE
C
      IF(SC) THEN
         IF(PRV) THEN
            HMEZ = M1 - 1
            DMEZ = 1       
C NORMALIZACE ZACINA PRVNIM SLOUPCEM
            DO 100 J=1,N   
C CYKLUS PRO VSECHNY ORTOGONALIZOVANE SLOUPCE
               MAXIM = 0E0 
C HLEDA SE V ABS. HODNOTE NEJVETSI PRVEK
               DO 110 I=DMEZ,DMEZ+HMEZ
110               IF(ABS(A(I)).GT.MAXIM)  MAXIM = ABS(A(I))
               IF(MAXIM.GT.0E0) THEN
                  DO 120 I=DMEZ,DMEZ+HMEZ
120                  A(I) = A(I)/MAXIM
                  A(DMEZ+HMEZ+J) = 1E0/MAXIM   
C PRVEK NA DIAGONALE
               END IF
               DMEZ = DMEZ + MN1   
C ZACATEK NASLEDUJICIHO SLOUPCE
100         CONTINUE
         ELSE
            DO 200 J=1,N   
C CYKLUS PRO VSECHNY ORTOGONALIZOVANE SLOUPCE
               INDSJ = ABS(INDS(J))      
C INDEX ZPRACOVAVANEHO SLOUPCE
               MAXIM = 0E0
               DO 210 K=1,MAXINT
                  IF(INDX(K).GT.INDSJ)  GOTO 215
                  I = INDEX(M1+INDX(K),INDSJ)
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
      DO 300 J=1,N         
C CYKLUS PRO VSECHNY ORTOGONALIZOVANE SLOUPCE
C        VYTVORENI KOPIE BEZNEHO VEKTORU V POLI CP, VYPOCET SKALARNIHO
C        SOUCINU (CP*CP)
         IF(PRV) THEN
            IMAX = M1 + J
            KMIN = MAX(N4,J) + 1    
C  1. SLOUPEC NEORTOGONALNI K J-TEMU
            K = (J-1)*MN1  
C J-TY SLOUPEC ZACINA PRVKEM A(K+1)
            DO 410 I=1,IMAX
410            CP(I) = A(K+I)       
C A(I,J)
            SK = 0D0
            DO 420 I=1,M1
420            SK = SK + CP(I)**2
            NORM = SQRT(SK)
C           ZMENA PORADI ORTOGONALIZACE SLOUPCU PRO SOURADNICE BODU
            IF(MOD(J-N4,2).EQ.1.AND.NORM.GE.TOL1) THEN
               K = K + MN1 + 1      
C SLOUPEC J+1 ZACINA PRVKEM A(K)
               SKP = 0D0   
C SKALARNI SOUCIN PRO NASLEDUJICI SLOUPEC
               DO 430 I=K,K+M1-1
430               SKP = SKP + DBLE(A(I))**2
               NORMP = SQRT(SKP)
               IF(NORMP.GE.TOL1) THEN
                  IF(ABS(A(INDEX(M1+J+1,J+1)))/NORMP.LT.
     /            0.7*ABS(A(INDEX(M1+J,J)))/NORM) THEN
                     K = (J-1)*MN1  
C SLOUPEC J ZACINA PRVKEM A(K+1)
                     K1 = K + MN1   
C SLOUPEC J+1 ZACINA PRVKEM A(K1+1)
                     DO 440 I=1,M11+J   
C VYMENA SLOUPCU J A J+1
                        KI = K + I
                        KI1 = K1 + I
                        CP(I) = A(KI1)  
C A(I,J+1)
                        A(KI1) = A(KI)  
C A(I,J)
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
500            CP(I) = A(K+I)       
C A(I,INDS(J))
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
            DF = DF + 1   
C CITAC LINEARNE ZAVISLYCH SLOUPCU
            IF(ZMENA.EQ.-1) THEN
               INDS(DF) = -J     
C DOSLO K VYMENE SLOUPCU
            ELSE
               INDS(DF) =  J
            END IF
            K1 = (J-1)*MN1
            DO 550 I=K1+1,K1+M1
550            A(I) = 0E0 
C VYMAZ LINEARNE ZAVISLEHO SLOUPCE
         ELSE
            IF(PRV) THEN
               K1 = (J-1)*MN1       
C J-TY SLOUPEC ZACINA PRVKEM A(K1+1)
               DO 600 I=1,IMAX
                  CP(I) = CP(I)/NORM
600               A(K1+I) = CP(I)
               K1 = (KMIN-1)*MN1    
C K-TY SLOUPEC ZACINA PRVKEM A(K1+1)
               DO 610 K=KMIN,N12
                  SK = 0D0
                  DO 620 I=1,M1     
C SK. SOUCIN SLOUPCU J A K
620                  SK = SK + CP(I)*A(K1+I)   
C A(I,K)
                  DO 630 I=1,IMAX
                     K2 = K1 + I
630                  A(K2) = A(K2) - SK*CP(I)
                  K1 = K1 + MN1
610            CONTINUE
            ELSE
               K1 = (INDSJ-1)*MN1   
C SLOUPEC INDS(J) ZACINA A(K1+1)
               DO 700 I=M11,IMAX
                  CP(I) = CP(I)/NORM
700               A(K1+I) = 0E0     
C LIN. ZAVISLY SLOUPEC VYNULOVAN
               K1 = 0               
C K-TY SLOUPEC ZACINA A(K1+1)
               DO 710 K=1,N12
                  SK = 0D0
                  DO 720 I=1,MAXINT
                     IF(INDX(I).GT.INDSJ)  GOTO 725
                     K2 = M1 + INDX(I)   
C CISLO RADKU V MATICI A
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
