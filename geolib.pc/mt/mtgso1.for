C+
C MTGSO1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro reseni soustav rovnic oprav algoritmem GSO     *
C AUTOR: Ales Cepek, VUGTK,                                           *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MTGSO1(A,ADIM,M1,N1,N2,N4,MODE,SC,EPS,TOL,
     /                  NX,DF,PROC,IER,INDX,INDS,CP)
        IMPLICIT INTEGER (A-Z)
	INTEGER  ADIM,M1,N1,N2,N4,MODE,NX,DF,IER,INDX(1),INDS(1)
	LOGICAL  SC
	REAL     A(ADIM),EPS,TOL
	DOUBLE PRECISION CP(1)
	EXTERNAL PROC
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura je urcena predevsim k reseni soustav rovnic oprav *
C    vznikajicich pri vyrovnani zprostredkujicich pozorovani. V sou-  *
C    ladu s principem metody nejmensich ctvercu najde procedura vek-  *
C    tor reseni soustavy rovnic oprav tak, ze odpovidajici vektor     *
C    oprav ma minimalni delku (euklidovskou normu). Je-li hodnost ma- *
C    tice soustavy rovnic oprav neuplna, tj. vykazuje-li tato matice  *
C    nenulovy defekt, pak bude navic splnena i vedlejsi podminka: bu- *
C    de minimalizovana delka vektoru neznamych, prip. delka vektoru,  *
C    tvoreneho libovolne zadanou podmnozinou mnoziny slozek vektoru   *
C    neznamych. Vedle vektoru neznamych najde procedura i vektor      *
C    oprav a matice vahovych koeficientu vyrovnanych hodnot merenych  *
C    velicin i neznamych (ve faktorizovanem tvaru).                   *
C         Zakladem procedury MTGSO1 je algoritmus GSO, jehoz jadro    *
C    tvori Gramova-Schmidtova ortogonalizace.                         *
C PREHLED PARAMETRU - detailni popis viz textovou cast dokumentace    *
C VSTUPNI PARAMETRY:                                                  *
C    ADIM   - pocet prvku prideleny pracovnimu poli A deklaraci ve    *
C             volajicim programu                                      *
C    M1     - pocet rovnic oprav                                      *
C    N1     - pocet neznamych                                         *
C    N2     - pocet vektoru absolutnich clenu                         *
C    N4     - pocet apriorne ortogonalnich sloupcu v leve casti mati- *
C             ce soustavy rovnic oprav                                *
C    MODE   - specifikace varianty reseni                             *
C    SC     - specifikace pozadavku na vstupni normalizaci            *
C    EPS    - charakteristika relativni presnosti uziteho pocitace    *
C    TOL    - tolerance pro identifikaci linearne zavislych sloupcu v *
C             ortogonalizovanych maticich                             *
C    NX     - pocet prvku subvektoru neznamych, jehoz delka ma byt    *
C             minimalizovana                                          *
C    INDX   - pole pro registraci indexu neznamych, ktere vstupuji do *
C             minimalizacni podminky v pripade nenuloveho defektu     *
C VYSTUPNI PARAMETR:                                                  *
C    IER    - IER = 0 regularni uloha                                 *
C             IER = 1 singularita - matice soustavy rovnic oprav ma   *
C                     neuplnou hodnost a minimalizacni podminky nede- *
C                     finuji jednoznacne vektor neznamych             *
C VSTUPNI/VYSTUPNI PARAMETRY:                                         *
C    A      - pracovni pole pro ulozeni ortogonalizovane matice; pred *
C             vyvolanim procedury MTGSO1 musi byt do pole A po sloup- *
C             cich ulozena tzv. vstupni matice; pri vystupu je v pra- *
C             covnim poli analogicky zapsana tzv. vystupni matice -   *
C             viz textovou cast dokumentace                           *
C    DF     - defekt matice soustavy rovnic oprav                     *
C    INDS   - pole pro registraci indexu linearne zavislych sloupcu v *
C             matici soustavy rovnic oprav                            *
C OSTATNI PARAMETRY:                                                  *
C    PROC   - procedura, ktere MTGSO1 prubezne predava na zaver zpra- *
C             covani urcitych sloupcu vstupni matice hodnoty tri pa-  *
C             rametru: sloupcoveho indexu, euklidovske normy sloupce  *
C             a aktualni hodnoty tolerance TOL                        *
C    CP     - prostor k prechodnemu ulozeni sloupcu ortogonalizovane  *
C             matice                                                  *
C ARITMETIKA SMISENA                                                  *
C EXTERNI PROCEDURA: pomocna procedura, kterou uzivatel muze zpraco-  *
C                    vat nektere mezivysledky, charakterizujici pru-  *
C                    beh vypoctu (viz parametr PROC)                  *
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
C   OPERACNI CAST PROCEDURY MTGSO1
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
C KONEC OPERACNI CASTI PROCEDURY MTGSO1
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
