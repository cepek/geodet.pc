C VYTVORENI STRUKTURY SOUSEDNOSTI - ADJACENCY STRUCTURE (XADJ, ADJNCY)
C=======================================================================
C
      SUBROUTINE GTVADJ(LUN,ADJ,MAXADJ,IOUT)
C
      INTEGER  LUN, ADJ(1), MAXADJ, IOUT
C
C VSTUPNI PARAMETR:
C 
C LUN     CISLO VSTUPNIHO LOGICKEHO ZARIZENI
C
C VYSTUPNI PARAMETRY:
C
C ADJ     V TOMTO POLI BUDE NA VYSTUPU ULOZENA STRUKTURA SOUSEDNOSTI
C         MATICE TRANS(A)*A. STRUKTURU SOUSEDNOSTI (ADJACENCY STRUCTURE)
C         TVORI DVOJICE POLI (XADJ, ADJNCY), KTERA JSOU PROCEDUROU
C         GTVADJ V POLI ADJ ULOZENA ZA SEBOU. PRI VOLANI PROCEDUR
C         ZE SPARSPAK SE PAK NA MISTE FORMALNICH PARAMETRU (XADJ,ADJNCY)
C         UVADI (ADJ,ADJ).
C         POZN.: PRI SEKVECNIM ZPRACOVANI MATICE  A  SE NEJPRVE
C                STRUKTURA SOUSEDNOSTI TRANS(A)*A VYTVARI V PRACOVNIM
C                SPOJOVEM SEZNAMU. TENTO SEZNAM UKLADAN VZDY PO DVOU
C                PRVCICH OD KONCE POLE  ADJ  NASLEDOVNE:
C                   ADJ(N)    SMERNIK NA DALSI SOUSEDNI VRHCOL,
C                  ADJ(N+1)   CISLO VRCHOLU (NEZNAME).
C MAXADJ  NA VSTUPU UDAVA DIMENZI POLE ADJ, NA VYSTUPU POCET PRVKU V 
C         POLI ADJ POUZITYCH PRO ULOZENI STRUKTURY SOUSEDNOSTI
C IOUT    VYSTUPNI PARAMETR:
C         IOUT =  0  V POLI ADJ ULOZENA STRUKTURA SOUSEDNOSTI
C              = -1  NEDOSTATECNA KAPACITA POLE ADJ
C              = -2  CHYBA PRI CTENI MATICE A
C
C PROCEDURA GTVADJ VOLA PRO CTENI RADKU MATICE PROCEDURU GTGETR
C
C-----------------------------------------------------------------------
C
C
      INTEGER   MAXSLP
      PARAMETER (MAXSLP=100)
C$INCLUDE: 'GTREAL.INC'
      DOUBLE PRECISION
     /          ROW(MAXSLP), ABSCL, W
      INTEGER   I, J, II, JJ, NONZ, INDC(MAXSLP), BUFPTR, PTR,
     /          POCNEZ, POCPOZ
      LOGICAL   FOUND
C
C     ------------
C     INICIALIZACE
C     ------------
      CALL GTGET0(LUN,POCNEZ,POCPOZ,IOUT)
      IF(IOUT.NE.0)  RETURN
      DO 101 I=1,POCNEZ
         ADJ(I) = -1
101   CONTINUE
      BUFPTR = MAXADJ + 1
C
200   CALL GTGETR(LUN,NONZ,INDC,ROW,ABSCL,W,IOUT)
         IF(IOUT.LT.0)  GOTO 299
C        ---------------------------------
C        ZPRACOVANI NENULOVYCH PRVKU RADKU
C        ---------------------------------
         DO 300 II=1,NONZ
            I = INDC(II)
            DO 400 JJ=1,NONZ
               J = INDC(JJ)
               IF(I.EQ.J)  GOTO 400
               IF(J.GT.POCNEZ)  GOTO 400
               PTR = ADJ(I)
C              ----------------------------------------------------
C              HLEDANI HRANY (I,J) VE SPOJOVEM SEZNAMU NA KONCI ADJ
C              ----------------------------------------------------
600               IF(PTR.LE.0)  GOTO 500
                  IF(ADJ(PTR+1) .EQ. J)  GOTO 400
                  PTR = ADJ(PTR)
                  GOTO 600
500            CONTINUE
C              ----------------------------------------------------
C              HRANA (I,J) NENALEZENA - ULOZIT DO SPOJOVEHO SEZNAMU
C              ----------------------------------------------------
               BUFPTR = BUFPTR - 2
               IF(POCNEZ.GE.BUFPTR)  GOTO 900
               ADJ(BUFPTR)   = ADJ(I)
               ADJ(BUFPTR+1) = J
               ADJ(I) = BUFPTR
400         CONTINUE
300      CONTINUE
         GOTO 200
299   CONTINUE
      IF(IOUT.EQ.-2)  RETURN
C     -----------------------------------------------------
C     PREVOD STRUKTURY SOUSEDNOSTI DO FORMATU (ADJ, ADJNCY)
C     -----------------------------------------------------
      MAXADJ = POCNEZ + 1
      DO 700 I=1,POCNEZ
         PTR = ADJ(I)
         ADJ(I) = MAXADJ + 1
C        ------------------------------------------------
C        PRUCHOD SEZNAMEM VRCHOLU SOUSEDNICH S VRCHOLEM I
C        ------------------------------------------------
800         IF(PTR.LE.0)  GOTO 899
            MAXADJ = MAXADJ + 1
            IF(MAXADJ.GE.BUFPTR)  GOTO 900
            ADJ(MAXADJ) = ADJ(PTR+1)
            PTR = ADJ(PTR)
            GOTO 800
899      CONTINUE
700   CONTINUE      
      ADJ(POCNEZ+1) = MAXADJ + 1
      IOUT = 0
      RETURN
C
C     ------------------------------
C     NEDOSTATECNA KAPACITA POLE ADJ
C     ------------------------------
900   IOUT = -1
C
      RETURN
C
      END
