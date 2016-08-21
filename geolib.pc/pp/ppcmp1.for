C+
C PPCMP1 PPCMP2
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedury pro srovnani obsahu dvou poli                      *
C AUTOR: Ales Cepek, VUGTK                                            *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      INTEGER FUNCTION PPCMP1(LPA,LPB,IDIM)
C     INTEGER FUNCTION PPCMP2(IPA,IPB,IDIM)
      INTEGER    IDIM
      INTEGER*1  LPA(1), LPB(1)
C     INTEGER    IPA(1), IPB(1)   
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura PPCMP1, resp. PPCMP2, porovnava obsah dvou poli   *
C    po slabikach (bytech), resp. po slovech (INTEGER). Vysledek      *
C    porovnani predava jako funkcni hodnotu. Absolutni hodnota funkc- *
C    ni hodnoty PPCMP1, resp. PPCMP2, udava "index" prvni nalezene    *
C    dvojice ruznych prvku; je-li funkcni hodnota rovna  nule, je ob- *
C    sah obou poli stejny. Zaporna funkcni hodnota vyjadruje, ze pole *
C    A "je mensi" nez pole B; kladna funkcni hodnota vyjadruje, ze    *
C    pole A "je vetsi" nez pole B.                                    *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    LPA, LPB - identifikatory (prvky) porovnavanych poli typu        *
C               INTEGER*1                                             *
C    IPA, IPB - identifikatory (prvky) porovnavanych poli typu        *
C               INTEGER                                               *
C    IDIM     - pocet prvku specifikovanych poli, pro ktere bude      *
C               provedeno porovnani                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
C
      DO 100 I=1,IDIM
         IF(LPA(I).LT.LPB(I)) THEN
	    PPCMP1 = -I
	    RETURN
	 END IF
         IF(LPA(I).GT.LPB(I)) THEN
	    PPCMP1 = I
	    RETURN
	 END IF
100   CONTINUE
      PPCMP1 = 0
      RETURN
      END
C     
      INTEGER FUNCTION PPCMP2(IPA,IPB,IDIM)
C
      INTEGER    IDIM
      INTEGER    IPA(1), IPB(1)   
C
      DO 200 I=1,IDIM
         IF(IPA(I).LT.IPB(I)) THEN
	    PPCMP2 = -I
	    RETURN
	 END IF
         IF(IPA(I).GT.IPB(I)) THEN
	    PPCMP2 = I
	    RETURN
	 END IF
200   CONTINUE
      PPCMP2 = 0
      RETURN
      END
