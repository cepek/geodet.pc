C+
C PPRES1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro presun dat                                     *
C AUTOR: Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE PPRES1 (ZDROJ,INDEXZ,CIL,INDEXC,POCETB)
	INTEGER*1 ZDROJ(1),CIL(1)
	INTEGER   INDEXZ,INDEXC,POCETB
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura prenasi volitelny pocet bytu ze zdrojove do cilo- *
C ve oblasti v operacni pameti. Obe oblasti jsou definovany identifi- *
C katorem pole a indexem prvniho prenaseneho bytu. Procedury lze rov- *
C nez uzit pro presuny dat  v  ramci jednoho pole, a to i tehdy, kdyz *
C cilova a zdrojova oblast se prekryvaji.                             *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    ZDROJ  - identifikator zdrojoveho pole                           *
C    INDEXZ - index prvniho prenaseneho bytu v poli ZDROJ             *
C    INDEXC - index bytu v poli CIL, od nejz  pocinaje  budou  uloze- *
C             ny prenesene udaje                                      *
C    POCETB - pocet prenasenych bytu                                  *
C VYSTUPNI PARAMETR:                                                  *
C    CIL    - identifikator ciloveho pole                             *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	INTEGER I,IZ,IC,JEDNA
C
	IF (POCETB.LE.0) RETURN
	IZ=INDEXZ
	IC=INDEXC
	JEDNA=1
	IF (INDEXC.LE.INDEXZ) GOTO 10
	IZ=IZ+POCETB-1
	IC=IC+POCETB-1
	JEDNA=-1
10	DO 20 I=1,POCETB
	   CIL(IC)=ZDROJ(IZ)
	   IZ=IZ+JEDNA
	   IC=IC+JEDNA
20	CONTINUE
	RETURN
	END
