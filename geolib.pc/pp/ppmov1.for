C+
C PPMOV1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro presun prvku z jednoho pole do druheho         *
C AUTOR: Ales Cepek, VUGTK                                            *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE PPMOV1(IT,ZDROJ,INDZ,CIL,INDC,POCETP)
      INTEGER   IT, INDZ, INDC, POCETP
      INTEGER*1 ZDROJ(1), CIL(1)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura presune zadany pocet prvku ze zdrojoveho do cilo- *
C    veho pole, od volitelnych pozic v obou polich pocinaje. Procedu- *
C    ry lze rovnez uzit pro presuny dat v ramci jednoho pole, a to i  *
C    tehdy, kdyz zdrojova a cilova oblast se prekryvaji.              *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETRY:                                                  *
C    IT     - delka prvku pole v bytech                               *
C    ZDROJ  - zdrojove pole                                           *
C    INDZ   - index prvniho prenaseneho prvku v poli ZDROJ            *
C    INDC   - index prvku v poli CIL, od nejz pocinaje budou ulozeny  *
C             prenesene udaje                                         *
C    POCETP - pocet prenasenych prvku                                 *        
C VYSTUPNI PARAMETR:                                                  *
C    CIL    - cilove pole                                             *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
C LOKALNI PROMENNE
      INTEGER  I,IZ,IC,JEDNA,INDEXZ,INDEXC,POCETB
      INDB(ITF,IND) = (IND-1)*ITF + 1   
C
      INDEXZ = INDB(IT,INDZ)
      INDEXC = INDB(IT,INDC)
      POCETB = IT*POCETP
      IF (POCETB.LE.0) RETURN
      IZ=INDEXZ
      IC=INDEXC
      JEDNA=1
      IF (INDEXC.LE.INDEXZ) GOTO 10
      IZ=IZ+POCETB-1
      IC=IC+POCETB-1
      JEDNA=-1
10    DO 20 I=1,POCETB
         CIL(IC)=ZDROJ(IZ)
         IZ=IZ+JEDNA
         IC=IC+JEDNA
20    CONTINUE
      RETURN
C
      END
