C TISK PREHLEDU NOREM
C======================================================================
C         1. VERZE      /SRPEN 1984/            VUGTK
C       * 5. VERZE      /ZARI 1989/             VUGTK      ALES CEPEK
C
      SUBROUTINE GDTTPN(K,NORM,TLR)
C
$INCLUDE: 'GDTSOW.COM'
C*    INTEGER   K      !!! MTGSO2 PRELOZENA S PARAMETREM /2I4
      INTEGER*4 K
C
      DOUBLE PRECISION  NORM
      REAL  TLR
2     FORMAT(I4,F12.7)
3     FORMAT(/' PRUBEH ORTOGONALIZACE - PREHLED NOREM:'/)
4     FORMAT(I4,F12.7,'    LIN. ZAVISLY SLOUPEC')
C
      CALL GDSSTR(1,0)
0203  IF(NORM.LT.TLR)  GOTO 0204
         WRITE(GSGDPT,2)  K, NORM
         WRITE(ITT   ,2)  K, NORM
         GOTO 0299
0204  CONTINUE
         WRITE(GSGDPT,4)  K, NORM
         WRITE(ITT   ,4)  K, NORM
0299  CONTINUE
      RETURN
C
      ENTRY GDTTP0
      CALL GDSSTR(3,POCNEZ+2)
      WRITE(GSGDPT,3)
      WRITE(ITT   ,3)
      RETURN
C
      END
