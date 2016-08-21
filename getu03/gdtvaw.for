C POMOCNA PROCEDURA PRO HLEDANI PRVKU V SEZMER PRI ARCHIVACI
C=======================================================================
C         1. VERZE  /RIJEN 1984/         VUGTK
C
      SUBROUTINE GDTVAW(TYP,IPOZ,IPRV,TPRV)
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  IPOZ,IPRV,I
      INTEGER*1  TYP,TPRV(MAXMER)
C
      DO 100 I=1,SMPOC
         IF(TPRV(I).NE.TYP)  GOTO 100
         IF(CST(IPOZ).NE.CST(I))  GOTO 100
         IF(CCI(IPOZ).NE.CCI(I))  GOTO 100
         TPRV(I) = ' '
         IPRV = I
         RETURN
100   CONTINUE
      IPRV = 0
      RETURN
C
      END
