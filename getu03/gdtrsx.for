C VYMENA PREDPOSLEDNIHO URCOVANEHO BODU V SEZMER
C====================================================================
C         4. VERZE   /ZARI 1986/      ALES CEPEK      VUGTK
C
      SUBROUTINE GDTRSX
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  IND1,IND2,IND3,IPOM
      DOUBLE PRECISION  MAXD,DPOM
      INTEGER*1  BPOM
      EQUIVALENCE  (IPOM,DPOM,BPOM)
C
      IF(URCBOD.LE.2)  RETURN
      DO 100 I=1,SBPOC
         IF(TYPB(I).NE.'U')  GOTO 100
C PREDPOSLEDNI URCOVANY BOD
            IND2 = IND1
C POSLEDNI     URCOVANY BOD
            IND1 = I
100   CONTINUE
      MAXD = 0D0
      DO 200 I=1,IND2
         IF(TYPB(I).NE.'U')  GOTO 200
            DPOM = (SBY(IND1)-SBY(I))**2 + (SBX(IND1)-SBX(I))**2
            IF(DPOM.LE.MAXD)  GOTO 200
               IND3 = I
               MAXD = DPOM
200   CONTINUE
C
      IPOM      = CB1(IND3)
      CB1(IND3) = CB1(IND2)
      CB1(IND2) = IPOM
C
      IPOM      = CB2(IND3)
      CB2(IND3) = CB2(IND2)
      CB2(IND2) = IPOM
C
      IPOM      = CB3(IND3)
      CB3(IND3) = CB3(IND2)
      CB3(IND2) = IPOM
C
      DPOM      = SBY(IND3)
      SBY(IND3) = SBY(IND2)
      SBY(IND2) = DPOM
C
      DPOM      = SBX(IND3)
      SBX(IND3) = SBX(IND2)
      SBX(IND2) = DPOM
C
      DPOM      = SBZ(IND3)
      SBZ(IND3) = SBZ(IND2)
      SBZ(IND2) = DPOM
C
      BPOM       = CHAB(IND3)
      CHAB(IND3) = CHAB(IND2)
      CHAB(IND2) = BPOM
C
      BPOM       = CHAS(IND3)
      CHAS(IND3) = CHAS(IND2)
      CHAS(IND2) = BPOM
C
C TYPB A IZOBOD  MUSI MIT STEJNE HODNOTY !!!
C     BPOM       = TYPB(IND3)
C     TYPB(IND3) = TYPB(IND2)
C     TYPB(IND2) = BPOM
C
C     BPOM         = IZOBOD(IND3)
C     IZOBOD(IND3) = IZOBOD(IND2)
C     IZOBOD(IND2) = BPOM
C
      DO 300 I=1,SMPOC
0303     IF(CST(I).NE.IND2)  GOTO 0304
            CST(I) = IND3
            GOTO 0399
0304     IF(CST(I).NE.IND3)  GOTO 0399
            CST(I) = IND2
0399     CONTINUE
0403     IF(CCI(I).NE.IND2)  GOTO 0404
            CCI(I) = IND3
            GOTO 0499
0404     IF(CCI(I).NE.IND3)  GOTO 0499
            CCI(I) = IND2
0499     CONTINUE
300   CONTINUE
      RETURN
C
      END
