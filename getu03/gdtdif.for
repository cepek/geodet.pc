C VYPOCET DIF-D, DIF-SK, DIF-P
C=======================================================================
C         1. VERZE      /RIJEN 1984/            VUGTK
C       * 5. VERZE      /ZARI 1989/             VUGTK      ALES CEPEK
C
      SUBROUTINE GDTDIF(AW,IPOZ,IOPR,DIF,TEXT,NOVOSN,IPOC)
C
C IPOZ   VSTUPNI  PARAMETR, INDEX POZOROVANI V SEZMER
C IOPR   VSTUPNI  PARAMETR, INDEX OPRAVY
C DIF    VYSTUPNI PARAMETR, DIF-D NEBO DIF-P
C TEXT   VYSTUPNI PARAMETR, TEXTOVY RADEK PRO DANOU DIFERENCI
C NOVOSN VSTUPNI  PARAMETR, URCUJE PRVNI PRVEK OSNOVY
C IPOC   VYSTUPNI PARAMETR, 2 NEBO 1 PRO NOVOSN RESP. .NOT.NOVOSN
C
C
$INCLUDE: 'GDTSOW.COM'
      REAL  AW
C*    VIRTUAL  AW(VIRDIM)
      DIMENSION  AW(VIRDIM)
      INTEGER  IPOZ,IOPR,IPOC
      REAL  DIF
      INTEGER*1  TEXT(71)
      CHARACTER*71 CTEXT
      LOGICAL  NOVOSN
      INTEGER  I,J,GDTSUM,IER
      INTEGER*4 GDTIND
      DOUBLE PRECISION  Y(2),X(2),SM,D,INTPRV,OPRAVA,IDIFSK,ZOPR
      INTEGER*1  S(13),C(13)
C*    INTEGER  NNN
C*    PARAMETER  (NNN=7)
C*1151  FORMAT('0',13A1,2X,13A1,4X,A1,F10.3,2('      -   '),<NNN>X)
C*1152  FORMAT('0',13A1,2X,13A1,4X,A1,'      -   ',2F10.3,  <NNN>X)
1151  FORMAT('0',13A1,2X,13A1,4X,A1,F10.3,2('      -   '),7X)
1152  FORMAT('0',13A1,2X,13A1,4X,A1,'      -   ',2F10.3,  7X)
C
      I = CST(IPOZ)
      Y(1) = SBY(I)
      X(1) = SBX(I)
0103  IF(TYPB(I).NE.'U')  GOTO 0199
         J = POCPOZ + POCORP + 2*GDTSUM(TYPB,I,'U')
         X(1) = X(1) + AW(GDTIND(J,NARED))*1E-3
         J = J - 1
         Y(1) = Y(1) + AW(GDTIND(J,NARED))*1E-3
0199  CONTINUE
      I = CCI(IPOZ)
      Y(2) = SBY(I)
      X(2) = SBX(I)
0203  IF(TYPB(I).NE.'U')  GOTO 0299
         J = POCPOZ + POCORP + 2*GDTSUM(TYPB,I,'U')
         X(2) = X(2) + AW(GDTIND(J,NARED))*1E-3
         J = J - 1
         Y(2) = Y(2) + AW(GDTIND(J,NARED))*1E-3
0299  CONTINUE
      CALL NGSMDA(Y,X,SM,D,IER)
      CALL GDTEXT(CST(IPOZ),S)
      CALL GDTEXT(CCI(IPOZ),C)
      INTPRV = PRVEK(IPOZ)
      OPRAVA = AW(GDTIND(IOPR,NARED))/ABS(VAHA(IPOZ))
0303  IF(TYPP(IPOZ).NE.'D')  GOTO 0304
         DIF = (D-INTPRV)*1D3 - OPRAVA
C*       ENCODE(71,1151,TEXT)  S,C,TYPP(IPOZ),DIF
         WRITE(CTEXT,1151)     S,C,TYPP(IPOZ),DIF
         GOTO 0399
0304  CONTINUE
         I = POCPOZ
         DO 300 J=1,CST(IPOZ)
300      IF(SBZ(J).GE.0D0)  I= I + 1
         ZOPR = AW(GDTIND(I,NARED))
         IDIFSK = (SM - SBZ(CST(IPOZ)) - INTPRV)*RO*1D4 - ZOPR - OPRAVA
         IF(IDIFSK.GT.+350D4)  IDIFSK = IDIFSK - 400D4
         IF(IDIFSK.LT.-350D4)  IDIFSK = IDIFSK + 400D4
         DIF = IDIFSK*D*(DVEPI/4D3)
C*       ENCODE(71,1152,TEXT)  S,C,TYPP(IPOZ),IDIFSK,DIF
         WRITE(CTEXT,1152)     S,C,TYPP(IPOZ),IDIFSK,DIF
0399  CONTINUE
0403  IF(.NOT.NOVOSN)  GOTO 0404
         IPOC = 2
         GOTO 0499
0404  CONTINUE
         IPOC = 1
C*       CALL PPINI1(' ',14,TEXT)
         CTEXT(:14) = ' '
0499  CONTINUE
      CALL PPMOV1(1,CTEXT,1,TEXT,1,71)
C
      END
