C VYPOCET INDEXU V JEDNOROZMERNEM POLI PRO UKLADANI MATICE ROVNIC OPRAV
C======================================================================
C         1. VERZE      /SRPEN 1984/            VUGTK
C       * 5. VERZE      /ZARI 1989/             VUGTK      ALES CEPEK
C
C I,J   INDEXY PRVKU MATICE ROVNIC OPRAV
C
C*    INTEGER FUNCTION GDTIND(I,J)
      INTEGER*4 FUNCTION GDTIND(I,J)
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  I,J
C
      INTEGER*4  M4, I4, J4
C
      M4 = MARED
      I4 = I
      J4 = J
      GDTIND = M4*(J4-1)+I4
      RETURN
C
      END
