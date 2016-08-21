C TEST A VYPIS PREHLEDU OPRAV, DETEKCE SLABYCH MIST SITE
C=======================================================================
C         1. VERZE  /ZARI 1984/         VUGTK
C
      SUBROUTINE GDTTVO(AW)
C
$INCLUDE: 'GDTSOW.COM'
      REAL  AW
C*    VIRTUAL  AW(VIRDIM)
      DIMENSION  AW(VIRDIM)
      LOGICAL TEST
      REAL  CALFA
C
      CALL GDTPR2(TEST,CALFA)
      CALL GDTPNV
      IF(KK.EQ.2)  RETURN
      CALL GDTVPO(AW,TEST,CALFA)
C
      END
