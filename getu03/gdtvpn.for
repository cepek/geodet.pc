C VYPIS PREHLEDU NEZNAMYCH
C=======================================================================
C         1. VERZE  /ZARI 1984/         VUGTK
C
      SUBROUTINE GDTVPN(AW)
C
$INCLUDE: 'GDTSOW.COM'
      REAL  AW
C*    VIRTUAL  AW(VIRDIM)
      DIMENSION  AW(VIRDIM)
C
C POZADAVKY NA VYPIS
      CALL GDTPNV
      IF(KK.EQ.2)  RETURN
C VYPOCET KPRST
      CALL GDTPR3
C VYPIS ORIENTACNICH POSUNU
      IF(POCORP.GT.0)  CALL GDTVOP(AW)
C VYPIS SOURADNIC
      CALL GDTVSO(AW)
C
      END
