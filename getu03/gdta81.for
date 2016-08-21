C TISK HLAVICKY - VYPIS ORIENTACNICH POSUNU
C=======================================================================
C         1. VERZE  /ZARI 1984/         VUGTK
C
      SUBROUTINE GDTA81(LUN)
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  LUN
C
      IF(LUN.EQ.GSGDPT)  CALL GDSSTR(3,0)
      WRITE(LUN,81)
81    FORMAT('   I    STANOVISKO    PRIBLIZNA    KOREKCE',
     /'   VYROVNANA  STR.CH. KONF.I.'/21('='),' HODN. [G]',
     /' ==== [G] === HODN. [G] ===== +-[CC] ==='/)
C
      END
C TISK HLAVICKY - VYPIS SOURADNIC
C=======================================================================
C         1. VERZE  /ZARI 1984/         VUGTK
C
      SUBROUTINE GDTA84(LUN)
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  LUN
C
      IF(LUN.EQ.GSGDPT)  CALL GDSSTR(2,0)
      WRITE(LUN,84)
84    FORMAT('   I   CISLO BODU   PRIBLIZNA    KOREKCE    VYROVNANA',
     /'   STR.CH. KONF.I.'/20('='),' HODNOTA ===== [M] ===== HODNOTA',
     /' ======= +-[MM] ===')
C
      END
