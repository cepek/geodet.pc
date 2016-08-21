C PRIPRAVA MERENYCH PRVKU
C=======================================================================
C         1. VERZE  /CERVEN 1984/         VUGTK
C
      SUBROUTINE GDTPM2
C
$INCLUDE: 'GDTSOW.COM'
      EXTERNAL GDDHRS
C
      CALL TTZAHL(ITT,'PRIPRAVA MERENYCH PRVKU:')
      CALL GDTPM4
      CALL GDTPM3(1,SBPOC,GDDHRS)
      CALL GDTKON
C
      END
