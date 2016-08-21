C PROCEDURA GDTPAU PRO POZASTAVENI TISKU NA OBRAZOVKU
C======================================================================
C         1. VERZE  /SRPEN 1984/         VUGTK
C
C WAIT  LOGICKA PROMENNA - POZADAVEK NA POZASTAVENI TISKU
C
      SUBROUTINE GDTPAU(WAIT)
C
$INCLUDE: 'GDTSOW.COM'
      LOGICAL  WAIT
      INTEGER*1  ZN
C
      IF(.NOT.WAIT)  RETURN
100   CONTINUE
      CALL TTASKS(ITT,'STISKNI KLAVESU <ENTER> : ',IOUT,ZN,1)
      WAIT = IOUT.NE.-2
      IF(IOUT.NE.-1)  RETURN
      WRITE(ITT,1)
1     FORMAT(/
     /' <   >  ... VYPIS PO CASTECH'/
     /' <END>  ... SOUVISLY VYPIS'/)
      GOTO 100
C
      END
