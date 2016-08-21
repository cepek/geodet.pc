C UKONCENI PROGRAMU
C=======================================================================
C         1. VERZE      /SRPEN 1984/            VUGTK
C       * 5. VERZE      /DUBEN 1990/            VUGTK      ALES CEPEK
C
      SUBROUTINE GDTEXI
C
$INCLUDE: 'GDTSOW.COM'
C
      CALL GDDUDZ
      CALL GDTICZ('ZAPIS')
      CALL GDSSTR(4,0)
      WRITE(GSGDPT,1)
1     FORMAT(/1X,70('*')/'0KONEC')
      CALL TTEXIT(ITT,'GETU03')
C
      END
