C REDAKCE SEZNAMU BODU - SEZBOD
C=======================================================================
C         1. VERZE  /CERVEN 1984/         VUGTK
C         4. VERZE  /ZARI 1986/           ALES CEPEK      VUGTK
C       * 5. VERZE      /ZARI 1989/             VUGTK      ALES CEPEK
C
      SUBROUTINE GDTRSB
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  PIZBO,GDTSUM,II,CISLO(3),IER
      REAL  RPOM,GDSTCH
      INTEGER*1  BOD(13),TEXT(20)
      CHARACTER*20 CTEXT
      EQUIVALENCE (CTEXT,TEXT)
   13 FORMAT('0POCET IZOLOVANYCH, ZE SITE VYLOUCENYCH BODU:',I6)
14    FORMAT(2X,13A1,A4,',')
C
      IF(POCPOZ.LE.0)  RETURN
      PIZBO = GDTSUM(IZOBOD,SBPOC,'I')
C     IF(PIZBO.EQ.0)  RETURN
      IF(PIZBO.EQ.0)  GOTO 999
      CALL GDSSTR(2,8)
      WRITE(GSGDPT,13)  PIZBO
      WRITE(ITT   ,13)  PIZBO
      CALL GDTBU1(.TRUE.,.TRUE.)
C*    CALL GDTBUF('CISLA VYLOUCENYCH BODU:',0)
      CALL GDTBUF('CISLA VYLOUCENYCH BODU:',23)
      II = 1
0102  IF(II.GT.SBPOC)  GOTO 0199
0203     IF(IZOBOD(II).NE.'I')  GOTO 0299
            CISLO(1) = CB1(II)
            CISLO(2) = CB2(II)
            CISLO(3) = CB3(II)
            CALL GDSKCB(3,BOD,IER,CISLO,IER)
            RPOM = GDSTCH(CHAB(II))
C*    ENCODE(20,14,TEXT)  BOD,RPOM
      WRITE(CTEXT,14)     BOD,RPOM
      CALL GDTBUF(TEXT,20)
            TYPB(II) = ' '
0299     CONTINUE
         II = II + 1
         GOTO 0102
0199  CONTINUE
      CALL GDTBU2(1)
      PEVBOD = GDTSUM(TYPB,SBPOC,'P')
      URCBOD = GDTSUM(TYPB,SBPOC,'U')
      POCBOD = PEVBOD + URCBOD
C  *** 4
999   CALL GDTRSX
      RETURN
C
      END
