C TISK HLAVICKY S VOLITELNYM TISKEM NAZVU ULOHY A VARIANTY RESENI
C=======================================================================
C         1. VERZE  /ZARI 1984/         VUGTK
C
      SUBROUTINE GDTTHL(LUN,RET)
C
$INCLUDE: 'GDTSOW.COM'
      INTEGER  LUN,PPBIT1,TTLENF,I,II
      INTEGER*1  RET(1),TEXT(70)
1     FORMAT(1X,70A1)
C
      IF(LUN.EQ.GSGDPT)  CALL GDSSTR(4,10)
0103  IF(.NOT.(LUN.EQ.GSGDPT.AND.PPBIT1(PK,6).EQ.0))  GOTO 0199
C*       CALL PPINI1(' ',70,TEXT)
         DO 9981 II81=1,70
9981        TEXT(II81) = ' '	
         CALL PPMOV1(1,ULOHA,1,TEXT,1,TTLENF(ULOHA))
         DO 100 I=25,1,-1
         IF(NAZVAR(I).NE.' ')  GOTO 150
100      CONTINUE
         I = 1
150      CALL PPMOV1(1,NAZVAR,1,TEXT,71-I,I)
         CALL GDSSTR(2,0)
         WRITE(LUN,1)
         WRITE(LUN,1)  TEXT
0199  CONTINUE
      II = TTLENF(RET)
      WRITE(LUN,1)
      WRITE(LUN,1)  (RET(I),I=1,II)
      WRITE(LUN,1)  ('*'   ,I=1,II)
      WRITE(LUN,1)
      RETURN
C
      END
