C URCENI POCTU OBJEKTU DANEHO TYPU V SEZNAMU
C=======================================================================
C         1. VERZE  /CERVEN 1984/         VUGTK
C
      INTEGER FUNCTION GDTSUM(SEZNAM,DIM,TYP)
C
      INTEGER  DIM,SUMA,I
      INTEGER*1  SEZNAM(1),TYP
C
      SUMA = 0
0103  IF(DIM.LE.0)  GOTO 0199
0202     DO 0299 I=1,DIM
            IF(SEZNAM(I).EQ.TYP)  SUMA = SUMA + 1
0299     CONTINUE
0199  CONTINUE
      GDTSUM = SUMA
      RETURN
C
      END
