C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
C*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
C*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
C*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C       * 5. VERZE      /ZARI 1989/             VUGTK      ALES CEPEK
C
      SUBROUTINE LADENI
C
$INCLUDE: 'GDTSOW.COM'
      PARAMETER  (POMDIM=SOWDIM*4)
      INTEGER*1  WPOM(0:POMDIM)
      EQUIVALENCE  (WPOM,SOW)
C
      INTEGER*1  INSTR(80),MZN
      CHARACTER*80 CINSTR
      EQUIVALENCE (CINSTR,INSTR)
      INTEGER  IADR,IOD,IDO,IBYTE,I,J
C
      INTEGER*1  PBYTE(8),BB,LBB
      INTEGER*2  II
      INTEGER*4  JJ
      LOGICAL*2  L2
      LOGICAL*4  L4
      REAL*4     RR
      REAL*8     DD
      EQUIVALENCE  (PBYTE,BB,II,JJ,L2,L4,RR,DD)
C
99100 CONTINUE
      WRITE(ITT,99001)
99001 FORMAT('$LADENI>')
      READ (ITT,99002,ERR=99999,END=99999)  INSTR
99002 FORMAT(80A1)
      IF(INSTR(1).EQ.'?')  WRITE(ITT,99003)
99003 FORMAT(
     /' SYNTAX POVELU: T ADR [OD [DO] ]'/
     /' T  ......  TYP PROMENNE / POLE'/
     /'            B  BYTE'/
     /'            I  INTEGER*2 / LOGICAL*2'/
     /'            J  INTEGER*4 / LOGICAL*4'/
     /'            R  REAL'/
     /'            D  DOUBLE PRECISION'/
     /' ADR  ....  RELATIVNI ADRESA (OKTALOVE) V COMMON BLOKU'/
     /' OD  .....  UDAVA OD KTEREHO PRVKU (V POLI ZACINAJICIM NA '/
     /'            ADRESE ADR) ZACINA VYPIS (IMPLICITNE OD=1)'/
     /' DO  .....  UDAVA, KTERYM PRVKEM KONCI VYPIS POLE'/
     /'            (IMPLICITNE DO=OD)'/
     /' CTRL/Z     UKONCUJE CINNOST LADICI PROCEDURY'/
     /)
      IF(INSTR(1).EQ.'?')  GOTO 99100
C
      MZN = 0
      II = 0
      DO 99200 I=1,80
         IF(INSTR(I).EQ.' '.OR.INSTR(I).EQ.'	')  INSTR(I) = ','
         IF(MZN.EQ.','.AND.INSTR(I).EQ.',')  GOTO 99200
            MZN = INSTR(I)
            INSTR(I) = ','
            II = II + 1
            INSTR(II) = MZN
99200 CONTINUE
C*    DECODE(80,99033,INSTR,ERR=99100)  IADR,IOD,IDO
C******* NENI KONVERZE O !!!    READ (CINSTR,99033,ERR=99100)     IADR,IOD,IDO
C*99033 FORMAT(2X,O10,2I10)
      IF(IOD.LE.0)  IOD = 1
      IF(IDO.LE.0)  IDO = IOD
      IBYTE = 0
      IF(INSTR(1).EQ.'B')  IBYTE = 1
      IF(INSTR(1).EQ.'I')  IBYTE = 2
      IF(INSTR(1).EQ.'J')  IBYTE = 4
      IF(INSTR(1).EQ.'R')  IBYTE = 4
      IF(INSTR(1).EQ.'D')  IBYTE = 8
C
      DO 99300 I=1,IDO
         DO 99350 J=1,IBYTE
            PBYTE(J) = WPOM(IADR)
99350       IADR = IADR + 1
         LBB = BB
         IF(BB.LT.32)  LBB = ' '
         IF(I.LT.IOD)  GOTO 99300
         IF(INSTR(1).EQ.'B')  WRITE(ITT,99351)  I,BB,LBB
99351    FORMAT(' INDEX:',I5,'    PRVEK:',I4,3X,A1)
         IF(INSTR(1).EQ.'I')  WRITE(ITT,99352)  I,II,L2
99352    FORMAT(' INDEX:',I5,'    PRVEK:',I6,L4)
         IF(INSTR(1).EQ.'J')  WRITE(ITT,99353)  I,JJ,L4
99353    FORMAT(' INDEX:',I5,'    PRVEK:',I12,L4)
         IF(INSTR(1).EQ.'R')  WRITE(ITT,99354)  I,RR
99354    FORMAT(' INDEX:',I5,'    PRVEK:',E16.8)
         IF(INSTR(1).EQ.'D')  WRITE(ITT,99355)  I,DD
99355    FORMAT(' INDEX:',I5,'    PRVEK:',D26.18)
99300 CONTINUE
      GOTO 99100
99999 CONTINUE
C
      END
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
C   VERZE URCENA PRO LADENI
C       * 5. VERZE      /ZARI 1989/             VUGTK      ALES CEPEK
C
      SUBROUTINE TTASKS(ITT,TEXT,IOUT,LPOLE,IDIM)
C
      INTEGER*2  ITT,IOUT,IDIM,TTLEN1
      CHARACTER  TEXT(1),LPOLE(IDIM),ZN
      DATA       ZN  / '$' /
C
888   IF(ICHAR(TEXT(1)).EQ.0)  GOTO 400
      DO 100 I=2,255
      IF(ICHAR(TEXT(I)).EQ.0)  GOTO 200
100   CONTINUE
200   I = I - 1
C*    CLOSE(UNIT=ITT)
      WRITE(ITT,300)  ZN, (TEXT(J),J=1,I)
300   FORMAT(255A1)
400   READ (ITT,300,ERR=500,END=600)  (LPOLE(J),J=1,IDIM)
      IF(LPOLE(1).EQ.'!')  GOTO 999
      IOUT = TTLEN1(LPOLE,IDIM)
      IF(IOUT.LT.0)  IOUT = -1
      RETURN
500   IOUT = -4
      RETURN
600   IOUT = -2
      RETURN
999   CALL LADENI
      GOTO 888
C
      END
