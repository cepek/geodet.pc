C+
C TRANK2
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro analyzu klice linearni transformace rovinnych  *
C        souradnic                                                    *
C AUTOR: Ales Cepek, VUGTK,                                           *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE TRANK2 (AA,A,B,ALFAC,BETAC,ALFA,BETA,IER)
	INTEGER IER
	REAL    AA(2,2),A,B,ALFAC,BETAC,ALFA,BETA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Linearni transformaci souradnic bodu ze soustavy S do sou-  *
C    stavy S' provazeji, v zavislosti na prvcich transformacni matice *
C    A, zmeny delek spojnic transformovanych bodu. Ukolem procedury   *
C    TRANK2 je najit na zaklade analyzy matice A extremni hodnoty po- *
C    meru delky  v soustave S' k odpovidajici delce v soustave S a    *
C    smery, ve kterych se techto hodnot dosahuje.                     *
C PREHLED PARAMETRU                                                   *
C VSTUPNI PARAMETR:                                                   *
C    AA     - pole pro ulozeni transformacni matice A                 *
C VYSTUPNI PARAMETRY:                                                 *
C    A,B    - extremni hodnoty pomeru delky v soustave S' k odpovida- *
C             jici delce v soustave S                                 *
C    ALFAC,BETAC - smery (smerniky) v soustave S', ve kterych nasta-  *
C                  va extremni dilatace                               *
C    ALFA,BETA   - smery (smerniky) v soustave S, ve kterych nastava  *
C                  extremni dilatace                                  *
C    IER    - IER = 0 regularni transformacni matice A                *
C             IER = 1 singularita - matice A transformuje vsechny bo- *
C                     dy soustavy S do jednoho bodu v soustave S'     *
C             IER =-1 singularita - matice A transformuje vsechny bo- *
C                     dy soustavy S do jedne primky v soustave S'     *
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA                               *
C EXTERNI PROCEDURA: MTELP1                                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      EQUIVALENCE(CYY,SA),(CYX,SB),(CXX,CA),(PIPUL,CB)
C
      ALFAC=0E0
      BETAC=0E0
      ALFA =0E0
      BETA =0E0
      CYY=AA(1,1)*AA(1,1)+AA(1,2)*AA(1,2)
      CYX=AA(1,1)*AA(2,1)+AA(1,2)*AA(2,2)
      CXX=AA(2,1)*AA(2,1)+AA(2,2)*AA(2,2)
      CALL MTELP1(CYY,CYX,CXX,A,B,ALFAC,IER)
      IF(IER.EQ.1) RETURN
      IF(A.NE.0E0.OR.B.NE.0E0) GO TO 100
         IER=+1
         RETURN
  100 CONTINUE
      PIPUL=ATAN2(1E0,0E0)
      DVEPI=4E0*PIPUL
      IF(PIPUL.LE.ALFAC) PIPUL=-PIPUL
      BETAC=ALFAC+PIPUL
      IF(A+B.NE.A) GO TO 200
         IER=-1
         RETURN
  200 CONTINUE
      SA=SIN(ALFAC)
      SB=SIN(BETAC)
      CA=COS(ALFAC)
      CB=COS(BETAC)
      SGN=SIGN(1E0,AA(1,1)*AA(2,2)-AA(1,2)*AA(2,1))
      ALFA=ATAN2(SGN*( AA(2,2)*SA-AA(1,2)*CA),
     /           SGN*(-AA(2,1)*SA+AA(1,1)*CA))
      BETA=ATAN2(SGN*( AA(2,2)*SB-AA(1,2)*CB),
     /           SGN*(-AA(2,1)*SB+AA(1,1)*CB))
      IF(ALFA.LT.0E0) ALFA=ALFA+DVEPI
      IF(BETA.LT.0E0) BETA=BETA+DVEPI
      IER=0
      RETURN
C
      END
