C AKTUALIZACE INFORMACI O BODECH A ORIENTACNICH POSUNECH
C=======================================================================
C         1. VERZE      /RIJEN 1984/            VUGTK
C       * 5. VERZE      /ZARI 1989/             VUGTK      ALES CEPEK
C
      SUBROUTINE GDTA3S(TISK,IOP)
C
$INCLUDE: 'GDTSOW.COM'
      LOGICAL  TISK,ZMENA,EOF,AKTB,EXIT
      INTEGER  IOP,IBOD,I,IFORM,IOUT,JOUT,IX,IPOM
      PARAMETER  (IFORM=14)
      INTEGER*1  BOD(13),BPSY(IFORM),BPSX(IFORM),BPOP(IFORM),
     / BPCB(4),BPCS(4),TYP
      CHARACTER*(IFORM) CBPSY, CBPSX, CBPOP, CBPCB*4, CBPCS*4
      EQUIVALENCE (CBPSY,BPSY), (CBPSX,BPSX), (CBPOP,BPOP),
     / (CBPCB,BPCB), (CBPCS,BPCS)
      DOUBLE PRECISION  DPOM
      EXIT(IX) = IX.EQ.0..OR.IX.EQ.-2
3     FORMAT(
     /' 3 ... AKTUALIZACE INFORMACI O BODECH A ORIENTACNICH POSUNECH')
C*11  FORMAT(F<IFORM>.5)
C*12  FORMAT(F<IFORM>.6)
11    FORMAT(F14.5)
12    FORMAT(F14.6)
13    FORMAT(I4)
C*20  FORMAT(//
C*   /' CISLO BODU: ',13A1/
C*   /' = INDEX ',<17+IFORM>('=')/
C*   /'     1   TYP BODU        :',X,A1/
C*   /'     2   SOURADNICE Y [M]:',<IFORM>A1/
C*   /'     3   SOURADNICE X [M]:',<IFORM>A1/
C*   /'     4   OR. POSUN    [G]:',<IFORM>A1/
C*   /'     5   CHAR. BODU      :',4A1/
C*   /'     6   CHAR. SOURADNIC :',4A1/
C*   /)
20    FORMAT(//
     /' CISLO BODU: ',13A1/
     /' = INDEX ',31('=')/
     /'     1   TYP BODU        :',1X,A1/
     /'     2   SOURADNICE Y [M]:',14A1/
     /'     3   SOURADNICE X [M]:',14A1/
     /'     4   OR. POSUN    [G]:',14A1/
     /'     5   CHAR. BODU      :',4A1/
     /'     6   CHAR. SOURADNIC :',4A1/
     /)
30    FORMAT(' AKTUALIZOVANY PRVEK: BOD ',13A1)
C
      IF(SS.LT.1)   RETURN
0103  IF(.NOT.TISK)  GOTO 0199
         WRITE(ITT,3)
         RETURN
0199  CONTINUE
      IF(IOP.NE.3)  RETURN
      CALL TTZAHL(ITT,
     /'AKTUALIZACE INFORMACI O BODECH A ORIENTACNICH POSUNECH:')
      ZMENA = .FALSE.
1102  CONTINUE
         CALL GDTSCP('CISLO BODU: ',BOD,IBOD,EOF)
         IF(EOF)  GOTO 1199
0202     CONTINUE
         TYP = TYPB(IBOD)
C*       ENCODE(IFORM,11,BPSY)  SBY(IBOD)
         WRITE(CBPSY,11) SBY(IBOD)
         CALL GDSMEZ(BPSY,IFORM,I)
C*       ENCODE(IFORM,11,BPSX)  SBX(IBOD)
         WRITE(CBPSX,11) SBX(IBOD)
         CALL GDSMEZ(BPSX,IFORM,I)
C*       ENCODE(IFORM,12,BPOP)  SBZ(IBOD)*RO
         WRITE(CBPOP,12) SBZ(IBOD)*RO
         CALL GDSMEZ(BPOP,IFORM,I)
C*       IF(SBZ(IBOD).LT.0)  CALL PPINI1(' ',IFORM,BPOP)
         IF(SBZ(IBOD).LT.0) THEN
	    DO 9981 II81=1,IFORM
9981           BPOP(II81) = ' '
         END IF	
C*       ENCODE(4,13,BPCB)  CHAB(IBOD)
         WRITE(CBPCB,13) CHAB(IBOD)
         CALL GDSMEZ(BPCB,4,I)
C*       IF(CHAB(IBOD).LE.0)  CALL PPINI1(' ',4,BPCB)
         IF(CHAB(IBOD).LE.0) THEN
	    DO 9982 II82=1,4
9982           BPCB(II82) = ' '
         END IF	
C*       ENCODE(4,13,BPCS)  CHAS(IBOD)
         WRITE(CBPCS,13) CHAS(IBOD)
         CALL GDSMEZ(BPCS,4,I)
C*       IF(CHAS(IBOD).LE.0)  CALL PPINI1(' ',4,BPCS)
         IF(CHAS(IBOD).LE.0) THEN
	    DO 9983 II83=1,4
9983           BPCS(II83) = ' '
         END IF
         WRITE(ITT,20)  BOD,TYP,BPSY,BPSX,BPOP,BPCB,BPCS
0252     CONTINUE
         CALL TTASK(ITT,'INDEX (1-6): ',IOUT)
         AKTB = .FALSE.
         IF(IOUT.EQ.-2) GOTO 0299
         IF(IOUT.LE.0)  GOTO 0202
0303     IF(IOUT.NE.1)  GOTO 0304
            CALL GDTPPK('TYP BODU (P/U): ',JOUT)
            IF(JOUT.EQ.1)  TYPB(IBOD) = 'P'
            IF(JOUT.EQ.2)  TYPB(IBOD) = 'U'
            IF(TYP.NE.TYPB(IBOD))  AKTB = .TRUE.
            GOTO 0399
0304     IF(IOUT.NE.2)  GOTO 0305
0354        CALL TTASKD(ITT,'SOURADNICE Y [M]: ',JOUT,DPOM,1)
            IF(EXIT(JOUT))  GOTO 0399
            IF(JOUT.LT.0)  GOTO 0354
            SBY(IBOD) = DPOM
            SBZ(IBOD) = -1
            AKTB = .TRUE.
            GOTO 0399
0305     IF(IOUT.NE.3)  GOTO 0306
0355        CALL TTASKD(ITT,'SOURADNICE X [M]: ',JOUT,DPOM,1)
            IF(EXIT(JOUT))  GOTO 0399
            IF(JOUT.LE.0)  GOTO 0355
            SBX(IBOD) = DPOM
            SBZ(IBOD) = -1
            AKTB = .TRUE.
            GOTO 0399
0306     IF(IOUT.NE.4)  GOTO 0307
0356        CALL TTASKD(ITT,'OR. POSUN [G]: ',JOUT,DPOM,1)
            IF(EXIT(JOUT))  GOTO 0399
            IF(JOUT.EQ.-1)  CALL TTPUT1(ITT,1,
     /      'PRO ZRUSENI ORIENTACNIHO POSUNU VLOZ HODNOTU -1:')
            IF(JOUT.LT.0)  GOTO 0356
            IF(DPOM.GE.400)  GOTO 0356
            IF(DPOM.NE.-1)  DPOM = DPOM/RO
            SBZ(IBOD) = DPOM
            AKTB = .TRUE.
            GOTO 0399
0307     IF(IOUT.NE.5)  GOTO 0308
0357        CALL TTASKN(ITT,'CHARAKTERISTIKA BODU: ',JOUT,IPOM,1)
            IF(EXIT(JOUT))  GOTO 0399
            IF(JOUT.LT.0)  GOTO 0357
            IF(IPOM.LT.0.OR.IPOM.GT.127)  GOTO 0357
            CHAB(IBOD) = IPOM
            GOTO 0399
0308     IF(IOUT.NE.6)  GOTO 0399
0358        CALL TTASKN(ITT,'CHARAKTERISTIKA SOURADNIC: ',JOUT,IPOM,1)
            IF(EXIT(JOUT))  GOTO 0399
            IF(JOUT.LT.0)  GOTO 0358
            IF(IPOM.LT.0.OR.IPOM.GT.127)  GOTO 0358
            CHAS(IBOD) = IPOM
0399     CONTINUE
0403     IF(.NOT.AKTB)  GOTO 0499
            ZMENA = .TRUE.
            CALL GDSSTR(1,0)
            WRITE(GSGDPT,30)  BOD
0499     CONTINUE
         GOTO 0252
0299     CONTINUE
         CALL TTROL1(ITT,2)
         GOTO 1102
1199  CONTINUE
      IF(ZMENA)  SS = MIN(SS,2)
      RETURN
C
      END
