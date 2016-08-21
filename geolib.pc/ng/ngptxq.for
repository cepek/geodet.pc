C+
C NGPTXQ
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Pomocna procedura pro prevod seznamu merenych prvku na sez-  *
C        nam urcujicich prvku                                         *
C AUTOR: Jaroslava Horejcova, VUGTK, nyni Geodezie, s. p., Praha,     *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE NGPTXQ (NSMP,NSUP,IER)
	INTEGER     IER,NSMP,NSUP
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: NGPTXQ slouzi procedure NGPTXA k prevodu seznamu merenych   *
C    prvku na seznam urcujicich prvku (viz textovou cast dokumentace  *
C    procedury NGPTXA, odstavec 4.1.).                                *
C PREHLED PARAMETRU - viz textovou cast dokumentace procedury NGPTXA, *
C                     odstavec 4.1.3.                                 *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C EXTERNI PROCEDURA: PPKMB1                                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	DOUBLE PRECISION SMP2(20,3),SUP2(36),U,PI
	INTEGER*1        SMP1(20),SUP1(36,3)
	INTEGER          KOMB(2)	
C OBLASTI COMMON:
	COMMON/NGPTXW/SMP1,SMP2,SUP1,SUP2
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	PI=DATAN2(0D0,-1D0)
	IF(NSMP.LE.20.AND.NSMP.GE.2) GO TO 1
	IER=2
	RETURN
1	IER=0
	I=0
	NSUP=1
2	I=I+1
	IF(I.LE.NSMP) GO TO 4
3	NSUP=NSUP-1
	RETURN
4	IF(SMP1(I)-2.GE.0) GO TO 5 
	SUP1(NSUP,1)=SMP1(I)
	SUP1(NSUP,2)=I
	SUP1(NSUP,3)=0
	SUP2(NSUP)=SMP2(I,1)
	NSUP=NSUP+1
	IF(NSUP.LE.36) GO TO 2 
	GO TO 3 
5	M=0
	DO 10 K=I+1,NSMP
	IF(SMP1(K).NE.2) GO TO 6 
10	M=M+1
6	IF(M.LE.0) GO TO 2 
	KOMB(1)=0
7	CALL PPKMB1(M+1,2,KOMB)
	IF(KOMB(1).NE.0) GO TO 8 
	I=I+M
	GO TO 2 
8	I1=I+KOMB(1)-1
	I2=I+KOMB(2)-1
	U=SMP2(I2,1)-SMP2(I1,1)
	IF(DABS(DSIN(U)).LT.15D-2) GO TO 7 
	IF(U.LT.0D0) U=U+2D0*PI
	SUP1(NSUP,1)=3
	SUP1(NSUP,2)=I1
	SUP1(NSUP,3)=I2
	SUP2(NSUP)=U
	NSUP=NSUP+1
	IF(NSUP.LE.36) GO TO 7
	GO TO 3 
	END
