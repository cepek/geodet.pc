C+
C NGPTXA
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Procedura pro vypocet souradnic bodu protinanim              *
C AUTOR: Jaroslava Horejcova, VUGTK, nyni Geodezie, s. p., Praha,     *
C        Frantisek Charamza, VUGTK                                    *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SUBROUTINE NGPTXA (NSMP,NSUP,KOMB,Y,X,IER)
	DOUBLE PRECISION Y(2),X(2)
	INTEGER          IER,NSMP,NSUP,KOMB(2)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: NGPTXA je univerzalni procedura, jejiz jadro tvori vypocet  *
C    rovinnych souradnic bodu ruznymi druhy protinani (bez vyrovna-   *
C    ni). Urcujicimi prvky pritom mohou byt libovolne dva z nasledu-  *
C    jicich objektu:                                                  *
C    - orientovana poloprimka s danym pocatecnim bodem prochazejici   *
C      hledanym bodem (je dan tzv. vnejsi smernik na bode se znamymi  *
C      souradnicemi),                                                 *
C    - delka usecky spojujici urcovany bod s danym bodem,             *
C    - uhel s vrcholem v hledanem bode a rameny prochazejicimi dvema  *
C      danymi body (tzv. vnitrni uhel).                               *
C    Logicka struktura procedury, uzite algoritmy a zpusob prace      *
C    s procedurou jsou popsany v textove casti dokumentace.           *
C PREHLED PARAMETRU - viz textovou cast dokumentace                   *
C ARITMETIKA S DVOJNASOBNOU DELKOU SLOVA                              *
C EXTERNI PROCEDURY: NGKRUA, NGPPKA, NGPTDB, NGPTVA, NGSMDA, PPKMB1   *
C POMOCNE PROCEDURY: NGPTXQ, NGPTXR, NGPTXP                           *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
	DOUBLE PRECISION SMP2(20,3),SUP2(36),R(2),X1(9),Y1(9)
	INTEGER*1        SMP1(20),SUP1(36,3)
	INTEGER          TYPP,L(2)
C OBLASTI COMMON:
	COMMON/NGPTXW/SMP1,SMP2,SUP1,SUP2
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	IF(KOMB(1).NE.0) GO TO 1
	CALL NGPTXQ(NSMP,NSUP,IER)  
C VYTVORENI SEZNAMU URCUJICICH PRVKU
	IF (IER.NE.0) RETURN        
C NEPRIPUSTNY POCET PRVKU V SMP
1	CALL PPKMB1 (NSUP,2,KOMB)   
C TVORBA KOMBINACE URCUJICICH PRVKU
	IF (KOMB(1).NE.0) GO TO 2
	IER=1                       
C BYLY VYCERPANY VSECHNY KOMBINACE
	RETURN
2	TYPP=SUP1(KOMB(1),1)+SUP1(KOMB(2),1)+1   
C URCENI TYPU PROTINANI
	GO TO (3,20,3,5,5,1,6),TYPP
C TYP "SMERNIK-SMERNIK" A "DELKA-DELKA"
3	DO 4 I=1,2
	K=SUP1(KOMB(I),2)
	Y1(I)=SMP2(K,2)
	X1(I)=SMP2(K,3)
4	R(I)=SUP2(KOMB(I))
	IF(TYPP.EQ.3) GO TO 30      
C TYP "DELKA-DELKA"
C TYP "SMERNIK-SMERNIK"
10	CALL NGPTVA (Y1,X1,R,IER)   
C PROTINANI VPRED
	IF(IER.NE.0) GO TO 1
11	IER=0                       
C NALEZENO JEDNOZNACNE RESENI
	Y(1)=Y1(3)
	X(1)=X1(3)
	Y(2)=0
	X(2)=0
	RETURN
C TYP "DELKA-DELKA"
30	CALL NGPTDB (Y1,X1,R,IER)   
C PRUSECIKY DVOU KRUZNIC
	IF(IER.EQ.0) GO TO 100
	GO TO 1
C TYP "SMERNIK-UHEL" A "DELKA-UHEL"
5	IF(SUP1(KOMB(1),1).EQ.3) GO TO 6
	L(1)=KOMB(2)
	L(2)=KOMB(1)
	GO TO 7
C PRIPOJENI TYPU "UHEL-UHEL"
6	L(1)=KOMB(1)
	L(2)=KOMB(2)
7	DO 8 I=1,2
	K=SUP1(L(1),I+1)
	Y1(I)=SMP2(K,2)
8	X1(I)=SMP2(K,3)
	CALL NGKRUA(Y1,X1,SUP2(L(1)),R(1),IER)   
C PARAMETRY KRUZNICE
	IF(IER.NE.0) GO TO 1
	IF(TYPP.EQ.7) GO TO 70      
C TYP "UHEL-UHEL"
	K=SUP1(L(2),2)
	Y1(4)=SMP2(K,2)
	X1(4)=SMP2(K,3)
	IF(TYPP.EQ.5) GO TO 50            
C TYP "DELKA-UHEL"
C TYP "SMERNIK-UHEL"
C	PRUSECIK POLOPRIMKY A KRUZNICE
40	CALL NGPPKA (Y1(3),X1(3),SUP2(L(2)),R(1),IER)
	IF(IER.NE.0.AND.IER.NE.1) GO TO 1
C	OVERENI 1. PRUSECIKU
	CALL NGPTXP (Y1,X1,Y1(5),X1(5),SUP2(L(1)),X(1),Y,IER1)
	I=IER+IER1+1
	GO TO (41,103,41,1,41,1),I
C	OVERENI 2. PRUSECIKU
41	CALL NGPTXP (Y1,X1,Y1(6),X1(6),SUP2(L(1)),X(1),Y,IER1)
	IF(I.EQ.3.OR.I.EQ.5) GO TO 45
	IF(IER1.EQ.0) GO TO 101
	GO TO 103
45	IF(IER1.EQ.0) GO TO 104
	GO TO 1
C TYP "DELKA-UHEL"
50	R(2)=SUP2(L(2))
	CALL NGPTDB (Y1(3),X1(3),R,IER)   
C PRUSECIKY DVOU KRUZNIC
	IF(IER.NE.0) GO TO 1
C	OVERENI 1. PRUSECIKU
	CALL NGPTXP (Y1,X1,Y1(5),X1(5),SUP2(L(1)),X(1),Y,IER1)
	I=IER1
C	OVERENI 2. PRUSECIKU
	CALL NGPTXP (Y1,X1,Y1(6),X1(6),SUP2(L(1)),X(1),Y,IER1)
	IF(I.EQ.0) GO TO 51
	IF(IER1.NE.0) GO TO 1
	GO TO 104
51	IF(IER1.EQ.0) GO TO 101
	GO TO 103
C TYP "UHEL-UHEL"
70	DO 71 I=7,8
	K=SUP1(KOMB(2),I-5)
	Y1(I)=SMP2(K,2)
71	X1(I)=SMP2(K,3)
C	VYPOCET PARAMETRU KRUZNICE
	CALL NGKRUA (Y1(7),X1(7),SUP2(KOMB(2)),R(2),IER)
	IF (IER.NE.0) GO TO 1
	Y1(4)=Y1(9)
	X1(4)=X1(9)
	CALL NGPTDB (Y1(3),X1(3),R,IER)   
C PRUSECIKY DVOU KRUZNIC
	IF(IER.NE.0) GO TO 1
C	OVERENI 1. PRUSECIKU VZHLEDEM K 1. KRUZNICI
	CALL NGPTXP (Y1,X1,Y1(5),X1(5),SUP2(KOMB(1)),X(1),Y,IER1)
	IF(IER1.NE.0) GO TO 72
C	OVERENI 1. PRUSECIKU VZHLEDEM K 2. KRUZNICI
	CALL NGPTXP (Y1(7),X1(7),Y1(5),X1(5),SUP2(KOMB(2)),X(1),Y,IER1)
	IF(IER1.NE.0) GO TO 72
	I=2
	GO TO 73
72	I=1
C	OVERENI 2. PRUSECIKU VZHLEDEM K 1. KRUZNICI
73	CALL NGPTXP (Y1,X1,Y1(6),X1(6),SUP2(KOMB(1)),X(1),Y,IER1)
	IF(IER1.EQ.0) GO TO 74
	IF(I.EQ.1) GO TO 1
	GO TO 103
C	OVERENI 2. PRUSECIKU VZHLEDEM K 2. KRUZNICI
74	CALL NGPTXP (Y1(7),X1(7),Y1(6),X1(6),SUP2(KOMB(2)),X(1),Y,IER1)
	I=I+IER1
	GO TO (104,101,1,103,1,103),I
C TYP "SMERNIK-DELKA"
20	IF(SUP1(KOMB(1),1).EQ.1) GO TO 21
	L(1)=KOMB(2)
	L(2)=KOMB(1)
	GO TO 22
21	L(2)=KOMB(2)
	L(1)=KOMB(1)
22	DO 23 I=1,2
	K=SUP1(L(I),2)
	Y1(I)=SMP2(K,2)
23	X1(I)=SMP2(K,3)
C	PRUSECIK POLOPRIMKY A KRUZNICE
	CALL NGPPKA (Y1,X1,SUP2(L(2)),SUP2(L(1)),IER)
	IF(IER.EQ.1) GO TO 11
	IF(IER.EQ.0) GO TO 100
	GO TO 1
100	Y1(5)=Y1(3)           
C PRIPRAVA PARAMETRU PRO NGPTXR
	X1(5)=X1(3)
	Y1(6)=Y1(4)
	X1(6)=X1(4)
101	CALL NGPTXR (NSUP,Y1(5),X1(5),K) 
C VYBER JEDNOHO ZE DVOU RESENI
	IF(K-1) 102,103,104
102	IER=-1                
C DVOJZNACNOST NEODSTRANENA
	Y(1)=Y1(5)
	X(1)=X1(5)
	Y(2)=Y1(6)
	X(2)=X1(6)
	RETURN
103	IER=0                 
C JEDNOZNACNE RESENI
	Y(1)=Y1(5)
	X(1)=X1(5)
	Y(2)=0
	X(2)=0
	RETURN
104	Y(1)=Y1(6)            
C JEDNOZNACNE RESENI
	X(1)=X1(6)
	Y(2)=0
	X(2)=0
	RETURN
	END
