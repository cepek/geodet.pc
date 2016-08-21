C+
C MTCHI1
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C NAZEV: Funkcni procedura pro vypocet kriticke hodnoty rozdeleni     *
C        chi-kvadrat                                                  *
C AUTOR: Ales Cepek, VUGTK                                            *
C VERZE: 1.00                                                         *
C DATUM: 1989-06-30                                                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	REAL FUNCTION MTCHI1 (P,N)
	INTEGER N
	REAL    P  
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C FUNKCE: Procedura pocita pro zadanou pravdepodobnost P a pocet      *
C    stupnu volnosti N kritickou hodnotu rozdeleni chi-kvadrat, kte-  *
C    rou predava volajicimu programu jako funkcni hodnotu typu REAL.  *
C PREHLED PARAMETRU						      *
C VSTUPNI PARAMETRY:						      *
C    P      - zadana pravdepodobnost P (O < P < 1)                    *
C    N      - pocet stupnu volnosti                                   *
C ARITMETIKA S JEDNODUCHOU DELKOU SLOVA.                              *
C EXTERNI PROCEDURY A FUNKCE: MTNORA, MTNORB                          *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C-
      DOUBLE PRECISION MTNORA
C      
      IF(N-2)1,2,3
    1 MTCHI1=MTNORA(DBLE(0.5*P))
      MTCHI1=MTCHI1*MTCHI1
      RETURN
    2 MTCHI1=-2.0*ALOG(P)
      RETURN
    3 F=N
      F1=1.0/F
      T=MTNORA(DBLE(P))
      F2=SQRT(F1)*T
      IF(N.GE.(2+INT(4.0*ABS(T))))GOTO 4
      MTCHI1=(((((((0.1565326E-2*F2+0.1060438E-2)*F2-0.6950356E-2)*F2-
     /0.1323293E-1)*F2+0.2277679E-1)*F2-0.8986007E-2)*F2-0.1513904E-1)
     /*F1+((((((0.253001E-2-0.1450117E-2*F2)*F2+0.5169654E-2)*F2-
     /0.1153761E-1)*F2+0.1128186E-1)*F2+0.2607083E-1)*F2-0.2237368))*
     /F1+(((((0.9780499E-4*F2-0.8426812E-3)*F2+0.312558E-2)*F2-
     /0.8553069E-2)*F2+0.1348028E-3)*F2+0.4713941)*F2+1.0000886
      GOTO 5
    4 MTCHI1=(((0.1264616E-1-0.1425296E-1*F2)*F1+(((0.1400483E-1-
     /0.588609E-2*F2)*F2-0.1091214E-1)*F2-0.2304527E-1))*F1+(((((
     /0.3135411E-2-0.2728484E-3*F2)*F2-0.9699681E-2)*F2+0.1316872E-1)*
     /F2+0.2618914E-1)*F2-0.2222222))*F1+(((((0.5406674E-4*F2
     /+0.3483789E-4)*F2-0.7274761E-3)*F2+0.3292181E-2)*F2-0.8729713E-2)
     /*F2*F2+0.4714045)*F2+1.0
    5 MTCHI1=F*MTCHI1**3
      RETURN
      END
