C POMOCNA PROCEDURA PRO MERENI A TISK CASU VE VTERINACH
C=======================================================================
C
      SUBROUTINE GTTIME
C
      REAL  S, CAS(20)
      INTEGER  H, M, V, T, POC, I, MINH
      SAVE     CAS, POC, MINH
      DATA POC /0/ CAS /20*0.0/ MINH /0/
C
C g77
C
      CALL CPU_Time(s)
C
C Microsoft Fortran
C
C     CALL GETTIM(H,M,V,T)
C     IF(MINH.GT.H)  H = H + 24
C     MINH = H
C     S =  3600.0*H + 60.0*M + V + FLOAT(T)/100.0
C
C Lahey Fortran
C
C     CALL TIMER(V)
C     S = V/100.0
C
      POC = POC + 1
      CAS(POC) = S
      RETURN
C
      ENTRY GTTIMW
C
      WRITE(*,'('' ###'',20F10.2)')  (CAS(I)-CAS(I-1),I=2,POC)
C
      END
