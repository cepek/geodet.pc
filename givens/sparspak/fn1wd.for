C*************************************************************
C*************************************************************
C*******     FN1WD ..... FIND ONE-WAY DISSECTORS       *******
C*************************************************************
C*************************************************************
C
C     PURPOSE - THIS SUBROUTINE FINDS ONE-WAY DISSECTORS OF
C        A CONNECTED COMPONENT SPECIFIED BY MASK AND ROOT.
C
C     INPUT PARAMETERS -
C        ROOT - A NODE THAT DEFINES (ALONG WITH MASK) THE
C               COMPONENT TO BE PROCESSED.
C        (XADJ, ADJNCY) - THE ADJACENCY STRUCTURE.
C
C     OUTPUT PARAMETERS -
C        NSEP - NUMBER OF NODES IN THE ONE-WAY DISSECTORS.
C        SEP - VECTOR CONTAINING THE DISSECTOR NODES.
C
C     UPDATED PARAMETER -
C        MASK - NODES IN THE DISSECTOR HAVE THEIR MASK VALUES
C               SET TO ZERO.
C
C     WORKING PARAMETERS -
C        (XLS, LS) - LEVEL STRUCTURE USED BY THE ROUTINE FNROOT.
C
C     PROGRAM SUBROUTINE -
C        FNROOT.
C
C*************************************************************
C
      SUBROUTINE  FN1WD ( ROOT, XADJ, ADJNCY, MASK,
     1                    NSEP, SEP, NLVL, XLS, LS )
C
C*************************************************************
C
         INTEGER ADJNCY(1), LS(1), MASK(1), SEP(1), XLS(1)
         INTEGER XADJ(1), I, J, K, KSTOP, KSTRT, LP1BEG, LP1END,
     1           LVL, LVLBEG, LVLEND, NBR, NLVL, NODE,
     1           NSEP, ROOT
         REAL DELTP1, FNLVL, WIDTH, SIGMA
C
C*************************************************************
C
         CALL FNROOT ( ROOT, XADJ, ADJNCY, MASK,
     1                 NLVL, XLS, LS )
         FNLVL = FLOAT(NLVL)
         NSEP  = XLS(NLVL + 1) - 1
         WIDTH = FLOAT(NSEP) / FNLVL
C ******   ******  ******  ******  ******  ******  ******  ****** 
C HODNOTA  DELTA  JE VYPOCTENA PRO I. A II. PAMETOVY MODEL GSO
C
C        DELTP1 = 1.0 + SQRT((3.0*WIDTH+13.0)/2.0)
         SIGMA  = SQRT(FNLVL+1) - 1
	 DELTP1 = 1.0 + (FNLVL - SIGMA)/(SIGMA + 1)
C
C ******   ******  ******  ******  ******  ******  ******  ****** 
      GOTO 300
C***         IF (NSEP .GE. 50 .AND. DELTP1 .LE. 0.5*FNLVL) GO TO 300
C        ----------------------------------------------------
C        THE COMPONENT IS TOO SMALL, OR THE LEVEL STRUCTURE
C        IS VERY LONG AND NARROW. RETURN THE WHOLE COMPONENT.
C        ---------------------------------------------------- 
            DO 200 I = 1, NSEP
               NODE = LS(I)
               SEP(I) = NODE
               MASK(NODE) = 0
  200       CONTINUE
            RETURN
C        -----------------------------
C        FIND THE PARALLEL DISSECTORS.
C        -----------------------------
  300    NSEP = 0
         I = 0
  400    I = I + 1
            LVL = IFIX (FLOAT(I)*DELTP1 + 0.5)
            IF ( LVL .GE. NLVL )  RETURN
            LVLBEG = XLS(LVL)
            LP1BEG = XLS(LVL + 1)
            LVLEND = LP1BEG - 1
            LP1END = XLS(LVL + 2) - 1
            DO 500 J = LP1BEG, LP1END
               NODE = LS(J)
               XADJ(NODE) =  - XADJ(NODE)
  500       CONTINUE
C           -------------------------------------------------
C           NODES IN LEVEL LVL ARE CHOSEN TO FORM DISSECTOR.
C           INCLUDE ONLY THOSE WITH NEIGHBORS IN LVL+1 LEVEL.
C           XADJ IS USED TEMPORARILY TO MARK NODES IN LVL+1.
C           -------------------------------------------------
            DO 700 J = LVLBEG, LVLEND
               NODE = LS(J)
               KSTRT = XADJ(NODE)
               KSTOP = IABS(XADJ(NODE+1)) - 1
               DO 600 K = KSTRT, KSTOP
                  NBR = ADJNCY(K)
                  IF ( XADJ(NBR) .GT. 0 )  GO TO 600
                     NSEP = NSEP + 1
                     SEP(NSEP) = NODE
                     MASK(NODE) = 0
                     GO TO 700
  600          CONTINUE
  700       CONTINUE
            DO 800 J = LP1BEG, LP1END
               NODE = LS(J)
               XADJ(NODE) = - XADJ(NODE)
  800       CONTINUE
         GO TO 400
      END
