C*************************************************************
C*************************************************************
C*******   FNROOT ..... FIND PSEUDO-PERIPHERAL NODE    *******
C*************************************************************
C*************************************************************
C
C     PURPOSE - FNROOT IMPLEMENTS A MODIFIED VERSION OF THE
C        SCHEME BY GIBBS, POOLE, AND STOCKMEYER TO FIND PSEUDO-
C        PERIPHERAL NODES.  IT DETERMINES SUCH A NODE FOR THE
C        SECTION SUBGRAPH SPECIFIED BY MASK AND ROOT.
C
C     INPUT PARAMETERS -
C        (XADJ, ADJNCY) - ADJACENCY STRUCTURE PAIR FOR THE GRAPH.
C        MASK - SPECIFIES A SECTION SUBGRAPH. NODES FOR WHICH
C               MASK IS ZERO ARE IGNORED BY FNROOT.
C
C     UPDATED PARAMETERS -
C        ROOT - ON INPUT, IT (ALONG WITH MASK) DEFINES THE
C               COMPONENT FOR WHICH A PSEUDO-PERIPHERAL NODE IS
C               TO BE FOUND. ON OUTPUT, IT IS THE NODE OBTAINED.
C
C     OUTPUT PARAMETERS -
C        NLVL - IS THE NUMBER OF LEVELS IN THE LEVEL STRUCTURE
C               ROOTED AT THE NODE ROOT.
C        (XLS,LS) - THE LEVEL STRUCTURE ARRAY PAIT CONTAINING
C                   THE LEVEL STRUCTURE FOUND.
C
C     PROGRAM SUBROUTINES -
C        ROOTLS.
C
C*************************************************************
C
      SUBROUTINE FNROOT ( ROOT, XADJ, ADJNCY, MASK, NLVL, XLS, LS )
C
C*************************************************************
C
         INTEGER ADJNCY(1), LS(1), MASK(1), XLS(1)
         INTEGER XADJ(1), CCSIZE, J, JSTRT, K, KSTOP, KSTRT,
     1           MINDEG, NABOR, NDEG, NLVL, NODE, NUNLVL,
     1           ROOT
C
C*************************************************************
C
C        ---------------------------------------------
C        DETERMINE THE LEVEL STRUCTURE ROOTED AT ROOT.
C        ---------------------------------------------
         CALL ROOTLS ( ROOT, XADJ, ADJNCY, MASK, NLVL, XLS, LS )
         CCSIZE = XLS(NLVL+1) - 1
         IF ( NLVL .EQ. 1 .OR. NLVL .EQ. CCSIZE ) RETURN
C        ----------------------------------------------------
C        PICK A NODE WITH MINIMUM DEGREE FROM THE LAST LEVEL.
C        ----------------------------------------------------
  100    JSTRT = XLS(NLVL)
         MINDEG = CCSIZE
         ROOT = LS(JSTRT)
         IF ( CCSIZE .EQ. JSTRT )  GO TO 400
            DO 300 J = JSTRT, CCSIZE
               NODE = LS(J)
               NDEG = 0
               KSTRT = XADJ(NODE)
               KSTOP = XADJ(NODE+1) - 1
               DO 200 K = KSTRT, KSTOP
                  NABOR = ADJNCY(K)
                  IF ( MASK(NABOR) .GT. 0 )  NDEG = NDEG + 1
  200          CONTINUE
               IF ( NDEG .GE. MINDEG ) GO TO 300
                  ROOT = NODE
                  MINDEG = NDEG
  300       CONTINUE
C        ----------------------------------------
C        AND GENERATE ITS ROOTED LEVEL STRUCTURE.
C        ----------------------------------------   
  400    CALL  ROOTLS ( ROOT, XADJ, ADJNCY, MASK, NUNLVL, XLS, LS )
         IF (NUNLVL .LE. NLVL)  RETURN
            NLVL = NUNLVL
            IF ( NLVL .LT. CCSIZE )  GO TO 100
            RETURN
      END
