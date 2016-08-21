C*************************************************************
C*************************************************************
C********     GEN1WD ..... GENERAL ONE-WAY DISSECTION  *******
C*************************************************************
C*************************************************************
C
C     PURPOSE - GEN1WD FINDS A ONE-WAY DISSECTION PARTITIONING
C        FOR A GENERAL GRAPH.  FN1WD IS USED FOR EACH CONNECTED
C        COMPONENT.
C
C     INPUT PARAMETERS -
C        NEQNS - NUMBER OF EQUATIONS.
C        (XADJ, ADJNCY) - THE ADJACENCY STRUCTURE PAIR.
C
C     OUTPUT PARAMETERS -
C        (NBLKS, XBLK) - THE PARTITIONING FOUND.
C        PERM - THE ONE-WAY DISSECTION ORDERING.
C
C     WORKING VECTORS -
C        MASK - IS USED TO MARK VARIABLES THAT HAVE
C               BEEN NUMBERED DURING THE ORDERING PROCESS.
C        (XLS, LS) - LEVEL STRUCTURE USED BY ROOTLS.
C
C     PROGRAM SUBROUTINES -
C        FN1WD, REVRSE, ROOTLS.
C
C*************************************************************
C
      SUBROUTINE  GEN1WD ( NEQNS, XADJ, ADJNCY, MASK,
     1                     NBLKS, XBLK, PERM, XLS, LS )
C
C*************************************************************
C
         INTEGER ADJNCY(1), LS(1), MASK(1), PERM(1),
     1           XBLK(1), XLS(1)
         INTEGER XADJ(1), CCSIZE, I, J, K, LNUM,
     1           NBLKS, NEQNS, NLVL, NODE, NSEP,
     1           NUM, ROOT
C
C*************************************************************
C
         DO 100 I = 1, NEQNS
            MASK(I) = 1
  100    CONTINUE
         NBLKS = 0
         NUM   = 0
         DO 400 I = 1, NEQNS
            IF ( MASK(I) .EQ. 0 )  GO TO 400
C              -------------------------------------------
C              FIND A ONE-WAY DISSECTOR FOR EACH COMPONENT.
C              --------------------------------------------
               ROOT = I
               CALL  FN1WD ( ROOT, XADJ, ADJNCY, MASK,
     1                       NSEP, PERM(NUM+1), NLVL, XLS, LS )
               NUM = NUM + NSEP
               NBLKS = NBLKS + 1
               XBLK(NBLKS) = NEQNS - NUM + 1
               CCSIZE = XLS(NLVL+1) - 1
C              ----------------------------------------------
C              NUMBER THE REMAINING NODES IN THE COMPONENT.
C              EACH COMPONENT IN THE REMAINING SUBGRAPH FORMS
C              A NEW BLOCK IN THE PARTITIONING.
C              ----------------------------------------------
               DO 300 J = 1, CCSIZE
                  NODE = LS(J)
                  IF ( MASK(NODE) .EQ. 0 )  GO TO 300
                     CALL  ROOTLS ( NODE, XADJ, ADJNCY, MASK,
     1                              NLVL, XLS, PERM(NUM+1) )
                     LNUM = NUM + 1
                     NUM  = NUM + XLS(NLVL+1) - 1
                     NBLKS = NBLKS + 1
                     XBLK(NBLKS) = NEQNS - NUM + 1
                     DO 200 K = LNUM, NUM
                        NODE = PERM(K)
                        MASK(NODE) = 0
  200                CONTINUE
                     IF ( NUM .GT. NEQNS )  GO TO 500
  300          CONTINUE
  400    CONTINUE
C        ----------------------------------------------------
C        SINCE DISSECTORS FOUND FIRST SHOULD BE ORDERED LAST,
C        ROUTINE REVRSE IS CALLED TO ADJUST THE ORDERING
C        VECTOR, AND THE BLOCK INDEX VECTOR.
C        ----------------------------------------------------
  500    CALL REVRSE ( NEQNS, PERM )
         CALL REVRSE ( NBLKS, XBLK )
         XBLK(NBLKS+1) = NEQNS + 1
         RETURN
      END
C*************************************************************
C*************************************************************
C*******    REVRSE ..... REVERSE AN INTEGER ARRAY     ********
C*************************************************************
C*************************************************************
C
      SUBROUTINE REVRSE ( NV, V )
C
C*************************************************************
C
         INTEGER NV, V(1), NV2, I, K, ITMP
C
         NV2 = NV/2
         K = NV
         DO 100 I = 1, NV2
            ITMP = V(I)
            V(I) = V(K)
            V(K) = ITMP
            K = K - 1
  100    CONTINUE
         RETURN
      END
