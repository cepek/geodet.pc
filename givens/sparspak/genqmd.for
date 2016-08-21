C*************************************************************
C*************************************************************
C********    GENQMD ..... QUOT MIN DEGREE ORDERING    ********
C*************************************************************
C*************************************************************
C
C     PURPOSE - THIS ROUTINE IMPLEMENTS THE MINIMUM DEGREE
C        ALGORITHM.  IT MAKES USE OF THE IMPLICIT REPRESENT-
C        ATION OF THE ELIMINATION GRAPHS BY QUOTIENT GRAPHS,
C        AND THE NOTION OF INDISTINGUISHABLE NODES.
C        CAUTION - THE ADJACENCY VECTOR ADJNCY WILL BE
C        DESTROYED.
C
C     INPUT PARAMETERS -
C        NEQNS - NUMBER OF EQUATIONS.
C        (XADJ, ADJNCY) - THE ADJACENCY STRUCTURE.
C
C     OUTPUT PARAMETERS -
C        PERM - THE MINIMUM DEGREE ORDERING.
C        INVP - THE INVERSE OF PERM.
C
C     WORKING PARAMETERS -
C        DEG - THE DEGREE VECTOR. DEG(I) IS NEGATIVE MEANS
C               NODE I HAS BEEN NUMBERED.
C        MARKER - A MARKER VECTOR, WHERE MARKER(I) IS
C               NEGATIVE MEANS NODE I HAS BEEN MERGED WITH
C               ANOTHER NODE AND THUS CAN BE IGNORED.
C        RCHSET - VECTOR USED FOR THE REACHABLE SET.
C        NBRHD - VECTOR USED FOR THE NEIGHBORHOOD SET.
C        QSIZE - VECTOR USED TO STORE THE SIZE OF
C                INDISTINGUISHABLE SUPERNODES.
C        QLINK - VECTOR TO STORE INDISTINGUISHABLE NODES,
C               I, QLINK(I), QLINK(QLINK(I)) ... ARE THE
C               MEMBERS OF THE SUPERNODE REPRESENTED BY I.
C
C     PROGRAM SUBROUTINES -
C        QMDRCH, QMDQT, QMDUPD.
C
C****************************************************************
C
C
      SUBROUTINE  GENQMD ( NEQNS, XADJ, ADJNCY, PERM, INVP, DEG,
     1                     MARKER, RCHSET, NBRHD, QSIZE, QLINK,
     1                     NOFSUB )
C
C****************************************************************
C
         INTEGER ADJNCY(1), PERM(1), INVP(1), DEG(1), MARKER(1),
     1           RCHSET(1), NBRHD(1), QSIZE(1), QLINK(1)
         INTEGER XADJ(1), INODE, IP, IRCH, J, MINDEG, NDEG,
     1           NEQNS, NHDSZE, NODE, NOFSUB, NP, NUM, NUMP1,
     1           NXNODE, RCHSZE, SEARCH, THRESH
C
C****************************************************************
C
C        -----------------------------------------------------
C        INITIALIZE DEGREE VECTOR AND OTHER WORKING VARIABLES.
C        -----------------------------------------------------
         MINDEG = NEQNS
	 NOFSUB = 0
	 DO 100 NODE = 1, NEQNS
	    PERM(NODE) = NODE
	    INVP(NODE) = NODE
	    MARKER(NODE) = 0
	    QSIZE(NODE)  = 1
	    QLINK(NODE)  = 0
	    NDEG = XADJ(NODE+1) - XADJ(NODE)
	    DEG(NODE) = NDEG
	    IF ( NDEG .LT. MINDEG )  MINDEG = NDEG
  100    CONTINUE
         NUM = 0
C        -----------------------------------------------------
C        PERFORM THRESHOLD SEARCH TO GET A NODE OF MIN DEGREE.
C        VARIABLE SEARCH POINTS TO WHERE SEARCH SHOULD START.
C        -----------------------------------------------------
  200    SEARCH = 1
            THRESH = MINDEG
	    MINDEG = NEQNS
  300       NUMP1 = NUM + 1
               IF( NUMP1 .GT. SEARCH )   SEARCH = NUMP1
	       DO 400 J = SEARCH, NEQNS
	          NODE = PERM(J)
		  IF ( MARKER(NODE) .LT. 0 )  GOTO 400
		     NDEG = DEG(NODE)
		     IF ( NDEG .LE. THRESH )  GO TO 500
		     IF ( NDEG .LT. MINDEG )  MINDEG =  NDEG
  400          CONTINUE
            GO TO 200
C           ---------------------------------------------------
C           NODE HAS MINIMUM DEGREE. FIND ITS REACHABLE SETS BY
C           CALLING QMERCH.
C           ---------------------------------------------------
  500       SEARCH = J
            NOFSUB = NOFSUB + DEG(NODE)
	    MARKER(NODE) = 1
	    CALL QMDRCH (NODE, XADJ, ADJNCY, DEG, MARKER,
     1                   RCHSZE, RCHSET, NHDSZE, NBRHD )
C           ------------------------------------------------
C           ELIMINATE ALL NODES INDISTINGUISHABLE FROM NODE.
C           THEY ARE GIVEN BY NODE, QLINK(NODE), ....
C           ------------------------------------------------
            NXNODE = NODE
  600       NUM = NUM + 1
               NP  = INVP(NXNODE)
	       IP  = PERM(NUM)
	       PERM(NP) = IP
	       INVP(IP) = NP
	       PERM(NUM) = NXNODE
	       INVP(NXNODE) = NUM
	       DEG(NXNODE) = -1
	       NXNODE = QLINK(NXNODE)
	    IF (NXNODE .GT. 0) GOTO 600
C
            IF ( RCHSZE .LE. 0 )  GO TO 800
C              ------------------------------------------------
C              UPDATE THE DEGREES OF THE NODES IN THE REACHABLE
C              SET AND IDENTIFY INDISTINGUISHABLE NODES.
C              ------------------------------------------------
               CALL  QMDUPD ( XADJ, ADJNCY, RCHSZE, RCHSET, DEG,
     1                        QSIZE, QLINK, MARKER,
     1                        RCHSET(RCHSZE+1), NBRHD(NHDSZE+1) )
C              -------------------------------------------
C              RESET MARKER VALUE OF NODES IN REACH SET.
C              UPDATE THRESHOLD VALUE FOR CYCLIC SEARCH.
C              ALSO CALL QMDQT TO FORM NEW QUOTIENT GRAPH.
C              -------------------------------------------
               MARKER(NODE) = 0
	       DO 700 IRCH = 1, RCHSZE
	          INODE = RCHSET(IRCH)
		  IF ( MARKER(INODE) .LT. 0 )  GOTO 700
		     MARKER(INODE) = 0
		     NDEG = DEG(INODE)
		     IF ( NDEG .LT. MINDEG )  MINDEG = NDEG
		     IF ( NDEG .GT. THRESH )  GOTO 700
		        MINDEG = THRESH
			THRESH = NDEG
			SEARCH = INVP(INODE)
  700          CONTINUE
               IF ( NHDSZE .GT. 0 )  CALL  QMDQT ( NODE, XADJ,
     1            ADJNCY, MARKER, RCHSZE, RCHSET, NBRHD )
  800    IF ( NUM .LT. NEQNS )  GO TO 300
         RETURN
      END
