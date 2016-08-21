C*************************************************************
C*************************************************************
C**********     QMDMRG ..... QUOT MIN DEG MERGE       ********
C*************************************************************
C*************************************************************
C
C     PURPOSE - THIS ROUTINE MERGES INDISTINGUISHABLE NODES IN
C               THE MINIMUM DEGREE ORDERING ALGORITHM.
C               IT ALSO COMPUTES THE NEW DEGREES OF THESE
C               NEW SUPERNODES.
C
C     INPUT PARAMETERS -
C        (XADJ, ADJNCY) - THE ADJACENCY STRUCTURE.
C        DEG0 - THE NUMBER OF NODES IN THE GIVEN SET.
C        (NHDSZE, NBRHD) - THE SET OF ELIMINATED SUPERNODES
C               ADJACENT TO SOME NODES IN THE SET.
C
C     UPDATED PARAMETERS -
C        DEG - THE DEGREE VECTOR.
C        QSIZE - SIZE OF INDISTINGUISHABLE NODES.
C        QLINK - LINKED LIST FOR INDISTINGUISHABLE NODES.
C        MARKER - THE GIVEN SET IS GIVEN BY THOSE NODES WITH
C               MARKER VALUE SET TO 1.  THOSE NODES WITH DEGREE
C               UPDATED WILL HAVE MARKER VALUE SET TO 2.
C
C     WORKING PARAMETERS -
C        RCHSET - THE REACHABLE SET.
C        OVRLP -  TEMP VECTOR TO STORE THE INTERSECTION OF TWO
C               REACHABLE SETS.
C
C*************************************************************
C
      SUBROUTINE  QMDMRG ( XADJ, ADJNCY, DEG, QSIZE, QLINK,
     1                     MARKER, DEG0, NHDSZE, NBRHD, RCHSET,
     1                     OVRLP )
C
C*************************************************************
C
         INTEGER  ADJNCY(1), DEG(1), QSIZE(1), QLINK(1),
     1            MARKER(1), RCHSET(1), NBRHD(1), OVRLP(1)
         INTEGER  XADJ(1), DEG0, DEG1, HEAD, INHD, IOV, IRCH,
     1            J, JSTRT, JSTOP, LINK, LNODE, MARK, MRGSZE,
     1            NABOR, NHDSZE, NODE, NOVRLP, RCHSZE, ROOT
C
C*************************************************************
C
C        ------------------
C        INITIALIZATION ...
C        ------------------
         IF ( NHDSZE .LE. 0 )  RETURN
	 DO 100 INHD = 1, NHDSZE
	    ROOT = NBRHD(INHD)
	    MARKER(ROOT) = 0
  100    CONTINUE
C        -------------------------------------------------  
C        LOOP THROUGH EACH ELIMINATED SUPERNODE IN THE SET
C        (NHDSZE, NBRHD).
C        -------------------------------------------------
         DO 1400 INHD = 1, NHDSZE
	    ROOT = NBRHD(INHD)
	    MARKER(ROOT) = - 1
	    RCHSZE = 0
	    NOVRLP = 0
	    DEG1   = 0
  200       JSTRT  = XADJ(ROOT)
            JSTOP  = XADJ(ROOT+1) - 1
C           ----------------------------------------------
C           DETERMINE THE REACHABLE SET AND ITS INTERSECT-
C           ION WITH THE INPUT REACHABLE SET.
C           ----------------------------------------------
            DO 600 J = JSTRT, JSTOP
	       NABOR = ADJNCY(J)
	       ROOT  = - NABOR
	       IF (NABOR)  200, 700, 300
  300          MARK = MARKER(NABOR)
               IF ( MARK ) 600, 400, 500
  400             RCHSZE = RCHSZE + 1
                  RCHSET(RCHSZE) = NABOR
		  DEG1 = DEG1 + QSIZE(NABOR)
		  MARKER(NABOR) = 1
		  GOTO 600
  500          IF ( MARK .GT. 1 )  GOTO 600
                  NOVRLP = NOVRLP + 1
		  OVRLP(NOVRLP) = NABOR
		  MARKER(NABOR) = 2
  600       CONTINUE
C           --------------------------------------------
C           FROM THE OVERLAPPED SET, DETERMINE THE NODES
C           THAT CAN BE MERGED TOGETHER.
C           --------------------------------------------
  700       HEAD = 0
            MRGSZE = 0
	    DO 1100 IOV = 1, NOVRLP
	       NODE = OVRLP(IOV)
	       JSTRT = XADJ(NODE)
	       JSTOP = XADJ(NODE+1) - 1
	       DO 800 J = JSTRT, JSTOP
	          NABOR = ADJNCY(J)
		  IF ( MARKER(NABOR) .NE. 0 ) GOTO 800
		     MARKER(NODE) = 1
		     GOTO 1100
  800          CONTINUE
C              -----------------------------------------
C              NODE BELONGS TO THE NEW MERGED SUPERNODE.
C              UPDATE THE VECTORS QLINK AND QSIZE.
C              -----------------------------------------
               MRGSZE = MRGSZE + QSIZE(NODE)
	       MARKER(NODE) = - 1
	       LNODE = NODE
  900          LINK  = QLINK(LNODE)
               IF ( LINK .LE. 0 )  GOTO 1000
	          LNODE = LINK
		  GOTO 900
 1000          QLINK(LNODE) = HEAD
               HEAD = NODE
 1100       CONTINUE
            IF ( HEAD .LE. 0 )  GOTO 1200
	       QSIZE(HEAD) = MRGSZE
	       DEG(HEAD) = DEG0 + DEG1 - 1
	       MARKER(HEAD) = 2
C           --------------------
C           RESET MARKER VALUES.
C           --------------------
 1200       ROOT = NBRHD(INHD)
            MARKER(ROOT) =  0
	    IF ( RCHSZE .LE. 0 )  GOTO 1400
	       DO 1300 IRCH = 1, RCHSZE
	          NODE = RCHSET(IRCH)
		  MARKER(NODE) = 0
 1300          CONTINUE
 1400    CONTINUE
         RETURN
      END
