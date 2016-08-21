C*************************************************************
C*************************************************************
C**********     QMDUPD ..... QUOT MIN DEG UPDATE     *********
C*************************************************************
C*************************************************************
C
C     PURPOSE - THIS ROUTINE PERFORMS DEGREE UPDATE FOR A SET
C        OF NODES IN THE MINIMUM DEGREE ALGORITHM.
C
C     INPUT PARAMETERS -
C        (XADJ, ADJNCY) - THE ADJACENCY STRUCTURE.
C        (NLIST, LIST) - THE LIST OF NODES WHOSE DEGREE HAS TO
C               BE UPDATED.
C
C     UPDATED PARAMETERS -
C        DEG - THE DEGREE VECTOR.
C        QSIZE - SIZE OF INDISTINGUISHABLE SUPERNODES.
C        QLINK - LINKED LIST FOR INDISTINGUISHABLE NODES.
C        MARKER - USED TO MARK THOSE NODES IN REACH/NBRHD SETS.
C
C     WORKING PARAMETERS -
C        RCHSET - THE REACHABLE SET.
C        NBRHD -  THE NEIGHBORHOOD SET.
C
C     PROGRAM SUBROUTINES -
C        QMDMRG.
C
C*************************************************************
C
      SUBROUTINE  QMDUPD ( XADJ, ADJNCY, NLIST, LIST, DEG,
     1                     QSIZE, QLINK, MARKER, RCHSET, NBRHD )
C     
C*************************************************************
C
         INTEGER  ADJNCY(1), LIST(1), DEG(1), MARKER(1),
     1            RCHSET(1), NBRHD(1), QSIZE(1), QLINK(1)
         INTEGER  XADJ(1), DEG0, DEG1, IL, INHD, INODE, IRCH,
     1            J, JSTRT, JSTOP, MARK, NABOR, NHDSZE, NLIST,
     1            NODE, RCHSZE, ROOT
C
C*************************************************************
C
C        ------------------------------------------------
C        FIND ALL ELIMINATED SUPERNODES THAT ARE ADJACENT
C        TO SOME NODES IN THE GIVEN LIST. PUT THEM INTO
C        (NHDSZE, NBRHD). DEG0 CONTAINS THE NUMBER OF
C        NODES IN THE LIST.
C        ------------------------------------------------
         IF ( NLIST .LE. 0 )  RETURN
	 DEG0 = 0
	 NHDSZE = 0
	 DO 200 IL = 1, NLIST
	    NODE = LIST(IL)
	    DEG0 = DEG0 + QSIZE(NODE)
	    JSTRT = XADJ(NODE)
	    JSTOP = XADJ(NODE+1) - 1
	    DO 100 J = JSTRT, JSTOP
	       NABOR = ADJNCY(J)
	       IF ( MARKER(NABOR) .NE. 0  .OR.
     1              DEG(NABOR) .GE. 0 )  GO TO 100
                  MARKER(NABOR) = - 1
		  NHDSZE = NHDSZE + 1
		  NBRHD(NHDSZE) = NABOR
  100       CONTINUE
  200    CONTINUE
C        --------------------------------------------
C        MERGE INDISTINGUISHABLE NODES IN THE LIST BY
C        CALLING THE SUBROUTINE QMDMRG.
C        --------------------------------------------
         IF ( NHDSZE .GT. 0 )
     1      CALL  QMDMRG ( XADJ, ADJNCY, DEG, QSIZE, QLINK,
     1                     MARKER, DEG0, NHDSZE, NBRHD, RCHSET,
     1                     NBRHD(NHDSZE+1) )
C        ----------------------------------------------------
C        FIND THE NEW DEGREES OF THE NODES THAT HAVE NOT BEEN
C        MERGED.
C        ----------------------------------------------------
         DO 600 IL = 1, NLIST
	    NODE = LIST(IL)
	    MARK = MARKER(NODE)
	    IF ( MARK .GT. 1  .OR.  MARK .LT. 0 )  GO TO 600
	       MARKER(NODE) = 2
	       CALL  QMDRCH ( NODE, XADJ, ADJNCY, DEG, MARKER,
     1                        RCHSZE, RCHSET, NHDSZE, NBRHD )
               DEG1 = DEG0
	       IF ( RCHSZE .LE. 0 )  GO TO 400
	          DO 300 IRCH = 1, RCHSZE
		     INODE = RCHSET(IRCH)
		     DEG1 = DEG1 + QSIZE(INODE)
		     MARKER(INODE) = 0
  300             CONTINUE
  400          DEG(NODE) = DEG1 - 1
               IF ( NHDSZE .LE. 0 )  GO TO 600
	          DO 500 INHD = 1, NHDSZE
		     INODE = NBRHD(INHD)
		     MARKER(INODE) = 0
  500             CONTINUE
  600    CONTINUE
         RETURN
      END
