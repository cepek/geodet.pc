C*************************************************************
C*************************************************************
C*******     QMDQT  ..... QUOT MIN DEG QUOT TRANSFORM  *******
C*************************************************************
C*************************************************************
C
C     PURPOSE - THIS SUBROUTINE PERFORMS THE QUOTIENT GRAPH
C        TRANSFORMATION AFTER A NODE HAS BEEN ELIMINATED.
C
C     INPUT PARAMETERS -
C        ROOT - THE NODE JUST ELIMINATED. IT BECOMES THE
C               REPRESENTATIVE OF THE NEW SUPERNODE.
C        (XADJ, ADJNCY) - THE ADACENCY STRUCTURE.
C        (RCHSZE, RCHSET) - THE REACHABLE SET OF ROOT IN THE
C               OLD QUOTIENT GRAPH.
C        NBRHD - THE NEIGHBORHOOD SET WHICH WILL BE MERGED
C               WITH ROOT TO FORM THE NEW SUPERNODE.
C        MARKER - THE MARKER VECTOR.
C
C     UPDATED PARAMETERS -
C        ADJNCY - BECOMES THE ADJNCY OF THE QUOTIEN GRAPH.
C
C************************************************************
C
      SUBROUTINE  QMDQT ( ROOT, XADJ, ADJNCY, MARKER,
     1                    RCHSZE, RCHSET, NBRHD )
C           
C************************************************************
C
         INTEGER ADJNCY(1), MARKER(1), RCHSET(1), NBRHD(1)
	 INTEGER XADJ(1), INHD, IRCH, J, JSTRT, JSTOP, LINK,
     1           NABOR, NODE, RCHSZE, ROOT
C
C************************************************************
C
         IRCH = 0
	 INHD = 0
	 NODE = ROOT
  100    JSTRT = XADJ(NODE)
         JSTOP = XADJ(NODE+1) - 2
	 IF ( JSTOP .LT. JSTRT )  GO TO 300
C           ------------------------------------------------
C           PLACE REACH NODES INTO THE ADJACENT LIST OF NODE
C           ------------------------------------------------
            DO 200 J = JSTRT, JSTOP
	       IRCH = IRCH + 1
	       ADJNCY(J) = RCHSET(IRCH)
	       IF ( IRCH .GE. RCHSZE )  GOTO 400
  200       CONTINUE
C        ----------------------------------------------
C        LINK TO OTHER SPACE PROVIDED BY THE NBRHD SET.
C        ----------------------------------------------
  300    LINK = ADJNCY(JSTOP+1)
         NODE = - LINK
         IF ( LINK .LT. 0 )  GOTO 100
	    INHD = INHD + 1
	    NODE = NBRHD(INHD)
	    ADJNCY(JSTOP+1) = - NODE
	    GO TO 100
C        -------------------------------------------------------	       
C        ALL REACHABLE NODES HAVE BEEN SAVED.  END THE ADJ LIST.
C        ADD ROOT TO THE NBR LIST OF EACH NODE IN THE REACH SET.
C        -------------------------------------------------------
  400    ADJNCY(J+1) = 0
         DO 600 IRCH = 1, RCHSZE
	    NODE = RCHSET(IRCH)
	    IF ( MARKER(NODE) .LT. 0 )  GOTO 600
	       JSTRT = XADJ(NODE)
	       JSTOP = XADJ(NODE+1) - 1
	       DO 500 J = JSTRT, JSTOP
	          NABOR = ADJNCY(J)
		  IF ( MARKER(NABOR) .GE. 0 ) GO TO 500
		     ADJNCY(J) = ROOT
		     GOTO 600
  500          CONTINUE		     
  600    CONTINUE
         RETURN
      END
