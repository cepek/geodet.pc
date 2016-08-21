C*************************************************************
C*************************************************************
C*********     QMDRCH ..... QUOT MIN DEG REACH SET    ********
C*************************************************************
C*************************************************************
C
C     PURPOSE - THIS SUBROUTINE DETERMINES THE REACHABLE SET OF
C        A NODE THROUGH A GIVEN SUBSET. THE ADJACENCY STRUCTURE
C        IS ASSUMED TO BE STORED IN A QUOTIEN GRAPH FORMAT.
C
C     INPUT PARAMETERS -
C        ROOT - THE GIVEN NODE NOT IN THE SUBSET.
C        (XADJ, ADJNCY) - THE ADJACENCY STRUCTURE PAIR.
C        DEG - THE DEGREE VECTOR.  DEG(I) LT 0 MEANS THE NODE
C             BELONGS TO THE GIVEN SUBSET.
C
C     OUTPUT PARAMETERS -
C        (RCHSZE, RCHSET) - THE REACHABLE SET.
C        (NHDSZE, NBRHD) - THE NEIGHBORHOOD SET.
C
C     UPDATED PARAMETERS -
C        MARKER - THE MARKER VECTOR FOR REACH AND NBRHD SETS.
C               GT 0 MEANS THE NODE IS IN REACH SET.
C               LT 0 MEANS THE NODE HAS BEEN MERGED WITH
C               OTHERS IN THE QUOTIENT OR IT IS IN NBRHD SET.
C
C*************************************************************
C
      SUBROUTINE  QMDRCH ( ROOT, XADJ, ADJNCY, DEG, MARKER,
     1                     RCHSZE, RCHSET, NHDSZE, NBRHD )
C
C*************************************************************
C
         INTEGER ADJNCY(1), DEG(1), MARKER(1),
     1           RCHSET(1), NBRHD(1)
         INTEGER XADJ(1), I, ISTRT, ISTOP, J, JSTRT, JSTOP,
     1           NABOR, NHDSZE, NODE, RCHSZE, ROOT
C
C*************************************************************
C
C        -----------------------------------------
C        LOOP THROUGH THE NEIGHBORS OF ROOT IN THE
C        QUOTIENT GRAPH.
C        -----------------------------------------
         NHDSZE = 0
	 RCHSZE = 0
	 ISTRT = XADJ(ROOT)
	 ISTOP = XADJ(ROOT+1) - 1
	 IF ( ISTOP .LT. ISTRT )  RETURN
	    DO 600 I = ISTRT, ISTOP
	       NABOR =  ADJNCY(I)
	       IF ( NABOR .EQ. 0 ) RETURN
	       IF ( MARKER(NABOR) .NE. 0 )  GO TO 600
	          IF ( DEG(NABOR) .LT. 0 )     GO TO 200
C                    -------------------------------------
C                    INCLUDE NABOR INTO THE REACHABLE SET.
C                    -------------------------------------
                     RCHSZE = RCHSZE + 1
		     RCHSET(RCHSZE) = NABOR
		     MARKER(NABOR) = 1
		     GO TO 600
C                 -------------------------------------
C                 NABOR HAS BEEN ELIMINATED. FIND NODES
C                 REACHABLE FROM IT.
C                 -------------------------------------
  200             MARKER(NABOR) = -1
                  NHDSZE = NHDSZE + 1
		  NBRHD(NHDSZE) = NABOR
  300             JSTRT = XADJ(NABOR)
                  JSTOP = XADJ(NABOR+1) - 1
		  DO 500 J = JSTRT, JSTOP
		     NODE = ADJNCY(J)
		     NABOR = - NODE
		     IF (NODE) 300, 600, 400
  400                IF ( MARKER(NODE) .NE. 0 )  GO TO 500
                        RCHSZE = RCHSZE + 1
			RCHSET(RCHSZE) = NODE
			MARKER(NODE) = 1		     
  500             CONTINUE
  600       CONTINUE
            RETURN
      END
