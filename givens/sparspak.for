C$INCLUDE: '.\SPARSPAK\SMBFCT.FOR'
C*************************************************************
C*************************************************************
C********     SMBFCT ..... SYMBOLIC FACTORIZATION     ********
C*************************************************************
C*************************************************************
C
C     PURPOSE - THIS ROUTINE PERFORMS SYMBOLIC FACTORIZATION
C        ON A PERMUTED LINEAR SYSTEM AND IT ALSO SETS UP THE
C        COMPRESSED DATA STRUCTURE FOR THE SYSTEM.
C
C     INPUT PARAMETERS -
C        NEQNS - NUMBER OF EQUATIONS.
C        (XADJ, ADJNCY) - THE ADJACENCY STRUCTURE.
C        (PERM, INVP) - THE PERMUTATION VECTOR AND ITS INVERSE.
C
C     UPDATED PARAMETERS -
C        MAXSUB - SIZE OF THE SUBSCRIPT ARRAY NZSUB. ON RETURN,
C               IT CONTAINS THE NUMBER OF SUBSCRIPT USED
C
C     OUTPUT PARAMETERS -
C        XLNZ - INDEX INTO THE NONZERO STORAGE VECTOR LNZ.
C        (XNZSUB, NZSUB) - THE COMPRESSED SUBSCRIPT VECTORS.
C        MAXLNZ - THE NUMBER OF NONZEROS FOUND.
C        FLAG - ERROR FLAG.  POSITIVE VALUE INDICATES THAT
C               NZSUB ARRAY IS TOO SMALL.
C
C     WORKING PARAMETERS -
C        MRGLNK - A VECTOR OF SIZE NEQNS.  AT THE KTH STEP,
C               MRGLNK(K), MRGLNK(MRGLNK(K)) , .........
C               IS A LIST CONTAINING ALL THOSE COLUMNS L(*,J)
C               WITH J LESS THAN K, SUCH THAT ITS FIRST OFF-
C               DIAGONAL NONZERO IS L(K,J). THUS, THE
C               NONZERO STRUCTURE OF COLUMN L(*,K) CAN BE FOUND
C               BY MERGING THAT OF SUCH COLUMNS L(*,J) WITH
C               THE STRUCTURE OF A(*,K).
C        RCHLNK - A VECTOR OF SIZE NEQNS.  IT IS USED TO ACCUMULATE
C               THE STRUCTURE OF EACH COLUMN L(*,K).  AT THE
C               END OF THE KTH STEP,
C                   RCHLNK(K), RCHLNK(RCHLNK(K)), ........
C               IS THE LIST OF POSITIONS OF NONZEROS IN COLUMN K
C               OF THE FACTOR L.
C        MARKER  - AN INTEGER VECTOR OF LENGTH NEQNS. IT IS USED
C               TO TEST IF MASS SYMBOLIC ELIMINATION CAN BE
C               PERFORMED.  THAT IS, IT IS USED TO CHECK WHETHER
C               THE STRUCTURE OF THE CURRENT COLUMN K BEING
C               PROCESSED IS COMPLETELY DETERMINED BY THE SINGLE
C               COLUMN MRGLNK(K).
C
C*************************************************************
C
      SUBROUTINE  SMBFCT ( NEQNS, XADJ, ADJNCY, PERM, INVP,
     1                     XLNZ, MAXLNZ, XNZSUB, NZSUB, MAXSUB,
     1                     RCHLNK, MRGLNK, MARKER, FLAG )
C
C*************************************************************
C
         INTEGER ADJNCY(1), INVP(1), MRGLNK(1), NZSUB(1),
     1           PERM(1), RCHLNK(1), MARKER(1)
         INTEGER XADJ(1), XLNZ(1), XNZSUB(1),
     1           FLAG, I, INZ, J, JSTOP, JSTRT, K, KNZ,
     1           KXSUB, MRGK, LMAX, M, MAXLNZ, MAXSUB,
     1           NABOR, NEQNS, NODE, NP1, NZBEG, NZEND,
     1           RCHM, MRKFLG
C
C*************************************************************
C
C        ------------------
C        INITIALIZATION ...
C        ------------------
         NZBEG = 1
	 NZEND = 0
	 XLNZ(1) = 1
	 DO 100 K = 1, NEQNS
	    MRGLNK(K) = 0
	    MARKER(K) = 0
  100    CONTINUE
C        --------------------------------------------------
C        FOR EACH COLUMN ......... .  KNZ COUNTS THE NUMBER
C        OF NONZEROS IN COLUMN K ACCUMULATED IN RCHLNK.
C        --------------------------------------------------
         NP1 = NEQNS + 1
	 DO 1500 K = 1, NEQNS
	    KNZ = 0
	    MRGK = MRGLNK(K)
	    MRKFLG = 0
	    MARKER(K) = K
	    IF (MRGK .NE. 0 ) MARKER(K) = MARKER(MRGK)
	    XNZSUB(K) = NZEND
	    NODE = PERM(K)
	    JSTRT = XADJ(NODE)
	    JSTOP = XADJ(NODE+1) - 1
	    IF (JSTRT.GT.JSTOP)  GO TO 1500
C           -------------------------------------------
C           USE RCHLNK TO LINK THROUGH THE STRUCTURE OF
C           A(*,K) BELOW DIAGONAL
C           -------------------------------------------
            RCHLNK(K) = NP1
	    DO 300 J = JSTRT, JSTOP
	       NABOR = ADJNCY(J)
	       NABOR = INVP(NABOR)
	       IF ( NABOR .LE. K )  GO TO 300
	          RCHM = K
  200             M = RCHM
                  RCHM = RCHLNK(M)
		  IF ( RCHM .LE. NABOR )  GO TO 200
		     KNZ = KNZ+1
		     RCHLNK(M) = NABOR
		     RCHLNK(NABOR) = RCHM
		     IF ( MARKER(NABOR) .NE. MARKER(K) ) MRKFLG = 1
  300       CONTINUE
C           --------------------------------------
C           TEST FOR MASS SYMBOLIC ELIMINATION ...
C           --------------------------------------
            LMAX = 0
            IF ( MRKFLG .NE. 0 .OR. MRGK .EQ. 0 ) GO TO 350
            IF ( MRGLNK(MRGK) .NE. 0 ) GO TO 350
            XNZSUB(K) = XNZSUB(MRGK) + 1
            KNZ = XLNZ(MRGK+1) - (XLNZ(MRGK) + 1)
            GO TO 1400
C           -----------------------------------------------
C           LINK THROUGH EACH COLUMN I THAT AFFECTS L(*,K).
C           -----------------------------------------------
  350       I = K
  400       I = MRGLNK(I)
            IF (I.EQ.0)  GO TO 800
	       INZ = XLNZ(I+1) - (XLNZ(I)+1)
	       JSTRT = XNZSUB(I) + 1
	       JSTOP = XNZSUB(I) + INZ
	       IF (INZ.LE.LMAX)  GO TO 500
	          LMAX = INZ
		  XNZSUB(K) = JSTRT
C              -----------------------------------------------
C              MERGE STRUCTURE OF L(*,I) IN NZSUB INTO RCHLNK.
C              -----------------------------------------------
  500          RCHM = K
               DO 700 J = JSTRT, JSTOP
                  NABOR = NZSUB(J)
  600             M = RCHM
                  RCHM = RCHLNK(M)
                  IF (RCHM.LT.NABOR)  GO TO 600
		  IF (RCHM.EQ.NABOR)  GO TO 700
		     KNZ = KNZ+1
                     RCHLNK(M) = NABOR
                     RCHLNK(NABOR) = RCHM
                     RCHM = NABOR
  700          CONTINUE
               GO TO 400
C           -----------------------------------------------------
C           CHECK IF SUBSRIPTS DUPLICATE THOSE OF ANOTHER COLUMN.
C           -----------------------------------------------------
  800       IF (KNZ.EQ.LMAX)  GO TO 1400
C              -----------------------------------------------
C              OR IF TAIL OF K-1ST COLUMN MATCHES HEAD OF KTH.
C              -----------------------------------------------
               IF ( NZBEG.GT.NZEND)  GO TO 1200
                  I = RCHLNK(K)
                  DO 900 JSTRT=NZBEG,NZEND
                     IF (NZSUB(JSTRT)-I)  900, 1000, 1200
  900             CONTINUE
                  GO TO 1200
 1000             XNZSUB(K) = JSTRT
                  DO 1100 J=JSTRT,NZEND
                     IF (NZSUB(J).NE.I)  GO TO 1200
                     I = RCHLNK(I)
                     IF (I.GT.NEQNS)  GO TO 1400
 1100             CONTINUE
                  NZEND = JSTRT - 1
C              ----------------------------------------
C              COPY THE STRUCTURE OF L(*,K) FROM RCHLNK
C              TO THE DATA STRUCTURE (XNZSUB, NZSUB).
C              ----------------------------------------
 1200          NZBEG = NZEND +  1
               NZEND = NZEND + KNZ
               IF (NZEND.GT.MAXSUB)  GO TO 1600
               I = K
               DO 1300 J=NZBEG,NZEND
                  I = RCHLNK(I)
                  NZSUB(J) = I
                  MARKER(I) = K
 1300          CONTINUE
               XNZSUB(K) = NZBEG
               MARKER(K) = K
C           --------------------------------------------------------
C           UPDATE THE VECTOR MRGLNK.  NOTE COLUMN L(*,K) JUST FOUND
C           IS REQUIRED TO DETERMINE COLUMN L(*,J), WHERE
C           L(J,K) IS THE FIRST NONZERO IN L(*,K) BELOW DIAGONAL.
C           --------------------------------------------------------
 1400       IF (KNZ.LE.1)  GO TO 1500
               KXSUB = XNZSUB(K)
               I = NZSUB(KXSUB)
               MRGLNK(K) = MRGLNK(I)
               MRGLNK(I) = K
 1500       XLNZ(K+1) = XLNZ(K) + KNZ
         MAXLNZ = XLNZ(NEQNS) - 1
         MAXSUB = XNZSUB(NEQNS)
         XNZSUB(NEQNS+1) = XNZSUB(NEQNS)
         FLAG = 0
         RETURN
C        ----------------------------------------------------
C        ERROR - INSUFFUCIENT STORAGE FOR NONZERO SUBSCRIPTS.
C        ----------------------------------------------------
 1600    FLAG = 1
         RETURN
	 END
C$INCLUDE: '.\SPARSPAK\GENQMD.FOR'
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
C$INCLUDE: '.\SPARSPAK\QMDRCH.FOR'
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
C$INCLUDE: '.\SPARSPAK\QMDQT.FOR'
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
C$INCLUDE: '.\SPARSPAK\QMDUPD.FOR'
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
C$INCLUDE: '.\SPARSPAK\QMDMRG.FOR'
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
C$INCLUDE: '.\SPARSPAK\ROOTLS.FOR'
C*************************************************************
C*************************************************************
C********     ROOTLS ..... ROOTED LEVEL STRUCTURE      *******
C*************************************************************
C*************************************************************
C
C     PURPOSE - ROOTLS GENERATES THE LEVEL STRUCTURE ROOTED
C        AT THE INPUT NODE CALLED ROOT. ONLY THOSE NODES FOR
C        WHICH MASK IS NONZERO WILL BE CONSIDERED.
C
C     INPUT PARAMETERS -
C        ROOT - THE NODE AT WHICH THE LEVEL STRUCTURE IS TO
C               BE ROOTED.
C        (XADJ, ADJNCY) - ADJACENCY STRUCTURE PAIR FOR THE
C               GIVEN GRAPH.
C        MASK - IS USED TO SPECIFY A SECTION SUBGRAPH. NODES
C               WITH MASK(I)=0 ARE IGNORED.
C
C     OUTPUT PARAMETERS -
C        NLVL - IS THE NUMBER OF LEVELS IN THE LEVEL STRUCTURE.
C        (XLS, LS) - ARRAY PAIR FOR THE ROOTED LEVEL STRUCTURE.
C
C*************************************************************
C
      SUBROUTINE ROOTLS ( ROOT, XADJ, ADJNCY, MASK, NLVL, XLS, LS )
C
C*************************************************************
C
         INTEGER ADJNCY(1), LS(1), MASK(1), XLS(1)
         INTEGER XADJ(1), I, J, JSTOP, JSTRT, LBEGIN,
     1           CCSIZE, LVLEND, LVSIZE, NBR, NLVL,
     1           NODE, ROOT
C
C*************************************************************
C
C        ------------------ 
C        INITIALIZATION ...
C        ------------------ 
         MASK(ROOT) = 0
         LS(1) = ROOT
         NLVL = 0
         LVLEND = 0
         CCSIZE = 1
C        -----------------------------------------------------
C        LBEGIN IS THE POINTER TO THE BEGINNING OF THE CURRENT
C        LEVEL, AND LVLEND POINTS TO THE END OF THIS LEVEL.
C        -----------------------------------------------------
  200    LBEGIN = LVLEND + 1
         LVLEND = CCSIZE
         NLVL = NLVL + 1
         XLS(NLVL) = LBEGIN
C        -------------------------------------------------
C        GENERATE THE NEXT LEVEL BY FINDING ALL THE MASKED
C        NEIGHBORS OF NODES IN THE CURRENT LEVEL
C        -------------------------------------------------
         DO 400 I = LBEGIN, LVLEND
            NODE = LS(I)
            JSTRT = XADJ(NODE)
            JSTOP = XADJ(NODE + 1) - 1
            IF (JSTOP .LT. JSTRT )  GO TO 400
               DO 300 J = JSTRT, JSTOP
                  NBR = ADJNCY(J)
                  IF (MASK(NBR) .EQ. 0) GO TO 300
                      CCSIZE = CCSIZE + 1
                      LS(CCSIZE) = NBR
                      MASK(NBR) = 0
  300          CONTINUE
  400    CONTINUE
C        ------------------------------------------
C        COMPUTE THE CURRENT LEWEL WIDTH.
C        IF IT IS NONZERO, GENERATE THE NEXT LEVEL.
C        ------------------------------------------
         LVSIZE = CCSIZE - LVLEND
         IF (LVSIZE .GT. 0) GO TO 200
C        -------------------------------------------------------
C        RESET MASK TO ONE FOR THE NODES IN THE LEVEL STRUCTURE.
C        -------------------------------------------------------
         XLS(NLVL+1) = LVLEND + 1
         DO 500 I = 1, CCSIZE
            NODE = LS(I)
            MASK(NODE) = 1
  500    CONTINUE
         RETURN
      END
