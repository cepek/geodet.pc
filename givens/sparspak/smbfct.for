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
