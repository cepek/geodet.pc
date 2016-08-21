#
#
# -----------------------------------------------------------------------

OBJ=$(shell ls *.for | sed s-sources/--g | sed s/.for/.o/g )

gt4lsa: $(OBJ)
	g77 -o gt4lsa $(OBJ)

%.o: %.for; g77 -O -c $<

# -----------------------------------------------------------------------

clean:
	rm -f *.o *~ gt4lsa GT4LSA.TMP GT4LSA.WC

archive:
	make clean
	tar czf gt4lsa-`awk '/gt4lsa  v./{ print $$6 }' gt4lsa.for`.tgz * \
	        --exclude=*.tgz
