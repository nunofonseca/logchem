#
#
#
index:
	@rm -f pkgIndex.tcl
	@ls *.tcl
	@echo "auto_mkindex  . *.tcl" | tclsh
	@echo "puts [glob *.tcl]; pkg_mkIndex -verbose -direct . *.tcl" | tclsh


stats: FORCE
	wc -l *.tcl

clean:
	rm *~


FORCE:
