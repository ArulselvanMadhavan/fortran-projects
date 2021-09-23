

.PHONY: compilerinfo
compilerinfo:
	gfortran -fcheck=all -g -O0 -fbacktrace \
compilerinfo.f90 -o compilerinfo

.PHONY: tsunami
tsunami:
	gfortran mod_diff.f90 mod_initial.f90 tsunami.f90 -o tsunami
