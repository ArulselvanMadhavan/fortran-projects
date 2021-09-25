FC=gfortran
FMT=fprettify

all: echo read_write_list standard_streams dashboard qn

echo: echo.f90
	$(FC) $< -o $@

read_write_list: read_write_list.f90
	$(FC) $< -o $@

standard_streams: standard_streams.f90
	$(FC) $< -o $@

dashboard: dashboard.f90
	$(FC) $< -o $@

qn: qn.f90
	$(FC) $< -o $@

clean:
	$(RM) *.o *.mod echo read_write_list standard_streams dashboard qn

format:
	$(FMT) *.f90
