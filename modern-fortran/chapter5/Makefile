
.PHONY: clean

all: stock_gain

stock_gain:
	gfortran mod_array.f90 mod_io.f90 stock_gain.f90 -o stock_gain

clean:
	$(RM) *.o *.mod stock_gain
