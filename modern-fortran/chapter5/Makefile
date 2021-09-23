
.PHONY: clean

all: stock_gain	stock_volatility

.SUFFIXES: .f90 .o

stock_gain:
	gfortran mod_alloc.f90 mod_array.f90 mod_io.f90 stock_gain.f90 -o stock_gain

stock_volatility:
	gfortran mod_alloc.f90 mod_array.f90 mod_io.f90 stock_volatility.f90 -o stock_volatility

clean:
	$(RM) *.o *.mod stock_gain stock_volatility
