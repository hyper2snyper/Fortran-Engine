MOD := ./debug/mod
OUT := ./debug/out
DEBUG := ./debug

LIB := ./lib
DEMO := ./demo

all: lib demo
	gfortran -g -Wall ./main.F90 $(OUT)/*.o -o $(DEBUG)/out.a -lncurses -J$(MOD) -I$(MOD)

lib: 
	cd $(OUT) && gfortran -c -g -Wall $(LIB)/*.F90 -J$(MOD) -I$(MOD)

demo: 
	cd $(OUT) && gfortran -c -g -Wall $(DEMO)/*.F90 -J$(MOD) -I$(MOD)