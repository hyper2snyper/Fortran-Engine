HOME_DIR := /home/base/Homework/Fortran-Engine

MOD := $(HOME_DIR)/debug/mod
OUT := $(HOME_DIR)/debug/out
DEBUG := $(HOME_DIR)/debug

LIB := $(HOME_DIR)/lib
DEMO := $(HOME_DIR)/demo

all: lib demo
	gfortran -g -Wall ./main.F90 $(OUT)/*.o -o $(DEBUG)/out.a -lncurses -J$(MOD) -I$(MOD)

lib: curses_init $(LIB)/*.F90
	cd $(OUT) && gfortran -c -g -Wall $(LIB)/*.F90 -J$(MOD) -I$(MOD)

curses_init: $(LIB)/curses_init.c
	cd $(OUT) && gcc -c -g -Wall $(LIB)/curses_init.c

demo: $(DEMO)/*.F90
	cd $(OUT) && gfortran -c -g -Wall $(DEMO)/*.F90 -J$(MOD) -I$(MOD)

clean:
	cd $(OUT) && rm *.o