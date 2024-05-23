HOME_DIR := /home/base/Term-Project2

MOD := $(HOME_DIR)/debug/mod
OUT := $(HOME_DIR)/debug/out
DEBUG := $(HOME_DIR)/debug

LIB := $(HOME_DIR)/lib
DEMO := $(HOME_DIR)/demo

all: lib demo
	gfortran -g ./main.F90 $(OUT)/*.o -o $(DEBUG)/out.a -lncurses -J$(MOD) -I$(MOD)

lib: curses_init $(LIB)/*.F90
	cd $(OUT) && gfortran -c -g $(LIB)/*.F90 -J$(MOD) -I$(MOD)

curses_init: $(LIB)/curses_init.c
	cd $(OUT) && gcc -c -g $(LIB)/curses_init.c

demo: $(DEMO)/*.F90 snake tanks dungeon
	cd $(OUT) && gfortran -c -g $(DEMO)/*.F90 -J$(MOD) -I$(MOD)

snake: $(DEMO)/snake/*.F90
	cd $(OUT) && gfortran -c -g $(DEMO)/snake/*.F90 -J$(MOD) -I$(MOD)

tanks: $(DEMO)/tanks/*.F90
	cd $(OUT) && gfortran -c -g $(DEMO)/tanks/*.F90 -J$(MOD) -I$(MOD)

dungeon: $(DEMO)/dungeon/*.F90
	cd $(OUT) && gfortran -c -g $(DEMO)/dungeon/*.F90 -J$(MOD) -I$(MOD)

clean:
	cd $(OUT) && rm *.o

