MOD := ./mod
OUT := ./out


all: action_m input_handler_m main_loop_m screen_handler_m screen_m vector2_m curses_m curses_init main selection_m log_m game_space_m
	gfortran  -g -Wall $(OUT)/*.o -o out.a -lncurses

action_m: ../action_m.F90
	gfortran -I$(MOD) -J$(MOD) -c -g -Wall ../action_m.F90 -o $(OUT)/action_m.o

input_handler_m: ../input_handler_m.F90
	gfortran -I$(MOD) -J$(MOD) -c -g -Wall ../input_handler_m.F90 -o $(OUT)/input_handler_m.o

main_loop_m: ../main_loop_m.F90
	gfortran -I$(MOD) -J$(MOD) -c -g -Wall ../main_loop_m.F90 -o $(OUT)/main_loop_m.o

screen_handler_m: ../screen_handler_m.F90
	gfortran -I$(MOD) -J$(MOD) -c -g -Wall ../screen_handler_m.F90 -o $(OUT)/screen_handler_m.o

screen_m: ../screen_m.F90
	gfortran -I$(MOD) -J$(MOD) -c -g -Wall ../screen_m.F90 -o $(OUT)/screen_m.o

vector2_m: ../vector2_m.F90
	gfortran -I$(MOD) -J$(MOD) -c -g -Wall ../vector2_m.F90 -o $(OUT)/vector2_m.o

curses_m: ../curses_m.F90
	gfortran -I$(MOD) -J$(MOD) -c -g -Wall ../curses_m.F90 -o $(OUT)/curses_m.o

curses_init: ../curses_init.c
	gcc -c -g -Wall ../curses_init.c -o $(OUT)/curses_init.o

selection_m: ../selection_m.F90
	gfortran -I$(MOD) -J$(MOD) -c -g -Wall ../selection_m.F90 -o $(OUT)/selection_m.o

log_m: ../log_m.F90
	gfortran -I$(MOD) -J$(MOD) -c -g -Wall ../log_m.F90 -o $(OUT)/log_m.o

game_space_m: ../game_space_m.F90
	gfortran -I$(MOD) -J$(MOD) -c -g -Wall ..game_space_m.F90 -o $(OUT)/game_space_m.o

main: ../main.F90
	gfortran -I$(MOD) -J$(MOD) -c -g -Wall ../main.F90 -o $(OUT)/main.o


