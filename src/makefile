F90=gfortran
FLAGS=-O0 -fcheck=all
EXE=exec

all:$(EXE) clean

$(EXE):parameters.o write_solution.o geometry.o get_mesh.o quality.o delaunay.o border.o subdivision.o main.o
	$(F90) -o $@ $^
%.o:%.f90
	$(F90) $(FLAGS) -c $<
clean:
	rm *.o *.mod
