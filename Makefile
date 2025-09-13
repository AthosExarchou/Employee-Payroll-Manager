# Makefile for Employee Payroll Manager COBOL program

# Compiler
COBOLC = cobc

# Source file
SRC = payroll.cob

# Output executable
EXE = payroll

# Default target: compile and link
all: $(EXE)

$(EXE): $(SRC)
	$(COBOLC) -x $(SRC) -o $(EXE)

# Clean up generated files
clean:
	rm -f $(EXE) *.o *.lst *.tmp report.txt error.log

# Run the program
run: $(EXE)
	./$(EXE)
