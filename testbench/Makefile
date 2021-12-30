
VERILATOR_INCLUDE=/usr/share/verilator/include
SRCS=$(wildcard *.cpp)
OBJS=$(SRCS:.cpp=.o)
CXXFLAGS=-I. -I ${VERILATOR_INCLUDE} -I ${VERILATOR_INCLUDE}/vltstd
TOPLEVEL=toplevel.v
CLASH_OUTPUT_DIR=../verilog/FSM.RiscV.ExampleTop.topEntity
TOP_ENTITY=${CLASH_OUTPUT_DIR}/topEntity.v
SV_SOURCES=${TOPLEVEL} ${TOP_ENTITY}
VFLAGS=-Wno-fatal -I. -I${CLASH_OUTPUT_DIR}
TESTDIR=../riscv-tests
TESTS=$(notdir $(patsubst %.S,%,$(wildcard $(TESTDIR)/*.S)))

build: Vtoplevel.h
	${MAKE} run

${TOP_ENTITY}:
	cd .. && stack run clash -- --verilog FSM.RiscV.ExampleTop

Vtoplevel.h: $(SV_SOURCES)
	verilator ${VFLAGS} --cc ${TOPLEVEL} --Mdir .

run: $(addsuffix .run,$(TESTS))

%.run: testbench
	./testbench +text_file=$(TESTDIR)/$(@:.run=).text.vh +data_file=$(TESTDIR)/$(@:.run=).data.vh

testbench: ${OBJS}
	${CXX} ${CXXFLAGS} ${OBJS} ${VERILATOR_INCLUDE}/verilated.cpp -o testbench

%.o: %.cpp
	${CXX} ${CXXFLAGS} -c -o $@ $<

clean:
	rm -f testbench ${OBJS} $(wildcard V*)

