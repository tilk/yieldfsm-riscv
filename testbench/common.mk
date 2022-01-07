
VERILATOR_INCLUDE=/usr/share/verilator/include
SRCS=$(wildcard *.cpp)
OBJS=$(SRCS:.cpp=.o)
CXXFLAGS=-I. -I ${VERILATOR_INCLUDE} -I ${VERILATOR_INCLUDE}/vltstd
TOPLEVEL=../toplevel.v
CLASH_OUTPUT_DIR=../../verilog/FSM.RiscV.ExampleTop.${CORETYPE}
TOP_ENTITY=${CLASH_OUTPUT_DIR}/${CORETYPE}.v
HS_DIR=../../src
HS_FILES=$(wildcard ${HS_DIR}/FSM/*.hs ${HS_DIR}/FSM/*/*.hs)
SV_SOURCES=${TOPLEVEL} ${TOP_ENTITY}
VFLAGS=-Wno-fatal -I. -I${CLASH_OUTPUT_DIR}
TESTDIR=../../riscv-tests
TESTS=$(notdir $(patsubst %.S,%,$(wildcard $(TESTDIR)/*.S)))

build: Vtoplevel.h main.cpp
	${MAKE} run

${TOP_ENTITY}: ${HS_FILES}
	cd ../.. && stack run clash -- -fclash-spec-limit=30 --verilog FSM.RiscV.ExampleTop

Vtoplevel.h: $(SV_SOURCES)
	verilator ${VFLAGS} -DCORETYPE=${CORETYPE} --cc ${TOPLEVEL} --Mdir .

run: $(addsuffix .run,$(TESTS))

%.run: testbench
	./testbench +text_file=$(TESTDIR)/$(@:.run=).text.vh +data_file=$(TESTDIR)/$(@:.run=).data.vh

testbench: ${OBJS}
	${CXX} ${CXXFLAGS} ${OBJS} ${VERILATOR_INCLUDE}/verilated.cpp -o testbench

main.cpp: ../main.cpp
	cp ../main.cpp .

%.o: %.cpp
	${CXX} ${CXXFLAGS} -c -o $@ $<

clean:
	rm -f testbench ${OBJS} $(wildcard V*)

