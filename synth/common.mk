
YOSYS=yosys
NEXTPNR=nextpnr-ice40
NEXTPNR_OPTS=--hx8k
CLASH_OUTPUT_DIR=../../verilog/FSM.RiscV.ExampleTop.${CORETYPE}
TOP_ENTITY=${CLASH_OUTPUT_DIR}/${CORETYPE}.v
SV_SOURCES=${TOP_ENTITY}
HS_DIR=../../src
HS_FILES=$(wildcard ${HS_DIR}/FSM/*.hs ${HS_DIR}/FSM/*/*.hs)

synth: result.asc

result.json: ${SV_SOURCES}
	${YOSYS} -p "synth_ice40 -top ${CORETYPE} -json $@" $<

result.asc: result.json
	${NEXTPNR} ${NEXTPNR_OPTS} --json $< --asc $@ --log result.log

clean:
	rm -f result.json result.log

${TOP_ENTITY}: ${HS_FILES}
	cd ../.. && stack run clash -- -fclash-spec-limit=30 --verilog FSM.RiscV.ExampleTop

