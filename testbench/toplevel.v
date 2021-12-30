`timescale 100fs/100fs
function string text_mem_file ();
    string s;
    if ($value$plusargs("text_file=%s", s) != 0)
        return s;
    else begin
        $display("Text memory file not supplied.");
        $finish;
    end
endfunction

function string data_mem_file ();
    string s;
    if ($value$plusargs("data_file=%s", s) != 0)
        return s;
    else begin
        $display("Data memory file not supplied.");
        $finish;
    end
endfunction

module toplevel(
    input CLK, RST, EN,
    output WE_O, CYC_O, STB_O,
    output reg ACK_I,
    output [31:0] ADR_O, DAT_O,
    output reg [31:0] DAT_I
);
    topEntity top(
        .CLK(CLK), .RST(RST), .EN(EN),
        .WB_WE_O(WE_O), .WB_CYC_O(CYC_O), .WB_STB_O(STB_O),
        .WB_ADR_O(ADR_O), .WB_DAT_O(DAT_O), 
        .WB_ACK_I(ACK_I), .WB_DAT_I(DAT_I)
    );
endmodule
