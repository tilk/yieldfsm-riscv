`timescale 100fs/100fs
function string text_mem_file ();
    string s;
    if ($value$plusargs("text_file=%s", s) != 0)
        text_mem_file = s;
    else begin
        $display("Text memory file not supplied.");
        $finish;
    end
endfunction

function string data_mem_file ();
    string s;
    if ($value$plusargs("data_file=%s", s) != 0)
        data_mem_file = s;
    else begin
        $display("Data memory file not supplied.");
        $finish;
    end
endfunction

`define TEXT_BEGIN      32'h00400000
`define TEXT_BITS       16
`define TEXT_SIZE       2**`TEXT_BITS
`define TEXT_END        (`TEXT_BEGIN + `TEXT_SIZE)
`define TEXT_BITS_WB    (`TEXT_BITS - 2)
`define TEXT_BEGIN_WB   (`TEXT_BEGIN >> 2)
`define TEXT_END_WB     (`TEXT_END >> 2)

`define DATA_BEGIN      32'h8000_0000
`define DATA_BITS       17
`define DATA_SIZE       2**`DATA_BITS
`define DATA_END        (`DATA_BEGIN + `DATA_SIZE)
`define DATA_BITS_WB    (`DATA_BITS - 2)
`define DATA_BEGIN_WB   (`DATA_BEGIN >> 2)
`define DATA_END_WB     (`DATA_END >> 2)

module wordmem#(BITS)(
    input string hexfile,
    input clk,
    input [BITS-1:0] word_addr,
    input [31:0] in,
    input [3:0] sel,
    input wr,
    output [31:0] out
);
    reg [31:0] words[0:2**BITS-1];
    wire [31:0] mask;

    assign out = words[word_addr];

    always @(posedge clk)
        if (wr) begin
            if (sel[0]) words[word_addr][0+:8] <= in[0+:8];
            if (sel[1]) words[word_addr][8+:8] <= in[8+:8];
            if (sel[2]) words[word_addr][16+:8] <= in[16+:8];
            if (sel[3]) words[word_addr][24+:8] <= in[24+:8];
        end
    
    initial $readmemh(hexfile, words);
endmodule

module toplevel(
    input clk, rst, en,
    output we_o, cyc_o, stb_o,
    output [3:0] sel_o,
    output ack_i,
    output [29:0] adr_o,
    output [31:0] dat_o,
    output [31:0] dat_i
);
    wire text_wr, data_wr, text_en, data_en;
    wire [31:0] text_out, data_out;

    assign text_en = cyc_o && stb_o &&
                     adr_o >= `TEXT_BEGIN_WB && adr_o < `TEXT_END_WB;
    assign data_en = cyc_o && stb_o &&
                     adr_o >= `DATA_BEGIN_WB && adr_o < `DATA_END_WB;
    assign text_wr = text_en && we_o;
    assign data_wr = data_en && we_o;
    assign ack_i = text_en || data_en;
    assign dat_i = text_en ? text_out :
                   data_en ? data_out : 32'bx;

    `CORETYPE top(
        .clk(clk), .rst(rst), .en(en),
        .wb_we_o(we_o), .wb_cyc_o(cyc_o),
        .wb_stb_o(stb_o), .wb_sel_o(sel_o),
        .wb_adr_o(adr_o), .wb_dat_o(dat_o), 
        .wb_ack_i(ack_i), .wb_dat_i(dat_i)
    );

    string textfile = text_mem_file();
    string datafile = data_mem_file();

    wordmem#(`TEXT_BITS_WB) textmem(
        .hexfile(textfile),
        .clk(clk), .word_addr(adr_o[`TEXT_BITS_WB-1:0]),
        .sel(sel_o), .in(dat_o), .wr(text_wr), .out(text_out)
    );

    wordmem#(`DATA_BITS_WB) datamem(
        .hexfile(datafile),
        .clk(clk), .word_addr(adr_o[`DATA_BITS_WB-1:0]),
        .sel(sel_o), .in(dat_o), .wr(data_wr), .out(data_out)
    );
endmodule

