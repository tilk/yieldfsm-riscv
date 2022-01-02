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
`define TEXT_END        `TEXT_BEGIN + `TEXT_SIZE

`define DATA_BEGIN      32'h8000_0000
`define DATA_BITS       17
`define DATA_SIZE       2**`DATA_BITS
`define DATA_END        `DATA_BEGIN + `DATA_SIZE

module wordmem#(BITS)(
    input string hexfile,
    input clk,
    input [BITS-1:0] word_addr,
    input [31:0] in,
    input wr,
    output [31:0] out
);
    reg [31:0] words[0:2**BITS-1];

    assign out = words[word_addr];

    always @(posedge clk)
        if (wr)
            words[word_addr] <= in;
    
    initial $readmemh(hexfile, words);
endmodule

module toplevel(
    input clk, rst, en,
    output we_o, cyc_o, stb_o,
    output ack_i,
    output [31:0] adr_o, dat_o,
    output [31:0] dat_i
);
    wire text_wr, data_wr, text_en, data_en;
    wire [31:0] text_out, data_out;

    assign text_en = cyc_o && stb_o &&
                     adr_o >= `TEXT_BEGIN && adr_o < `TEXT_END;
    assign data_en = cyc_o && stb_o &&
                     adr_o >= `DATA_BEGIN && adr_o < `DATA_END;
    assign text_wr = text_en && we_o;
    assign data_wr = data_en && we_o;
    assign ack_i = text_en || data_en;
    assign dat_i = text_en ? text_out :
                   data_en ? data_out : 32'bx;

    topEntity top(
        .clk(clk), .rst(rst), .en(en),
        .wb_we_o(we_o), .wb_cyc_o(cyc_o), .wb_stb_o(stb_o),
        .wb_adr_o(adr_o), .wb_dat_o(dat_o), 
        .wb_ack_i(ack_i), .wb_dat_i(dat_i)
    );

    string textfile = text_mem_file();
    string datafile = data_mem_file();

    wordmem#(`TEXT_BITS-2) textmem(
        .hexfile(textfile),
        .clk(clk), .word_addr(adr_o[`TEXT_BITS-1:2]),
        .in(dat_o), .wr(text_wr), .out(text_out)
    );

    wordmem#(`DATA_BITS-2) datamem(
        .hexfile(datafile),
        .clk(clk), .word_addr(adr_o[`DATA_BITS-1:2]),
        .in(dat_o), .wr(data_wr), .out(data_out)
    );
endmodule

