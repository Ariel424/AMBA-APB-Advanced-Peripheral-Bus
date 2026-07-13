interface apb_if (input logic pclk, input logic presetn);
  
  // APB Signals
  logic [31:0] paddr;
  logic        psel;
  logic        penable;
  logic        pwrite;
  logic [31:0] pwdata;
  logic [31:0] prdata;
  logic        pready;
  logic        pslverr;

  //=================================================
  // Driver Clocking Block
  //=================================================
  clocking drv_cb @(posedge pclk);
    default input #1ns output #1ns;
    output paddr, psel, penable, pwrite, pwdata;
    input  pready, pslverr, prdata;
  endclocking

  //=================================================
  // Monitor Clocking Block
  //=================================================
  clocking mon_cb @(posedge pclk);
    default input #1ns;
    input paddr, psel, penable, pwrite, pwdata, prdata, pready, pslverr;
  endclocking

  //=================================================
  // Modports
  //=================================================
  modport MP_DRIVER (clocking drv_cb, input presetn);
  modport MP_MONITOR (clocking mon_cb, input presetn);

endinterface
