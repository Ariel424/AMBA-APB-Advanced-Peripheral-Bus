// Improved APB RAM Module with proper APB protocol implementation
module apb_ram #(
  parameter ADDR_WIDTH = 5,  // 2^5 = 32 locations
  parameter DATA_WIDTH = 32
)(
  input  logic                    presetn,
  input  logic                    pclk,
  input  logic                    psel,
  input  logic                    penable,
  input  logic                    pwrite,
  input  logic [ADDR_WIDTH-1:0]   paddr,
  input  logic [DATA_WIDTH-1:0]   pwdata,
  output logic [DATA_WIDTH-1:0]   prdata,
  output logic                    pready,
  output logic                    pslverr
);

  // Memory array - 32 locations of 32-bit data
  logic [DATA_WIDTH-1:0] mem [0:31];
  
  // APB States - Standard 2-state FSM
  typedef enum logic {
    IDLE   = 1'b0,
    ACCESS = 1'b1
  } apb_state_t;
  
  apb_state_t state, next_state;
  
  // Internal signals
  logic valid_addr;
  logic write_enable;
  logic read_enable;
  
  // Address validation
  assign valid_addr = (paddr < 32);
  
  // Control signals
  assign write_enable = psel && penable && pwrite && state == ACCESS;
  assign read_enable  = psel && penable && !pwrite && state == ACCESS;
  
  //===========================================
  // State Machine - Sequential Logic
  //===========================================
  always_ff @(posedge pclk or negedge presetn) begin
    if (!presetn) begin
      state <= IDLE;
    end else begin
      state <= next_state;
    end
  end
  
  //===========================================
  // Next State Logic
  //===========================================
  always_comb begin
    next_state = state;
    
    case (state)
      IDLE: begin
        if (psel && !penable) begin
          next_state = ACCESS;
        end
      end
      
      ACCESS: begin
        if (psel && penable) begin
          // Stay in ACCESS during transfer
          next_state = ACCESS;
        end else begin
          // Transaction complete, return to IDLE
          next_state = IDLE;
        end
      end
      
      default: next_state = IDLE;
    endcase
  end
  
  //===========================================
  // Memory Write Logic
  //===========================================
  always_ff @(posedge pclk or negedge presetn) begin
    if (!presetn) begin
      // Initialize memory to zero on reset
      for (int i = 0; i < 32; i++) begin
        mem[i] <= '0;
      end
    end else begin
      if (write_enable && valid_addr) begin
        mem[paddr] <= pwdata;
      end
    end
  end
  
  //===========================================
  // Memory Read Logic
  //===========================================
  always_ff @(posedge pclk or negedge presetn) begin
    if (!presetn) begin
      prdata <= '0;
    end else begin
      if (read_enable) begin
        if (valid_addr) begin
          prdata <= mem[paddr];
        end else begin
          prdata <= 32'hDEADBEEF;  // Error pattern for invalid address
        end
      end
    end
  end
  
  //===========================================
  // APB Response Signals - pready and pslverr
  //===========================================
  always_ff @(posedge pclk or negedge presetn) begin
    if (!presetn) begin
      pready  <= 1'b0;
      pslverr <= 1'b0;
    end else begin
      // Default values
      pready  <= 1'b0;
      pslverr <= 1'b0;
      
      // Generate response in ACCESS state when penable is high
      if (state == ACCESS && psel && penable) begin
        pready <= 1'b1;  // Always ready (single cycle response)
        
        // Error only for invalid address
        if (!valid_addr) begin
          pslverr <= 1'b1;
        end
      end
    end
  end

endmodule


//===========================================
// APB Interface Definition
//===========================================
interface apb_if #(
  parameter ADDR_WIDTH = 5,
  parameter DATA_WIDTH = 32
)();
  
  // Clock and Reset
  logic                   pclk;
  logic                   presetn;
  
  // Address and Control
  logic [ADDR_WIDTH-1:0]  paddr;
  logic                   pwrite;
  logic                   psel;
  logic                   penable;
  
  // Data
  logic [DATA_WIDTH-1:0]  pwdata;
  logic [DATA_WIDTH-1:0]  prdata;
  
  // Response
  logic                   pready;
  logic                   pslverr;
  
  // Modports for Master and Slave
  modport master (
    output pclk, presetn, paddr, pwrite, psel, penable, pwdata,
    input  prdata, pready, pslverr
  );
  
  modport slave (
    input  pclk, presetn, paddr, pwrite, psel, penable, pwdata,
    output prdata, pready, pslverr
  );
  
  modport monitor (
    input pclk, presetn, paddr, pwrite, psel, penable, pwdata,
          prdata, pready, pslverr
  );

endinterface : apb_if
