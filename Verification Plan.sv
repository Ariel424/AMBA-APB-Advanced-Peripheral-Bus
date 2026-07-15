`timescale 1ns / 1ps

`include "uvm_macros.svh"
import uvm_pkg::*;

//===========================================
// APB Configuration Class
//===========================================
class apb_config extends uvm_object;
  `uvm_object_utils(apb_config)
  
  uvm_active_passive_enum is_active = UVM_ACTIVE;
  bit has_coverage = 1;
  bit has_scoreboard = 1;
  int num_transactions = 20;
  
  function new(string name = "apb_config");
    super.new(name);
  endfunction
endclass

//===========================================
// Transaction Class
//===========================================
typedef enum bit [2:0] {
  WRITE     = 3'b000,
  READ      = 3'b001,
  RESET     = 3'b010,
  WRITE_ERR = 3'b011,
  READ_ERR  = 3'b100
} oper_mode_e;

class apb_transaction extends uvm_sequence_item;
  
  rand oper_mode_e      op;
  rand logic            pwrite;
  rand logic [31:0]     pwdata;
  rand logic [31:0]     paddr; 
  logic                 pready;
  logic                 pslverr;
  logic [31:0]          prdata;
  rand int unsigned     slave_wait_cycles;
  
  constraint c_wait_cycles { 
    slave_wait_cycles dist {0 := 70, [1:3] := 20, [4:10] := 10}; 
  }
  
  constraint valid_addr_c {
    (op == WRITE || op == READ) -> paddr < 32;
  }
  
  constraint error_addr_c {
    (op == WRITE_ERR || op == READ_ERR) -> paddr >= 32;
  }
  
  constraint op_type_c {
    op dist {
      WRITE     := 40,
      READ      := 40,
      WRITE_ERR := 10,
      READ_ERR  := 10
    };
  }
  
  `uvm_object_utils_begin(apb_transaction)
    `uvm_field_enum(oper_mode_e, op, UVM_ALL_ON)
    `uvm_field_int(pwrite, UVM_ALL_ON)
    `uvm_field_int(pwdata, UVM_ALL_ON)
    `uvm_field_int(paddr, UVM_ALL_ON)
    `uvm_field_int(pready, UVM_ALL_ON)
    `uvm_field_int(pslverr, UVM_ALL_ON)
    `uvm_field_int(prdata, UVM_ALL_ON)
  `uvm_object_utils_end
  
  function new(string name = "apb_transaction");
    super.new(name);
  endfunction
  
  function void post_randomize();
    case(op)
      WRITE, WRITE_ERR: pwrite = 1'b1;
      READ, READ_ERR:   pwrite = 1'b0;
      default:          pwrite = 1'b0;
    endcase
  endfunction
endclass

//===========================================
// Sequence Library
//===========================================
class apb_base_sequence extends uvm_sequence#(apb_transaction);
  `uvm_object_utils(apb_base_sequence)
  function new(string name = "apb_base_sequence");
    super.new(name);
  endfunction
endclass

class write_read_seq extends apb_base_sequence;
  `uvm_object_utils(write_read_seq)
  function new(string name = "write_read_seq");
    super.new(name);
  endfunction
  
  virtual task body();
    apb_transaction wr_tr, rd_tr;
    repeat(15) begin
      wr_tr = apb_transaction::type_id::create("wr_tr");
      start_item(wr_tr);
      if(!wr_tr.randomize() with {op == WRITE;}) `uvm_fatal("SEQ", "Randomization failed")
      finish_item(wr_tr);
      
      rd_tr = apb_transaction::type_id::create("rd_tr");
      start_item(rd_tr);
      if(!rd_tr.randomize() with { op == READ; paddr == wr_tr.paddr; }) `uvm_fatal("SEQ", "Randomization failed")
      finish_item(rd_tr);
    end
  endtask
endclass

class apb_back_to_back_stall_seq extends apb_base_sequence;
  `uvm_object_utils(apb_back_to_back_stall_seq)
  function new(string name = "apb_back_to_back_stall_seq");
    super.new(name);
  endfunction
  
  virtual task body();
    apb_transaction tr;
    repeat(40) begin
      tr = apb_transaction::type_id::create("tr");
      start_item(tr);
      if (!tr.randomize() with { slave_wait_cycles inside {[5:12]}; op inside {WRITE, READ}; })
        `uvm_fatal("SEQ", "Randomization failed!")
      finish_item(tr);
    end
  endtask
endclass

//===========================================
// Driver Class - Adapted for MP_DRIVER
//===========================================
class apb_driver extends uvm_driver#(apb_transaction);
  `uvm_component_utils(apb_driver)
  
  virtual apb_if.MP_DRIVER vif;
  
  function new(string name = "apb_driver", uvm_component parent = null);
    super.new(name, parent);
  endfunction
  
  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    drv_ap = uvm_analysis_port#(my_transaction)::type_id::create("drv_ap", this);
    if(!uvm_config_db#(virtual apb_if.MP_MONITOR)::get(this, "", "vif", vif)) begin
      `uvm_fatal("MON", "Unable to access Interface Modport (MP_MONITOR)")
    end
  endfunction
  
  task reset_signals();
    vif.drv_cb.psel    <= 1'b0;
    vif.drv_cb.penable <= 1'b0;
    vif.drv_cb.pwrite  <= 1'b0;
    vif.drv_cb.paddr   <= '0;
    vif.drv_cb.pwdata  <= '0;
    `uvm_info("DRV", "Signals Initialized via CB", UVM_MEDIUM);
  endtask
  
  virtual task drive_transaction(apb_transaction tr);
    case(tr.op)
      RESET: begin
        `uvm_info("DRV", "Waiting for external reset sequence...", UVM_MEDIUM);
      end
      
      WRITE, WRITE_ERR, READ, READ_ERR: begin
        // 1. Setup Phase
        @(vif.drv_cb); 
        vif.drv_cb.psel    <= 1'b1;
        vif.drv_cb.penable <= 1'b0;
        vif.drv_cb.pwrite  <= (tr.op == WRITE || tr.op == WRITE_ERR) ? 1'b1 : 1'b0;
        vif.drv_cb.paddr   <= tr.paddr;
        if (tr.pwrite) vif.drv_cb.pwdata <= tr.pwdata;
        
        // 2. Access Phase
        @(vif.drv_cb);
        vif.drv_cb.penable <= 1'b1;
        
        // 3. PREADY Wait Loop
        fork
          begin: wait_pready
            while (vif.drv_cb.pready !== 1'b1) begin
              @(vif.drv_cb);
            end
          end
          begin: timeout_watchdog
            repeat(50) @(vif.drv_cb);
            `uvm_error("DRV_TIMEOUT", "APB Slave hung! PREADY timeout.")
          end
        join_any
        disable fork;
        
        if (!tr.pwrite) tr.prdata = vif.drv_cb.prdata;
        tr.pslverr = vif.drv_cb.pslverr;
        
        // 4. End Transaction
        vif.drv_cb.psel    <= 1'b0;
        vif.drv_cb.penable <= 1'b0;
      end
    endcase
  endtask
  
  virtual task run_phase(uvm_phase phase);
    wait(vif.presetn == 1'b1);
    reset_signals();
    
    forever begin
      seq_item_port.get_next_item(req);
      drive_transaction(req);
      seq_item_port.item_done();
    end
  endtask
endclass

//===========================================
// Monitor Class - Adapted for MP_MONITOR
//===========================================
class apb_monitor extends uvm_monitor;
  `uvm_component_utils(apb_monitor)
  
  virtual apb_if.MP_MONITOR vif;
  uvm_analysis_port#(apb_transaction) mon_ap;
  
  function new(string name = "apb_monitor", uvm_component parent = null);
    super.new(name, parent);
  endfunction
  
  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    mon_ap = uvm_analysis_port#(my_transaction)::type_id::create("drv_ap", this);
    if(!uvm_config_db#(virtual apb_if.MP_MONITOR)::get(this, "", "vif", vif)) begin
      `uvm_fatal("MON", "Unable to access Interface Modport (MP_MONITOR)")
    end
  endfunction
  
  virtual task run_phase(uvm_phase phase);
    apb_transaction tr;
    
    forever begin
      @(vif.mon_cb);
      
      if(!vif.presetn) begin
        tr = apb_transaction::type_id::create("tr");
        tr.op = RESET;
        mon_ap.write(tr);
      end
      
      else if(vif.mon_cb.psel && vif.mon_cb.penable && vif.mon_cb.pready) begin
        tr = apb_transaction::type_id::create("tr");
        tr.paddr   = vif.mon_cb.paddr;
        tr.pwrite  = vif.mon_cb.pwrite;
        tr.pslverr = vif.mon_cb.pslverr;
        
        if(vif.mon_cb.pwrite) begin
          tr.pwdata = vif.mon_cb.pwdata;
          tr.op = (vif.mon_cb.pslverr) ? WRITE_ERR : WRITE;
        end
        else begin
          tr.prdata = vif.mon_cb.prdata;
          tr.op = (vif.mon_cb.pslverr) ? READ_ERR : READ;
        end
        
        mon_ap.write(tr);
      end
    end
  endtask
endclass

//===========================================
// Scoreboard & Env
//===========================================
class apb_scoreboard extends uvm_scoreboard;
  `uvm_component_utils(apb_scoreboard)
  
  uvm_analysis_imp#(apb_transaction, apb_scoreboard) sb_imp;
  logic [31:0] mem_model [32];

  int write_count = 0;
  int read_count  = 0;
  int match_count = 0;
  int error_count = 0;
  
  function new(string name = "apb_scoreboard", uvm_component parent);
    super.new(name, parent);
  endfunction
  
  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    sb_imp = new("sb_imp", this);
    reset_memory();
  endfunction

 virtual function void reset_memory();
    foreach(mem_model[i]) mem_model[i] = 32'h0;
    `uvm_info("SCB", "Internal memory model has been reset to all zeros.", UVM_MEDIUM);
  endfunction
  
  virtual function void write(apb_transaction tr);
    case(tr.op)
      
      RESET: begin
        `uvm_info("SCB", "Reset transaction detected. Clearing scoreboard model...", UVM_MEDIUM);
        reset_memory();
      end
      
      WRITE, WRITE_ERR: begin
        write_count++;
        if (!tr.pslverr) begin
          mem_model[tr.paddr] = tr.pwdata;
          `uvm_info("SCB", $sformatf("WRITE Verified: Addr=0x%0h, Data=0x%0h written to model.", tr.paddr, tr.pwdata), UVM_HIGH);
        end else begin
          `uvm_info("SCB", $sformatf("WRITE_ERR ignored by memory model (Addr=0x%0h)", tr.paddr), UVM_HIGH);
        end
      end
      
      READ, READ_ERR: begin
        read_count++;
        if (!tr.pslverr) begin
          logic [31:0] expected_data = mem_model[tr.paddr];
          
          if (tr.prdata === expected_data) begin
            match_count++;
            `uvm_info("SCB_MATCH", $sformatf("READ MATCH [Addr=0x%0h] - Got: 0x%0h, Expected: 0x%0h", tr.paddr, tr.prdata, expected_data), UVM_MEDIUM);
          end else begin
            error_count++;
            `uvm_error("SCB_MISMATCH", $sformatf("READ MISMATCH at Addr=0x%0h! Expected: 0x%0h, Got: 0x%0h", tr.paddr, expected_data, tr.prdata));
          end
        end else begin
          `uvm_info("SCB", $sformatf("READ_ERR detected (Addr=0x%0h). Skipping data match check.", tr.paddr), UVM_HIGH);
        end
      end
      
    endcase
  endfunction  
endclass

class apb_agent extends uvm_agent;
  `uvm_component_utils(apb_agent)
  apb_driver drv;
  apb_monitor mon;
  uvm_sequencer#(apb_transaction) seqr;
  apb_config cfg;
  
  function new(string name = "apb_agent", uvm_component parent = null); super.new(name, parent); endfunction
  
  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    cfg = apb_config::type_id::create("cfg");
    mon = apb_monitor::type_id::create("mon", this);
    drv = apb_driver::type_id::create("drv", this);
    seqr = uvm_sequencer#(apb_transaction)::type_id::create("seqr", this);
  endfunction
  
  virtual function void connect_phase(uvm_phase phase);
    super.connect_phase(phase);
    drv.seq_item_port.connect(seqr.seq_item_export);
  endfunction
endclass

class apb_env extends uvm_env;
  `uvm_component_utils(apb_env)
  apb_agent agt;
  apb_scoreboard scb;
  
  function new(string name = "apb_env", uvm_component parent = null); super.new(name, parent); endfunction
  
  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    agt = apb_agent::type_id::create("agt", this);
    scb = apb_scoreboard::type_id::create("scb", this);
  endfunction
  
  virtual function void connect_phase(uvm_phase phase);
    super.connect_phase(phase);
    agt.mon.mon_ap.connect(scb.sb_imp);
  endfunction
endclass

//===========================================
// Base Test - Config Link with New Modports
//===========================================
class apb_base_test extends uvm_test;
  `uvm_component_utils(apb_base_test)
  apb_env env;
  
  function new(string name = "apb_base_test", uvm_component parent = null); super.new(name, parent); endfunction
  
  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    
    virtual apb_if global_vif;
    if(!uvm_config_db#(virtual apb_if)::get(this, "", "vif", global_vif))
      `uvm_fatal("TEST", "Failed to get global_vif from config_db")
      
    uvm_config_db#(virtual apb_if.MP_DRIVER)::set(this, "env.agt.drv", "vif", global_vif.MP_DRIVER);
    uvm_config_db#(virtual apb_if.MP_MONITOR)::set(this, "env.agt.mon", "vif", global_vif.MP_MONITOR);
    
    env = apb_env::type_id::create("env", this);
  endfunction
endclass

class write_read_test extends apb_base_test;
  `uvm_component_utils(write_read_test)
  function new(string name = "write_read_test", uvm_component parent = null); super.new(name, parent); endfunction
  
  virtual task run_phase(uvm_phase phase);
    write_read_seq seq = write_read_seq::type_id::create("seq");
    phase.raise_objection(this);
    seq.start(env.agt.seqr);
    #100;
    phase.drop_objection(this);
  endtask
endclass

//=================================================
// Top Module
//=================================================
module tb_top;
  logic pclk;
  logic presetn;
  
  initial begin
    pclk = 0;
    forever #5 pclk = ~pclk;
  end
  
  initial begin
    presetn = 0;
    #20 presetn = 1;
  end
  
  apb_if inf(pclk, presetn);

  initial begin
    uvm_config_db#(virtual apb_if)::set(null, "uvm_test_top", "vif", inf);
    run_test("write_read_test");
  end
endmodule
