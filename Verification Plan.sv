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
  rand logic [31:0]     paddr; // תוקן מ- [4:0] ל- [31:0] כדי למנוע התנגשות אילוצים בכתובות שגיאה
  logic                 pready;
  logic                 pslverr;
  logic [31:0]          prdata;
  rand int unsigned     slave_wait_cycles;
  
  // Constraints
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

class write_seq extends apb_base_sequence;
  `uvm_object_utils(write_seq)
  function new(string name = "write_seq");
    super.new(name);
  endfunction
  
  virtual task body();
    apb_transaction tr;
    repeat(15) begin
      tr = apb_transaction::type_id::create("tr");
      start_item(tr);
      if(!tr.randomize() with {op == WRITE;}) `uvm_fatal("SEQ", "Randomization failed")
      finish_item(tr);
    end
  endtask
endclass

class read_seq extends apb_base_sequence;
  `uvm_object_utils(read_seq)
  function new(string name = "read_seq");
    super.new(name);
  endfunction
  
  virtual task body();
    apb_transaction tr;
    repeat(15) begin
      tr = apb_transaction::type_id::create("tr");
      start_item(tr);
      if(!tr.randomize() with {op == READ;}) `uvm_fatal("SEQ", "Randomization failed")
      finish_item(tr);
    end
  endtask
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

class bulk_write_read_seq extends apb_base_sequence;
  `uvm_object_utils(bulk_write_read_seq)
  function new(string name = "bulk_write_read_seq");
    super.new(name);
  endfunction
  
  virtual task body();
    apb_transaction tr;
    repeat(20) begin
      tr = apb_transaction::type_id::create("tr");
      start_item(tr);
      if(!tr.randomize() with {op == WRITE;}) `uvm_fatal("SEQ", "Randomization failed")
      finish_item(tr);
    end
    repeat(20) begin
      tr = apb_transaction::type_id::create("tr");
      start_item(tr);
      if(!tr.randomize() with {op == READ;}) `uvm_fatal("SEQ", "Randomization failed")
      finish_item(tr);
    end
  endtask
endclass

class write_error_seq extends apb_base_sequence;
  `uvm_object_utils(write_error_seq)
  function new(string name = "write_error_seq");
    super.new(name);
  endfunction
  
  virtual task body();
    apb_transaction tr;
    repeat(10) begin
      tr = apb_transaction::type_id::create("tr");
      start_item(tr);
      if(!tr.randomize() with {op == WRITE_ERR;}) `uvm_fatal("SEQ", "Randomization failed")
      finish_item(tr);
    end
  endtask
endclass

class read_error_seq extends apb_base_sequence;
  `uvm_object_utils(read_error_seq)
  function new(string name = "read_error_seq");
    super.new(name);
  endfunction
  
  virtual task body();
    apb_transaction tr;
    repeat(10) begin
      tr = apb_transaction::type_id::create("tr");
      start_item(tr);
      if(!tr.randomize() with {op == READ_ERR;}) `uvm_fatal("SEQ", "Randomization failed")
      finish_item(tr);
    end
  endtask
endclass

class reset_seq extends apb_base_sequence;
  `uvm_object_utils(reset_seq)
  function new(string name = "reset_seq");
    super.new(name);
  endfunction
  
  virtual task body();
    apb_transaction tr;
    repeat(5) begin
      tr = apb_transaction::type_id::create("tr");
      start_item(tr);
      if(!tr.randomize() with {op == RESET;}) `uvm_fatal("SEQ", "Randomization failed")
      finish_item(tr);
    end
  endtask
endclass

class random_seq extends apb_base_sequence;
  `uvm_object_utils(random_seq)
  function new(string name = "random_seq");
    super.new(name);
  endfunction
  
  virtual task body();
    apb_transaction tr;
    repeat(30) begin
      tr = apb_transaction::type_id::create("tr");
      start_item(tr);
      if(!tr.randomize()) `uvm_fatal("SEQ", "Randomization failed")
      finish_item(tr);
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
    `uvm_info("SEQ_STRESS", "Starting APB Back-to-Back / Slave Stall Stress Sequence", UVM_LOW)
    
    repeat(40) begin
      tr = apb_transaction::type_id::create("tr");
      start_item(tr);
      
      // תוקן: הוסר תו הבקסלאש הלא חוקי שהיה כאן בתחילת השורה
      if (!tr.randomize() with {
        slave_wait_cycles inside {[5:12]}; 
        op inside {WRITE, READ};
      }) begin
        `uvm_fatal("SEQ", "Randomization failed!")
      end
      
      finish_item(tr);
    end
  endtask
endclass

//===========================================
// Driver Class
//===========================================
class apb_driver extends uvm_driver#(apb_transaction);
  `uvm_component_utils(apb_driver)
  
  virtual apb_if vif;
  
  function new(string name = "apb_driver", uvm_component parent = null);
    super.new(name, parent);
  endfunction
  
  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    if(!uvm_config_db#(virtual apb_if)::get(this, "", "vif", vif))
      `uvm_fatal("DRV", "Unable to access Interface");
  endfunction
  
  task reset_dut();
    vif.presetn <= 1'b0;
    vif.psel    <= 1'b0;
    vif.penable <= 1'b0;
    vif.pwrite  <= 1'b0;
    vif.paddr   <= '0;
    vif.pwdata  <= '0;
    repeat(5) @(posedge vif.pclk);
    vif.presetn <= 1'b1;
    `uvm_info("DRV", "Reset Complete", UVM_MEDIUM);
  endtask
  
  virtual task drive_transaction(apb_transaction tr);
    case(tr.op)
      RESET: begin
        vif.presetn <= 1'b0;
        vif.psel    <= 1'b0;
        vif.penable <= 1'b0;
        @(posedge vif.pclk);
        vif.presetn <= 1'b1;
        `uvm_info("DRV", "RESET operation", UVM_MEDIUM);
      end
      
      WRITE, WRITE_ERR, READ, READ_ERR: begin
        // Setup phase
        @(posedge vif.pclk);
        vif.psel    <= 1'b1;
        vif.penable <= 1'b0;
        vif.pwrite  <= (tr.op == WRITE || tr.op == WRITE_ERR) ? 1'b1 : 1'b0;
        vif.paddr   <= tr.paddr;
        if (vif.pwrite) vif.pwdata <= tr.pwdata;
        
        // Access phase
        @(posedge vif.pclk);
        vif.penable <= 1'b1;
        
        // מנגנון Watchdog להגנה מפני קריסת PREADY
        fork
          begin: wait_pready
            wait(vif.pready == 1'b1);
          end
          begin: timeout_watchdog
            repeat(50) @(posedge vif.pclk);
            `uvm_error("DRV_APB_TIMEOUT", $sformatf("APB Slave hung! PREADY failed to assert at addr=0x%0h", tr.paddr))
          end
        join_any
        disable fork;
        
        // דגימת תגובה (מבוצע מייד כשיש PREADY=1 בסנכרון עם השעון)
        if (!vif.pwrite) tr.prdata = vif.prdata;
        tr.pslverr = vif.pslverr;
        
        // חזרה ל-Idle או מעבר לטרנזקציה הבאה ללא השהייה מיותרת
        vif.psel    <= 1'b0;
        vif.penable <= 1'b0;
      end
    endcase
  endtask
  
  virtual task run_phase(uvm_phase phase);
    reset_dut();
    forever begin
      seq_item_port.get_next_item(req);
      drive_transaction(req);
      seq_item_port.item_done();
    end
  endtask
endclass

//===========================================
// Monitor Class
//===========================================
class apb_monitor extends uvm_monitor;
  `uvm_component_utils(apb_monitor)
  
  virtual apb_if vif;
  uvm_analysis_port#(apb_transaction) mon_ap;
  
  function new(string name = "apb_monitor", uvm_component parent = null);
    super.new(name, parent);
  endfunction
  
  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    mon_ap = new("mon_ap", this);
    if(!uvm_config_db#(virtual apb_if)::get(this, "", "vif", vif))
      `uvm_fatal("MON", "Unable to access Interface");
  endfunction
  
  virtual task run_phase(uvm_phase phase);
    apb_transaction tr;
    
    forever begin
      @(posedge vif.pclk);
      
      if(!vif.presetn) begin
        tr = apb_transaction::type_id::create("tr");
        tr.op = RESET;
        mon_ap.write(tr);
        `uvm_info("MON", "RESET detected", UVM_MEDIUM);
      end
      else if(vif.psel && vif.penable && vif.pready) begin
        tr = apb_transaction::type_id::create("tr");
        tr.paddr   = vif.paddr;
        tr.pwrite  = vif.pwrite;
        tr.pslverr = vif.pslverr;
        
        if(vif.pwrite) begin
          tr.pwdata = vif.pwdata;
          tr.op = (vif.pslverr) ? WRITE_ERR : WRITE;
          `uvm_info("MON", $sformatf("WRITE: addr=0x%0h, data=0x%0h, err=%0b", 
                    tr.paddr, tr.pwdata, tr.pslverr), UVM_HIGH);
        end
        else begin
          tr.prdata = vif.prdata;
          tr.op = (vif.pslverr) ? READ_ERR : READ;
          `uvm_info("MON", $sformatf("READ: addr=0x%0h, data=0x%0h, err=%0b", 
                    tr.paddr, tr.prdata, tr.pslverr), UVM_HIGH);
        end
        
        mon_ap.write(tr);
      end
    end
  endtask
endclass

//===========================================
// Coverage Class
//===========================================
class apb_coverage extends uvm_subscriber#(apb_transaction);
  `uvm_component_utils(apb_coverage)
  
  apb_transaction tr;
  
  covergroup cg_apb_operations;
    option.per_instance = 1;
    
    cp_operation: coverpoint tr.op {
      bins write      = {WRITE};
      bins read       = {READ};
      bins write_err  = {WRITE_ERR};
      bins read_err   = {READ_ERR};
      bins reset      = {RESET};
    }
    
    cp_address: coverpoint tr.paddr {
      bins low_addr   = {[0:7]};
      bins mid_addr   = {[8:23]};
      bins high_addr  = {[24:31]};
      bins error_addr = {[32:$]};
    }
    
    cp_data: coverpoint tr.pwdata {
      bins zero       = {32'h0};
      bins all_ones   = {32'hFFFFFFFF};
      bins pattern1   = {32'hAAAAAAAA};
      bins pattern2   = {32'h55555555};
      bins others     = default;
    }
    
    cp_error: coverpoint tr.pslverr {
      bins no_error = {1'b0};
      bins error    = {1'b1};
    }
    
    cross cp_operation, cp_address {
      ignore_bins reset_cross = binsof(cp_operation) intersect {RESET};
    }
    
    cross cp_operation, cp_error {
      bins write_success = binsof(cp_operation.write) && binsof(cp_error.no_error);
      bins write_fail    = binsof(cp_operation.write_err) && binsof(cp_error.error);
      bins read_success  = binsof(cp_operation.read) && binsof(cp_error.no_error);
      bins read_fail     = binsof(cp_operation.read_err) && binsof(cp_error.error);
    }
  endgroup
  
  covergroup cg_corner_cases;
    option.per_instance = 1;
    
    cp_boundary_addr: coverpoint tr.paddr {
      bins first_addr = {0};
      bins last_valid = {31};
      bins first_invalid = {32};
    }
    
    cp_data_patterns: coverpoint tr.pwdata {
      bins walking_ones[] = {32'h1, 32'h2, 32'h4, 32'h8, 
                             32'h10, 32'h20, 32'h40, 32'h80,
                             32'h100, 32'h200, 32'h400, 32'h800,
                             32'h1000, 32'h2000, 32'h4000, 32'h8000};
    }
  endgroup
  
  function new(string name = "apb_coverage", uvm_component parent = null);
    super.new(name, parent);
    cg_apb_operations = new();
    cg_corner_cases = new();
  endfunction
  
  virtual function void write(apb_transaction t);
    tr = t;
    cg_apb_operations.sample();
    cg_corner_cases.sample();
  endfunction
  
  virtual function void report_phase(uvm_phase phase);
    `uvm_info("COV", $sformatf("APB Operations Coverage: %.2f%%", cg_apb_operations.get_coverage()), UVM_LOW);
    `uvm_info("COV", $sformatf("Corner Cases Coverage: %.2f%%", cg_corner_cases.get_coverage()), UVM_LOW);
  endfunction
endclass

//===========================================
// Checker Class (Scoreboard)
//===========================================
class apb_scoreboard extends uvm_scoreboard;
  `uvm_component_utils(apb_scoreboard)
  
  uvm_analysis_imp#(apb_transaction, apb_scoreboard) sb_imp;
  
  logic [31:0] mem_model [32];
  
  int pass_count = 0;
  int fail_count = 0;
  int total_transactions = 0;
  
  function new(string name = "apb_scoreboard", uvm_component parent = null);
    super.new(name, parent);
  endfunction
  
  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    sb_imp = new("sb_imp", this);
    foreach(mem_model[i]) mem_model[i] = 32'h0;
  endfunction
  
  virtual function void write(apb_transaction tr);
    total_transactions++;
    
    case(tr.op)
      RESET: begin
        foreach(mem_model[i]) mem_model[i] = 32'h0;
        `uvm_info("SCO", "Memory reset", UVM_MEDIUM);
      end
      
      WRITE: begin
        if(tr.pslverr == 1'b0) begin
          if (tr.paddr < 32) begin
            mem_model[tr.paddr] = tr.pwdata;
            pass_count++;
            `uvm_info("SCO", $sformatf("WRITE SUCCESS: addr=0x%0h, data=0x%0h", tr.paddr, tr.pwdata), UVM_HIGH);
          end else begin
            fail_count++;
            `uvm_error("SCO", $sformatf("Bug in Monitor or Driver: WRITE operation marked success for out-of-bound address 0x%0h", tr.paddr));
          end
        end else begin
          fail_count++;
          `uvm_error("SCO", $sformatf("Unexpected WRITE error on valid address: addr=0x%0h", tr.paddr));
        end
      end
      
      READ: begin
        if(tr.pslverr == 1'b0) begin
          if(tr.paddr < 32 && tr.prdata == mem_model[tr.paddr]) begin
            pass_count++;
            `uvm_info("SCO", $sformatf("READ MATCH: addr=0x%0h, data=0x%0h", tr.paddr, tr.prdata), UVM_HIGH);
          end else begin
            fail_count++;
            `uvm_error("SCO", $sformatf("READ MISMATCH: addr=0x%0h, expected=0x%0h, got=0x%0h", tr.paddr, mem_model[tr.paddr], tr.prdata));
          end
        end else begin
          fail_count++;
          `uvm_error("SCO", $sformatf("Unexpected READ error on valid address: addr=0x%0h", tr.paddr));
        end
      end
      
      WRITE_ERR, READ_ERR: begin
        // תוקן: הסקורבורד כעת בודק בצורה אקטיבית אם הכתובת באמת מחוץ לטווח (>=32), כדי לוודא שחזר ה-Error הנכון מה-DUT
        if(tr.paddr >= 32 && tr.pslverr == 1'b1) begin
          pass_count++;
          `uvm_info("SCO", $sformatf("%s expected and received correctly for out-of-bound addr=0x%0h", tr.op.name(), tr.paddr), UVM_HIGH);
        end else begin
          fail_count++;
          `uvm_error("SCO", $sformatf("ERROR checking failed: op=%s, addr=0x%0h, pslverr=%0b (Expected error response for addr>=32)", tr.op.name(), tr.paddr, tr.pslverr));
        end
      end
    endcase
  endfunction
  
  virtual function void report_phase(uvm_phase phase);
    `uvm_info("SCO", "========================================", UVM_LOW);
    `uvm_info("SCO", "      SCOREBOARD FINAL REPORT", UVM_LOW);
    `uvm_info("SCO", "========================================", UVM_LOW);
    `uvm_info("SCO", $sformatf("Total Transactions: %0d", total_transactions), UVM_LOW);
    `uvm_info("SCO", $sformatf("Passed: %0d", pass_count), UVM_LOW);
    `uvm_info("SCO", $sformatf("Failed: %0d", fail_count), UVM_LOW);
    `uvm_info("SCO", "========================================", UVM_LOW);
    
    if(fail_count == 0)
      `uvm_info("SCO", "TEST PASSED!", UVM_LOW)
    else
      `uvm_error("SCO", "TEST FAILED!");
  endfunction
endclass

//===========================================
// Agent Class
//===========================================
class apb_agent extends uvm_agent;
  `uvm_component_utils(apb_agent)
  
  apb_driver                      drv;
  apb_monitor                     mon;
  uvm_sequencer#(apb_transaction) seqr;
  apb_config                      cfg;
  
  function new(string name = "apb_agent", uvm_component parent = null);
    super.new(name, parent);
  endfunction
  
  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    
    if(!uvm_config_db#(apb_config)::get(this, "", "cfg", cfg))
      `uvm_fatal("AGT", "Unable to get configuration");
    
    mon = apb_monitor::type_id::create("mon", this);
    
    if(cfg.is_active == UVM_ACTIVE) begin
      drv  = apb_driver::type_id::create("drv", this);
      seqr = uvm_sequencer#(apb_transaction)::type_id::create("seqr", this);
    end
  endfunction
  
  virtual function void connect_phase(uvm_phase phase);
    super.connect_phase(phase);
    if(cfg.is_active == UVM_ACTIVE)
      drv.seq_item_port.connect(seqr.seq_item_export);
  endfunction
endclass

//===========================================
// Environment Class
//===========================================
class apb_env extends uvm_env;
  `uvm_component_utils(apb_env)
  
  apb_agent      agt;
  apb_scoreboard scb;
  apb_coverage   cov;
  apb_config     cfg;
  
  function new(string name = "apb_env", uvm_component parent = null);
    super.new(name, parent);
  endfunction
  
  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    
    cfg = apb_config::type_id::create("cfg");
    uvm_config_db#(apb_config)::set(this, "*", "cfg", cfg);
    
    agt = apb_agent::type_id::create("agt", this);
    
    if(cfg.has_scoreboard)
      scb = apb_scoreboard::type_id::create("scb", this);
    
    if(cfg.has_coverage)
      cov = apb_coverage::type_id::create("cov", this);
  endfunction
  
  virtual function void connect_phase(uvm_phase phase);
    super.connect_phase(phase);
    
    if(cfg.has_scoreboard)
      agt.mon.mon_ap.connect(scb.sb_imp);
    
    if(cfg.has_coverage)
      agt.mon.mon_ap.connect(cov.analysis_export);
  endfunction
endclass

//===========================================
// Test Library
//===========================================

class apb_base_test extends uvm_test;
  `uvm_component_utils(apb_base_test)
  
  apb_env env;
  
  function new(string name = "apb_base_test", uvm_component parent = null);
    super.new(name, parent);
  endfunction
  
  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    env = apb_env::type_id::create("env", this);
  endfunction
  
  virtual function void end_of_elaboration_phase(uvm_phase phase);
    uvm_top.print_topology();
  endfunction
endclass

class write_read_test extends apb_base_test;
  `uvm_component_utils(write_read_test)
  
  write_read_seq seq;
  
  function new(string name = "write_read_test", uvm_component parent = null);
    super.new(name, parent);
  endfunction
  
  virtual task run_phase(uvm_phase phase);
    seq = write_read_seq::type_id::create("seq");
    phase.raise_objection(this);
    seq.start(env.agt.seqr);
    #100;
    phase.drop_objection(this);
  endtask
endclass

class bulk_test extends apb_base_test;
  `uvm_component_utils(bulk_test)
  
  bulk_write_read_seq seq;
  
  function new(string name = "bulk_test", uvm_component parent = null);
    super.new(name, parent);
  endfunction
  
  virtual task run_phase(uvm_phase phase);
    seq = bulk_write_read_seq::type_id::create("seq");
    phase.raise_objection(this);
    seq.start(env.agt.seqr);
    #100;
    phase.drop_objection(this);
  endtask
endclass

class error_test extends apb_base_test;
  `uvm_component_utils(error_test)
  
  write_error_seq werr_seq;
  read_error_seq  rerr_seq;
  
  function new(string name = "error_test", uvm_component parent = null);
    super.new(name, parent);
  endfunction
  
  virtual task run_phase(uvm_phase phase);
    werr_seq = write_error_seq::type_id::create("werr_seq");
    rerr_seq = read_error_seq::type_id::create("rerr_seq");
    
    phase.raise_objection(this);
    werr_seq.start(env.agt.seqr);
    #50;
    rerr_seq.start(env.agt.seqr);
    #100;
    phase.drop_objection(this);
  endtask
endclass

class random_test extends apb_base_test;
  `uvm_component_utils(random_test)
  
  random_seq seq;
  
  function new(string name = "random_test", uvm_component parent = null);
    super.new(name, parent);
  endfunction
  
  virtual task run_phase(uvm_phase phase);
    seq = random_seq::type_id::create("seq");
    phase.raise_objection(this);
    seq.start(env.agt.seqr);
    #100;
    phase.drop_objection(this);
  endtask
endclass

class apb_qualcomm_stress_test extends apb_base_test;
  `uvm_component_utils(apb_qualcomm_stress_test)
  
  apb_back_to_back_stall_seq stress_seq;
  
  function new(string name = "apb_qualcomm_stress_test", uvm_component parent = null);
    super.new(name, parent);
  endfunction
  
  virtual function void build_phase(uvm_phase phase);
    set_type_override_by_type(apb_base_sequence::get_type(), apb_back_to_back_stall_seq::get_type());
    super.build_phase(phase);
  endfunction
  
  virtual task run_phase(uvm_phase phase);
    stress_seq = apb_back_to_back_stall_seq::type_id::create("stress_seq");
    phase.raise_objection(this);
    stress_seq.start(env.agt.seqr);
    #200;
    phase.drop_objection(this);
  endtask
endclass
