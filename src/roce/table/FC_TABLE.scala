package network.roce.table

import common.storage._
import common._
import common.ToZero
import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum
import network.roce.util._

class FC_TABLE() extends Module{
	val io = IO(new Bundle{
		val rx2fc_req  	= Flipped(Decoupled(new IBH_META()))

		val tx2fc_req	= Flipped(Decoupled(new IBH_META()))
        val tx2fc_ack	= Flipped(Decoupled(new IBH_META()))
		val fc_init	    = Flipped(Decoupled(new FC_REQ()))
        val buffer_cnt	= Input(UInt(16.W))
        val ack_event   = (Decoupled(new IBH_META()))
		val fc2tx_rsp	= (Decoupled(new IBH_META()))
	})

    val ack_event_fifo = XQueue(new IBH_META(), entries=16)
    val tx_rsp_fifo = XQueue(new IBH_META(), entries=16)
    

    io.ack_event                        <> ack_event_fifo.io.out
    io.fc2tx_rsp                        <> tx_rsp_fifo.io.out


       

    val fc_table = XRam(new FC_STATE(), CONFIG.MAX_QPS, latency = 1)	

// RegInit(VecInit(Seq.fill(MAX_Q)(0.U(7.W))))


    val fc_request = Reg(Vec(2,(new IBH_META())))

    val tmq_req = Reg(Vec(4,(new IBH_META())))
    val tmp_bitmap = RegInit(0.U(4.W))
    val c_tmp_id = RegInit(0.U(2.W))
    val n_tmp_id = RegInit(0.U(2.W))
    val tx_wait = RegInit(0.U(2.W))


    // val rx_fc_request = RegInit(0.U.asTypeOf(new FC_REQ()))
    // val rx_tmp_credit = RegInit(0.U.asTypeOf(new FC_STATE()))
    val tmp_credit = RegInit(0.U.asTypeOf(new FC_STATE()))
    val tmp_request = RegInit(0.U.asTypeOf(new IBH_META()))
    val tx_forward = Wire(Bool())
    tx_forward := false.B
    val tx_event_lock = RegInit(0.B)


	val sIDLE :: sINIT :: sTXACK :: sTXRSP0 :: sTXRSP1 :: sTXRSP2 :: sRXRSP :: Nil = Enum(7)
	val state0                  = RegInit(sIDLE)
    val state1                  = RegInit(sIDLE)
    val state2                  = RegInit(sIDLE)
    // Collector.report(state0===sIDLE, "FC_TABLE===sIDLE") 
    fc_table.io.addr_a                 := 0.U
    fc_table.io.addr_b                 := 0.U
    fc_table.io.wr_en_a                := 0.U
    fc_table.io.data_in_a              := 0.U.asTypeOf(fc_table.io.data_in_a)

    
    io.fc_init.ready                    := 1.U
    io.rx2fc_req.ready                  := (!io.fc_init.valid.asBool)
    io.tx2fc_ack.ready                  := (!ack_event_fifo.io.almostfull) & (!tx_rsp_fifo.io.almostfull) & (!io.fc_init.valid.asBool) & (!io.rx2fc_req.valid.asBool)
    io.tx2fc_req.ready                  := (!ack_event_fifo.io.almostfull) & (!tx_rsp_fifo.io.almostfull) & (!io.fc_init.valid.asBool) & (!io.rx2fc_req.valid.asBool) & (!io.tx2fc_ack.valid.asBool) & (c_tmp_id === n_tmp_id)

 
    ToZero(ack_event_fifo.io.in.valid)
    ToZero(ack_event_fifo.io.in.bits)
    ToZero(tx_rsp_fifo.io.in.valid)
    ToZero(tx_rsp_fifo.io.in.bits)



    //cycle 1
    when(io.fc_init.fire()){
        fc_request(0).qpn                   := io.fc_init.bits.qpn
        fc_request(0).credit                := io.fc_init.bits.credit
        state0                              := sINIT
    }.elsewhen(io.rx2fc_req.fire()){
        fc_request(0)                       := io.rx2fc_req.bits
        fc_table.io.addr_b                  := io.rx2fc_req.bits.qpn
        state0                              := sRXRSP                      
    }.elsewhen(io.tx2fc_ack.fire()){
        fc_request(0)                       := io.tx2fc_ack.bits
        fc_table.io.addr_b                  := io.tx2fc_ack.bits.qpn
        state0                              := sTXACK
    }.elsewhen(c_tmp_id =/= n_tmp_id){
        fc_request(0)                       := tmq_req(c_tmp_id)
        fc_table.io.addr_b                  := tmq_req(c_tmp_id).qpn
        when(tx_wait === 0.U){
            state0                              := sTXRSP2
        }.otherwise{
            state0                              := sIDLE
        }
        tx_wait                                := tx_wait + 1.U
    }.elsewhen(io.tx2fc_req.fire()){
        fc_request(0)                       <> io.tx2fc_req.bits
        fc_table.io.addr_b                  := io.tx2fc_req.bits.qpn
        when(PKG_JUDGE.HAVE_DATA(io.tx2fc_req.bits.op_code)){
            state0                          := sTXRSP1
        }.otherwise{
            state0                          := sTXRSP0
        }
    }.otherwise{
        state0                              := sIDLE
    }

    //cycle 2

    when(state0 === sINIT){
        fc_request(1)                   := fc_request(0)
        state1                          := sINIT
    }.otherwise{
        fc_request(1)                   := fc_request(0)
        when(fc_request(1).qpn === fc_request(0).qpn){  
            when((fc_request(1).op_code === IB_OP_CODE.RC_ACK)&(state1 === sRXRSP)){
                tmp_credit.credit       := tmp_credit.credit + fc_request(1).credit
            }.elsewhen(tx_forward){
                when((fc_request(1).op_code === IB_OP_CODE.RC_WRITE_FIRST) || (fc_request(1).op_code === IB_OP_CODE.RC_DIRECT_FIRST)){
                    tmp_credit.credit       := tmp_credit.credit + CONFIG.MTU_WORD.U
                }.otherwise{
                    tmp_credit.credit       := tmp_credit.credit - fc_request(1).credit
                }
            }.otherwise{
                tmp_credit.credit           := fc_table.io.data_out_b.credit
            }
        }.otherwise{
            tmp_credit.credit           := fc_table.io.data_out_b.credit
        }
        state1                          := state0
    }

    

    //cycle 3

    when(state1 === sINIT){
        fc_table.io.addr_a              := fc_request(1).qpn
        fc_table.io.wr_en_a             := 1.U
        fc_table.io.data_in_a.credit    := fc_request(1).credit
    }.elsewhen(state1 === sRXRSP){
        when(fc_request(1).op_code === IB_OP_CODE.RC_ACK){
            fc_table.io.addr_a              := fc_request(1).qpn
            fc_table.io.wr_en_a             := 1.U
            fc_table.io.data_in_a.credit    := tmp_credit.credit + fc_request(1).credit
        }
    }.elsewhen(state1 === sTXACK){
        when(io.buffer_cnt < CONFIG.RX_BUFFER_FULL.U){
            tx_rsp_fifo.io.in.valid      := 1.U
            tx_rsp_fifo.io.in.bits              := fc_request(1)
            tx_rsp_fifo.io.in.bits.valid_event:= true.B
        }.otherwise{
		    tx_rsp_fifo.io.in.valid 		        := 1.U 
            tx_rsp_fifo.io.in.bits.valid_event 	:= false.B 
			ack_event_fifo.io.in.valid 		        := 1.U 
			ack_event_fifo.io.in.bits.ack_event(fc_request(1).qpn, fc_request(1).credit, fc_request(1).psn, fc_request(1).is_wr_ack)
        }
    }.elsewhen(state1 === sTXRSP0){
        tx_rsp_fifo.io.in.valid             := 1.U
        tx_rsp_fifo.io.in.bits              := fc_request(1)
        tx_rsp_fifo.io.in.bits.valid_event  := true.B
    }.elsewhen(state1 === sTXRSP1){
        when(tmp_credit.credit >= fc_request(1).credit){
			tx_rsp_fifo.io.in.valid 		        := 1.U 
            tx_rsp_fifo.io.in.bits              := fc_request(1)
            tx_rsp_fifo.io.in.bits.valid_event 	:= true.B 
            tx_forward                      := true.B
            when((fc_request(1).op_code === IB_OP_CODE.RC_WRITE_FIRST) || (fc_request(1).op_code === IB_OP_CODE.RC_DIRECT_FIRST)){
                fc_table.io.addr_a              := fc_request(1).qpn
                fc_table.io.wr_en_a             := 1.U
                fc_table.io.data_in_a.credit    := tmp_credit.credit - CONFIG.MTU_WORD.U                      
            }.otherwise{
                fc_table.io.addr_a              := fc_request(1).qpn
                fc_table.io.wr_en_a             := 1.U
                fc_table.io.data_in_a.credit    := tmp_credit.credit  - fc_request(1).credit                     
            }
        }.otherwise{
            tmq_req(n_tmp_id)               := fc_request(1)
            n_tmp_id                        := n_tmp_id + 1.U
        }
    }.elsewhen(state1 === sTXRSP2){
        when(tmp_credit.credit >= fc_request(1).credit){
			tx_rsp_fifo.io.in.valid 		        := 1.U 
            tx_rsp_fifo.io.in.bits              := fc_request(1)
            tx_rsp_fifo.io.in.bits.valid_event 	:= true.B 
            tx_forward                      := true.B
            c_tmp_id                        := c_tmp_id + 1.U
            when(fc_request(1).op_code === IB_OP_CODE.RC_WRITE_FIRST){
                fc_table.io.addr_a              := fc_request(1).qpn
                fc_table.io.wr_en_a             := 1.U
                fc_table.io.data_in_a.credit    := tmp_credit.credit - CONFIG.MTU_WORD.U                      
            }.otherwise{
                fc_table.io.addr_a              := fc_request(1).qpn
                fc_table.io.wr_en_a             := 1.U
                fc_table.io.data_in_a.credit    := tmp_credit.credit  - fc_request(1).credit                     
            }
        }
    }




	// switch(state){
	// 	is(sIDLE){
    //         when(fc_init_fifo.io.deq.fire()){
    //             fc_table.io.addr_a              := fc_init_fifo.io.deq.bits.qpn
    //             fc_table.io.wr_en_a             := 1.U
    //             fc_table.io.data_in_a.credit    := fc_init_fifo.io.deq.bits.credit
    //             state                           := sIDLE
    //         }.elsewhen(fc_rx_fifo.io.deq.fire()){
    //             rx_fc_request                      <> fc_rx_fifo.io.deq.bits
    //             when(fc_rx_fifo.io.deq.bits.op_code === IB_OP_CODE.RC_ACK){
    //                 fc_table.io.addr_b              := fc_rx_fifo.io.deq.bits.qpn
    //                 state                           := sRXRSP                      
    //             }
    //         }.elsewhen(fc_txack_fifo.io.deq.fire()){
    //             tx_fc_request                   <> fc_txack_fifo.io.deq.bits
    //             fc_table.io.addr_b              := fc_txack_fifo.io.deq.bits.qpn
    //             when(io.buffer_cnt < CONFIG.RX_BUFFER_FULL.U){
    //                 state                   := sTXRSP1
    //             }.otherwise{
    //                 state                   := sTXRSP2
    //             }
    //         }.elsewhen(tx_event_lock){
    //             fc_table.io.addr_b              := tmp_request.qpn
    //             state                           := sTXRSP3
    //         }.elsewhen(fc_tx_fifo.io.deq.fire()){
    //             tx_fc_request                   <> fc_tx_fifo.io.deq.bits
    //             fc_table.io.addr_b              := fc_tx_fifo.io.deq.bits.qpn
    //             when(PKG_JUDGE.HAVE_DATA(fc_tx_fifo.io.deq.bits.op_code)){
    //                 state                       := sTXRSP3
    //             }.otherwise{
    //                 state                       := sTXRSP1
    //             }
    //         }.otherwise{
    //             state                           := sIDLE
    //         }
	// 	}
	// 	is(sTXRSP1){
	// 		when(io.fc2tx_rsp.ready){
	// 			io.fc2tx_rsp.valid 		        := 1.U 
    //             io.fc2tx_rsp.bits.valid_event 	:= true.B 
    //             state                           := sIDLE
	// 		}.otherwise{
    //             state                           := sTXRSP1
    //         }
	// 	}
	// 	is(sTXRSP2){
	// 		when(io.fc2tx_rsp.ready & io.ack_event.ready){
	// 			io.fc2tx_rsp.valid 		        := 1.U 
    //             io.fc2tx_rsp.bits.valid_event 	:= false.B 
	// 			io.ack_event.valid 		        := 1.U 
	// 			io.ack_event.bits.ack_event(tx_fc_request.qpn, tx_fc_request.credit, tx_fc_request.psn, tx_fc_request.is_wr_ack)
    //             state                           := sIDLE
	// 		}.otherwise{
    //             state                           := sTXRSP2
    //         }
	// 	}
	// 	is(sTXRSP3){
    //         tmp_credit.credit               := fc_table.io.data_out_b.credit
    //         tmp_request                     := tx_fc_request
    //         tx_event_lock                   := true.B
    //         state                           := sTXRSP4
	// 	}       
	// 	is(sTXRSP4){
	// 		when(io.fc2tx_rsp.ready & (tmp_credit.credit >= tmp_request.credit)){
	// 			io.fc2tx_rsp.valid 		        := 1.U 
    //             io.fc2tx_rsp.bits.valid_event 	:= true.B 
    //             state                           := sIDLE
    //             tx_event_lock                   := false.B
    //             when(tmp_request.op_code === IB_OP_CODE.RC_WRITE_FIRST){
    //                 fc_table.io.addr_a              := tmp_request.qpn
    //                 fc_table.io.wr_en_a             := 1.U
    //                 fc_table.io.data_in_a.credit    := tmp_credit.credit - CONFIG.MTU_WORD.U                      
    //             }.otherwise{
    //                 fc_table.io.addr_a              := tmp_request.qpn
    //                 fc_table.io.wr_en_a             := 1.U
    //                 fc_table.io.data_in_a.credit    := tmp_credit.credit  - tmp_request.credit                     
    //             }
	// 		}.otherwise{
    //             tx_event_lock                   := true.B
    //             state                           := sIDLE
    //         }
	// 	}                  
	// 	is(sRXRSP){
    //         rx_tmp_credit.credit            := fc_table.io.data_out_b.credit
    //         state                           := sRXRSP1			
	// 	}
	// 	is(sRXRSP1){
    //         fc_table.io.addr_a              := rx_fc_request.qpn
    //         fc_table.io.wr_en_a             := 1.U
    //         fc_table.io.data_in_a.credit    := rx_tmp_credit.credit + rx_fc_request.credit
    //         state                           := sIDLE			
	// 	}        	
	// }
    

}