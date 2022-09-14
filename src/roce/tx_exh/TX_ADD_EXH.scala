package network.roce.tx_exh

import common.storage._
import common.axi._
import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum
import network.roce.util._
import common.BaseILA
import common.Collector



class TX_ADD_EXH() extends Module{
	val io = IO(new Bundle{
		val pkg_info  		= Flipped(Decoupled(new TX_PKG_INFO()))

		val header_data_in  = Flipped(Decoupled(new AXIS(CONFIG.DATA_WIDTH)))
		val reth_data_in	= Flipped(Decoupled(new AXIS(CONFIG.DATA_WIDTH)))
		val aeth_data_in	= Flipped(Decoupled(new AXIS(CONFIG.DATA_WIDTH)))
		val raw_data_in	    = Flipped(Decoupled(new AXIS(CONFIG.DATA_WIDTH)))
        val tx_data_out	    = (Decoupled(new AXIS(CONFIG.DATA_WIDTH)))
	})

	val pkg_info_fifo = Module(new Queue(new TX_PKG_INFO(),4))
	val header_fifo = Module(new Queue(new AXIS(CONFIG.DATA_WIDTH),4))
	val reth_fifo = Module(new Queue(new AXIS(CONFIG.DATA_WIDTH),16))
	val aeth_fifo = Module(new Queue(new AXIS(CONFIG.DATA_WIDTH),16))
	val raw_fifo = Module(new Queue(new AXIS(CONFIG.DATA_WIDTH),16))

	io.pkg_info 		<> pkg_info_fifo.io.enq
	io.header_data_in 	<> header_fifo.io.enq
	io.reth_data_in 	<> reth_fifo.io.enq
	io.aeth_data_in 	<> aeth_fifo.io.enq
	io.raw_data_in 		<> raw_fifo.io.enq


    val write_first = RegInit(Bool(),true.B)
	val pkg_info = RegInit(0.U.asTypeOf(new TX_PKG_INFO()))
	val sIDLE :: sHEADER :: sAETH :: sRETH :: sRAW :: Nil = Enum(5)
	val state                   = RegInit(sIDLE)	
	Collector.report(state===sIDLE, "TX_ADD_EXH===sIDLE")  
    val curr_word               = RegInit(0.U.asTypeOf(new AXIS(CONFIG.DATA_WIDTH)))
	
	pkg_info_fifo.io.deq.ready           := (state === sIDLE)

	header_fifo.io.deq.ready     := ((state === sHEADER) ||((state === sRETH)&write_first)||((state === sAETH)&write_first)) & io.tx_data_out.ready
    reth_fifo.io.deq.ready       := (state === sRETH) & io.tx_data_out.ready
    aeth_fifo.io.deq.ready       := (state === sAETH) & io.tx_data_out.ready
    raw_fifo.io.deq.ready        := (state === sRAW) & io.tx_data_out.ready


	io.tx_data_out.valid 			:= 0.U 
	io.tx_data_out.bits.data 		:= 0.U
	io.tx_data_out.bits.keep 		:= 0.U
	io.tx_data_out.bits.last 		:= 0.U	
	
	switch(state){
		is(sIDLE){
			when(pkg_info_fifo.io.deq.fire()){
                write_first := true.B
				pkg_info	:= pkg_info_fifo.io.deq.bits
				when(pkg_info_fifo.io.deq.bits.hasHeader){
					when(!pkg_info_fifo.io.deq.bits.hasPayload){
						state						:= sHEADER
					}.otherwise{
						when(pkg_info_fifo.io.deq.bits.isAETH){
							state                   := sAETH
						}.otherwise{
							state                   := sRETH
						}
					}
				}.otherwise{
					state	:= sRAW
				}
			}
		}
		is(sHEADER){
			when(header_fifo.io.deq.fire()){
                io.tx_data_out.valid 		:= 1.U
				io.tx_data_out.bits.last 	:= 1.U
                io.tx_data_out.bits.data 	:= header_fifo.io.deq.bits.data
	            io.tx_data_out.bits.keep 	:= header_fifo.io.deq.bits.keep
				state						:= sIDLE
			}
		}
		is(sAETH){
			when(aeth_fifo.io.deq.fire()& (header_fifo.io.deq.fire() || ~write_first)){
                io.tx_data_out.valid 		:= 1.U
				io.tx_data_out.bits 		<> aeth_fifo.io.deq.bits                
				when(write_first){
                    io.tx_data_out.bits.data    := Cat(aeth_fifo.io.deq.bits.data(CONFIG.DATA_WIDTH-1,CONFIG.AETH_HEADER_LEN),header_fifo.io.deq.bits.data(CONFIG.AETH_HEADER_LEN-1,0))
					write_first					:= false.B
				}               
                when(aeth_fifo.io.deq.bits.last === 1.U){
                    state                       := sIDLE
                }
			}
		}
		is(sRETH){
			when(reth_fifo.io.deq.fire()& (header_fifo.io.deq.fire() || ~write_first)){
                io.tx_data_out.valid 		:= 1.U
				io.tx_data_out.bits			<> reth_fifo.io.deq.bits               
				when(write_first){                  
                    io.tx_data_out.bits.data 	:= Cat(reth_fifo.io.deq.bits.data(CONFIG.DATA_WIDTH-1,CONFIG.RETH_HEADER_LEN),header_fifo.io.deq.bits.data(CONFIG.RETH_HEADER_LEN-1,0))
					write_first					:= false.B
				}               
                when(reth_fifo.io.deq.bits.last === 1.U){
                    state                       := sIDLE
                }
			}			
		}
		is(sRAW){
			when(raw_fifo.io.deq.fire()){
                io.tx_data_out.valid 		:= 1.U
				io.tx_data_out.bits 		<> raw_fifo.io.deq.bits                                
                when(raw_fifo.io.deq.bits.last === 1.U){
                    state                       := sIDLE
                }
			}			
		}		
	}

}