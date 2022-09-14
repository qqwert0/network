// package ip
package network.ip.mac_ip_encode
import common.Math
import chisel3._
import chisel3.util._
import common.axi._
import common.storage._
import common._
import network.ip.util._

class insert_eth_header extends Module{
    val io = IO(new Bundle{
        val data_in          =   Flipped(Decoupled(new AXIS(512)))
        val header_in        =   Input(UInt(112.W))
        val data_out         =   Decoupled(new AXIS(512))
	})

    val last        = RegInit(UInt(1.W), 1.U)
    
    io.data_in      <> io.data_out

    when(io.data_in.fire()){
        when(last === 1.U){
            io.data_out.bits.data      := Cat(io.data_in.bits.data(511,112),io.header_in)
            last            := 0.U
        }
        
        when(io.data_in.bits.last === 1.U){
            last := 1.U
        }
    }
    

    
}