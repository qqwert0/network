// package ip
package network.ip.mac_ip_encode
import common.Math
import chisel3._
import chisel3.util._
import common.axi._
import common.storage._
import common._
import network.ip.util._

class handle_arp_reply extends Module{
    val io = IO(new Bundle{
        val data_in          =   Flipped(Decoupled(new AXIS(512)))
        val arptablein       =   Input(new mac_out)
        val mymac            =   Input(UInt(48.W))
        val data_out         =   Decoupled(new AXIS(512))
        val ethheaderout     =   Output(UInt(112.W))
	})

    val last        = RegInit(UInt(1.W), 1.U)
    val hit         = RegInit(UInt(1.W), 1.U)
    val temp        = RegInit(UInt(16.W), 0x0008.U)
    io.data_in      <> io.data_out
    io.ethheaderout := Cat(Cat(temp, io.mymac),io.arptablein.mac_addr)
    // d 47:0 s 95:48 t 111,96
    when(io.data_in.fire()){
        when(last === 1.U){
            last            := 0.U
            when(io.arptablein.hit === 1.U){
                io.data_out.valid := 1.U
                hit         := 1.U
            }.otherwise{
                io.data_out.valid := 0.U
                hit         := 0.U
            }
        }.otherwise{
            when(hit === 1.U){
                io.data_out.valid := 1.U
            }.otherwise{
                io.data_out.valid := 0.U
            }
        }
        
        when(io.data_in.bits.last === 1.U){
            last := 1.U
        }
    }
    

    
}