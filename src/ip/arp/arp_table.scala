package network.ip.arp
import common.Math
import chisel3._
import chisel3.util._
import common.axi._
import common.storage._
import chisel3.experimental.{DataMirror, Direction, requireIsChiselType}
import network.ip.util._

class arp_table extends Module{
    val io = IO(new Bundle{
        val arpinsert        =   Flipped(Decoupled(UInt(81.W)))
        val arp_req1         =   Flipped(Decoupled(UInt(32.W)))
        val arp_req2         =   Flipped(Decoupled(UInt(32.W)))
        val arp_rsp1         =   Decoupled(new mac_out)
        val arp_rsp2         =   Decoupled(new mac_out)
        val requestmeta      =   Decoupled(UInt(32.W))
        val mymac            =   Input(UInt(48.W))
        val myip             =   Input(UInt(32.W))
	})
    
    val arp_table           =   XRam(UInt(81.W), 256, "auto", 1, 0, "none")
    val currRntrya          =   Wire(UInt(81.W))
    val currRntryb          =   Wire(UInt(81.W))
    val arp_table_write_data=   Wire(UInt(81.W))
    arp_table.io.data_in_a  :=  arp_table_write_data
    currRntrya              :=  arp_table.io.data_out_a
    currRntryb              :=  arp_table.io.data_out_b

    val full                = RegInit(UInt(2.W), 0.U)
    val valid1              = RegInit(UInt(1.W), 0.U)
    val valid2              = RegInit(UInt(1.W), 0.U)
    val temp                = RegInit(UInt(32.W), 0.U)
    val reqip               = RegInit(UInt(32.W), 0.U)
    io.arpinsert.ready      := 1.U
    io.arp_req1.ready       := 1.U
    io.arp_req2.ready       := 1.U
    io.arp_rsp1.valid       := valid1
    io.arp_rsp1.bits.mac_addr        := currRntryb(79,32)
    io.arp_rsp1.bits.hit             := currRntryb(80) && (currRntryb(31,0) === temp) 
    io.arp_rsp2.valid       := valid2
    io.arp_rsp2.bits.mac_addr        := currRntryb(79,32)
    io.arp_rsp2.bits.hit             := currRntryb(80) && (currRntryb(31,0) === temp)
    io.requestmeta.valid    := (valid1 & !io.arp_rsp1.bits.hit) | (valid2 & !io.arp_rsp2.bits.hit)
    io.requestmeta.bits     := temp

    arp_table.io.wr_en_a    := 0.U

    arp_table.io.addr_a     := io.arpinsert.bits(31,24)
    arp_table_write_data     := io.arpinsert.bits
    arp_table.io.addr_b     := temp(31,24)
    when(io.arpinsert.fire()){
        arp_table_write_data    := io.arpinsert.bits
        arp_table.io.wr_en_a    := 1.U
        arp_table.io.addr_a     := io.arpinsert.bits(31,24)
        io.arp_req1.ready       := 0.U
        io.arp_req2.ready       := 0.U
    }.elsewhen(io.arp_req1.fire()){
        io.arp_req2.ready       := 0.U
        arp_table.io.addr_b     := io.arp_req1.bits(31,24)
        full                    := 1.U
        valid1                  := 1.U
        temp                    := Cat(io.arp_req1.bits(31,24),io.arp_req1.bits(23,0))
    }.elsewhen(io.arp_req2.fire()){
        arp_table.io.addr_b     := io.arp_req2.bits(31,24)
        full                    := 1.U
        valid2                  := 1.U
        temp                    := Cat(io.arp_req2.bits(31,24),io.arp_req2.bits(23,0))
    }

    when(io.arp_rsp1.fire() || io.arp_rsp2.fire()){
        full := 0.U
        when(io.arp_rsp1.fire()){
            valid1 := 0.U
        }
        when(io.arp_rsp2.fire()){
            valid2 := 0.U
        }
    }
    when(full === 1.U && !io.arp_rsp1.fire() && !io.arp_rsp2.fire()){
        io.arp_req1.ready   := 0.U
        io.arp_req2.ready   := 0.U
    }
}