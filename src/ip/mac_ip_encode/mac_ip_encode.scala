// package ip
package network.ip.mac_ip_encode
import common.Math
import chisel3._
import chisel3.util._
import common.axi._
import common.storage._
import common._
import network.ip.util._


class mac_ip_encode extends Module{
    val io = IO(new Bundle{
        val data_in          =   Flipped(Decoupled(new AXIS(512)))
        val data_out         =   Decoupled(new AXIS(512))
        val arp_tablein      =   Flipped(Decoupled(new mac_out))//Input(new mac_out)
        val regdefaultgateway=   Input(UInt(32.W))
        val regsubnetmask    =   Input(UInt(32.W))
        val arp_tableout     =   Decoupled(UInt(32.W))//Output(UInt(32.W))
        val mac_address     =   Input(UInt(48.W))
	})
    val data_in_fifo        =   XQueue(new AXIS(512),512)
    val ex_ipaddress1       =  Module(new ex_ipaddress())
    val compute_checksum11  =  Module(new compute_checksum1())
    val insert_ip_checksum1 =  Module(new insert_ip_checksum()) 
    val handle_arp_reply1   =  Module(new handle_arp_reply())
    val lshift              =  Module(new LSHIFT(14,512))
    val insert_eth_header1  =  Module(new insert_eth_header())
    
    data_in_fifo.io.in                  <> io.data_in
    ex_ipaddress1.io.data_in            <> data_in_fifo.io.out
    ex_ipaddress1.io.regsubnetmask      := io.regsubnetmask
    ex_ipaddress1.io.regdefaultgateway  := io.regdefaultgateway
    io.arp_tableout                     <> ex_ipaddress1.io.arptableout
    // io.arp_tableout.valid               := ex_ipaddress1.io.arptableout.valid


    

    compute_checksum11.io.data_in       <> ex_ipaddress1.io.data_out
    
    insert_ip_checksum1.io.data_in      <> compute_checksum11.io.data_out
    insert_ip_checksum1.io.ip_checksum  := compute_checksum11.io.checksum

    val q0 = XQueue(new AXIS(512),32)
    q0.io.in                            <> insert_ip_checksum1.io.data_out
    val last        = RegInit(UInt(1.W), 1.U)
    when(last === 1.U){
        q0.io.out.ready                 := handle_arp_reply1.io.data_in.ready & io.arp_tablein.fire()
        when(q0.io.out.fire()){
            last                        := q0.io.out.bits.last
        }
    }.otherwise{
        q0.io.out.ready                 := handle_arp_reply1.io.data_in.ready
        when(q0.io.out.bits.last === 1.U){
            last                        := 1.U
        }
    }
    
     
    io.arp_tablein.ready                := handle_arp_reply1.io.data_in.ready
    handle_arp_reply1.io.data_in.bits   := q0.io.out.bits
    handle_arp_reply1.io.data_in.valid  := q0.io.out.fire()
    handle_arp_reply1.io.mymac          := io.mac_address
    handle_arp_reply1.io.arptablein     := io.arp_tablein.bits
    
    
    val q = XQueue(UInt(112.W),32)
    q.io.in.bits                        := handle_arp_reply1.io.ethheaderout
    q.io.in.valid                       := handle_arp_reply1.io.data_out.valid
    q.io.out.ready                      := lshift.io.out.fire()
    //handle_arp_reply1.io.data_out.fire()

    lshift.io.in                        <> handle_arp_reply1.io.data_out
    insert_eth_header1.io.data_in       <> lshift.io.out
    insert_eth_header1.io.header_in     := q.io.out.bits
    io.data_out                         <> insert_eth_header1.io.data_out
    
}