package power

import chisel3._
import chisel3.experimental.{FixedPoint, dontTouch}
import chisel3.util.{HasBlackBoxResource, _}
import chisel3.core.requireIsChiselType
import chisel3.internal.firrtl.KnownBinaryPoint
import chisel3.util._
import dspblocks._
import dsptools._
import craft._
import dsptools.numbers._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.coreplex.BaseCoreplexConfig
import freechips.rocketchip.diplomacy._

import scala.collection.Seq
import scala.math.{max, min, pow, abs, round}

class Power[T<:Data:Ring, V<:Data:Ring](val config: PowerParams[T, V])
                                (implicit p: Parameters) extends DspBlock[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle]
  with TLDspBlock with TLHasCSR {

  // Add CSRs
  //addControl("ctrl")
  addStatus("stat", 0)

  // identity node means inputs and outputs are the same
  // adapter node means different IO parameters 
  // val streamNode = AXI4StreamIdentityNode() //streamNodeOpt.getOrElse(streamInOpt.get)
  val streamNode = AXI4StreamAdapterNode(
    AXI4StreamAdapterNode.widthAdapter(_, _ + config.bytesAdded),
    {s: AXI4StreamSlavePortParameters => s}
  )

  override val csrBase   : BigInt = BigInt(0)
  override val csrSize   : BigInt = BigInt(8 * csrMap.size)
  override val beatBytes : Int    = 8

  makeCSRs()

  class PlusargReaderHack extends BlackBox with HasBlackBoxResource {
    override def desiredName: String = "plusarg_reader"
    val io = IO(new Bundle {
      val out = Output(UInt(32.W))
    })

    setResource("/plusarg_reader.v")
  }

  lazy val module = new PowerModule(this) {
    val hack = Module(new PlusargReaderHack)
    dontTouch(hack.io.out)
  }
}

class PowerModule[T<:Data:Ring, V<:Data:Ring](outer: Power[T, V]) extends LazyModuleImp(outer) {
  import outer.status

  val (inx, _)  = outer.streamNode.in.unzip
  val (outx, _) = outer.streamNode.out.unzip
  val mem       = outer.mem.map(_.in.map(_._1))
  val config    = outer.config

  // get fields from outer class
  val genInVec     : Vec[DspComplex[T]]   = Vec(config.lanes, config.genInInternal)
  val genOutVec    : Vec[V]   = Vec(config.lanes, config.genOutInternal)

  val csrs = outer.csrs.module.io.csrs

  // cast input to T
  val in               = inx(0)
  val in_data: Vec[DspComplex[T]]  = in.bits.data.asTypeOf(genInVec)
  val out              = outx(0)
  val out_data: Vec[V] = out.bits.data.asTypeOf(genOutVec)

  // take power
  out_data := in_data.map{i => 
    ((i.real * i.real) +
    (i.imag * i.imag)).asInstanceOf[V]
  }

  // output result
  out.bits.data := ShiftRegister(out_data.asUInt, config.pipelineDepth)
  out.bits.last := ShiftRegister(in.bits.last, config.pipelineDepth)
  out.valid := ShiftRegister(in.valid, config.pipelineDepth)

  // because...
  status("stat") := in.valid
}

case class PowerParams[T<:Data:Ring, V<:Data:Ring]
(
  // generic
  genIn: DspComplex[T],
  genOut: V,
  name: String = "power",
  lanes: Int, 
  pipelineDepth: Int = 0
) {
  requireIsChiselType(genIn,  s"genIn ($genIn) must be chisel type")
  requireIsChiselType(genOut, s"genOut ($genOut) must be chisel type")

  val genInInternal = genIn
  val genOutInternal = genOut
  // note this is doubled because in put is complex
  val inWidthBytes = (genInInternal.getWidth*lanes*2 + 7)/ 8
  val outWidthBytes = (genOutInternal.getWidth*lanes + 7)/ 8
  val bytesAdded = outWidthBytes - inWidthBytes
}

