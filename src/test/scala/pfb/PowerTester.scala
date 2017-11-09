package power

import breeze.math.Complex
import chisel3._
import chisel3.iotesters.PeekPokeTester
import dsptools.numbers._
import dsptools.numbers.implicits._
import dspblocks.BlindWrapperModule._
import freechips.rocketchip.amba.axi4stream.AXI4StreamBundlePayload
import freechips.rocketchip.tilelink._ 
import scala.collection._

class PowerDataTester[T<:Data:Ring, V<:Data:Ring](c: TLBlindWrapperModule[Power[T, V]], in_data: Seq[BigInt], 
  out_data: mutable.ArrayBuffer[BigInt]) extends PeekPokeTester(c) with TLMasterModel[TLBlindWrapperModule[Power[T, V]]] {

  val memTL = c.mem(0)
  val in  = c.in(0)
  val out = c.out(0)

  tlReset()
  poke(in.valid, 0)
  reset(4)

  poke(in.valid, 1)
  poke(in.bits.last, 0)
  poke(out.ready, 1)

  in_data.foreach { x =>
    poke(in.valid, 1)
    // TODO tlast, etc.
    poke(in.bits.data, x)
    if (peek(out.valid) != BigInt(0)) {
      out_data.append(peek(out.bits.data))
    }
    step(1)
  }
}
