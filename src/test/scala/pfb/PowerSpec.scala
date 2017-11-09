package power

import chisel3._
import chisel3.core.FixedPoint
import chisel3.internal.firrtl.Width
import chisel3.iotesters._
import chisel3.util._
import dspblocks._
import dsptools.numbers.{Ring, DspReal, DspComplex}
//import dsptools.numbers.implicits._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.coreplex.BaseCoreplexConfig
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.regmapper._
import org.scalatest.{FlatSpec, Matchers}
import scala.collection.{Seq, mutable}

import breeze.linalg._
import breeze.signal.fourierTr
import breeze.math.Complex
import chisel3.internal.firrtl.KnownBinaryPoint
import dsptools.DspTesterUtilities._
import dsptools.{DspTester, DspException}
import scala.math.{abs, pow, max, log10}




class PowerSpec extends FlatSpec with Matchers with AXI4StreamPackers {
  implicit val p: Parameters = Parameters.root((new BaseCoreplexConfig).toInstance)

  def runTest[T<:Data:Ring, V<:Data:Ring](params: PowerParams[T, V], 
                                in: Seq[BigInt]): Seq[BigInt] = {
    val out = new mutable.ArrayBuffer[BigInt]()
    val blindNodes = DspBlockBlindNodes.apply(
      AXI4StreamBundleParameters(
        n = params.inWidthBytes,
        i = 1,
        d = 1,
        u = 1,
        hasData = true,
        hasStrb = false,
        hasKeep = false
      ),
      () => TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters("power")))))
    )

    val dut = () => LazyModule(DspBlock.blindWrapper(() => new Power(params), blindNodes)).module

    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), dut) {
      c => new PowerDataTester(c, in, out)
    }

    //println(chisel3.Driver.emit(dut))

    out
  }




  //////////////////////////////////////////
  /////// BASIC COMIPLATION TESTS //////////
  //////////////////////////////////////////

  def paramTest[T<:Data:Ring, V<:Data:Ring](
    genIn: DspComplex[T] = DspComplex(SInt(4.W), SInt(4.W)), 
    genOut: V = SInt(4.W),
    lanes: Int = 2,
    pipelineDepth: Int = 0
  ): Unit = {

    it should s"compile with the following parameters:\n" + 
      s"\t input type ${genIn} and width ${genIn.getWidth}\n" +
      s"\t output type ${genOut} and width ${genOut.getWidth}\n" +
      s"\t lanes ${lanes} and pipeline ${pipelineDepth}" in {
      val params = PowerParams(
        genIn = genIn,
        genOut = genOut,
        lanes = lanes,
        pipelineDepth = pipelineDepth
      )

      // setup fake test data and run test
      val values = packInputStream(Seq.fill(10)(Seq.fill(params.lanes)(Complex(0.0, 0.0))), params.genInInternal)
      val out = runTest(params, values)
      //println(unpackOutputStream(params.genOutInternal, params.lanes, out))
    }
  }

  paramTest()
  paramTest(genIn = DspComplex(FixedPoint(16.W, 8.BP), FixedPoint(16.W, 8.BP)), genOut = FixedPoint(18.W, 8.BP))
  paramTest(genIn = DspComplex(FixedPoint(8.W, 16.BP), FixedPoint(8.W, 16.BP)), genOut = FixedPoint(4.W, 16.BP))
  paramTest(pipelineDepth = 11)
  paramTest(lanes = 12)

}
