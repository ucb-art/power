// See LICENSE for license details.

package pfb

/**
  * See [[https://en.wikipedia.org/wiki/Window_function#Blackman.E2.80.93Harris_window the wikipedia page]] for
  * more details
  */
object blackmanHarris {
  private val a0 = 0.35875
  private val a1 = 0.48829
  private val a2 = 0.14128
  private val a3 = 0.01168
  def apply(N: Int): Seq[Double] = Seq.tabulate(N) (i => {
    (a0 -
      a1 * math.cos(2 * math.Pi * i.toDouble / (N - 1)) +
      a2 * math.cos(4 * math.Pi * i.toDouble / (N - 1)) -
      a3 * math.cos(6 * math.Pi * i.toDouble / (N - 1)))
  })
  def apply(w: WindowConfig): Seq[Double] = blackmanHarris(w.outputWindowSize * w.numTaps)
}

object sincHanning {
  def apply(size: Int, nfft: Int): Seq[Double] = Seq.tabulate(size) (i=>{
    val hanning = 0.5 * (1 - breeze.numerics.cos(2 * scala.math.Pi * i.toDouble / (size-1)))
    val periods = 1.0 // should be a double
    val sinc = breeze.numerics.sincpi((i.toDouble*periods-0.5*periods*size.toDouble)/nfft.toDouble)
    (hanning * sinc)
  })
  def apply(w: WindowConfig): Seq[Double] = sincHanning(w.outputWindowSize * w.numTaps, w.outputWindowSize)
}

object sincHamming {
  def apply(size: Int, nfft: Int): Seq[Double] = Seq.tabulate(size) (i=>{
    val hamming = 0.54 - 0.46 * breeze.numerics.cos(2 * scala.math.Pi * i.toDouble / (size-1))
    val periods = 1.0 // should be a double
    val sinc = breeze.numerics.sincpi((i.toDouble*periods-0.5*periods*size.toDouble)/nfft.toDouble)
    (hamming * sinc)
  })
  def apply(w: WindowConfig): Seq[Double] = sincHamming(w.outputWindowSize * w.numTaps, w.outputWindowSize)
}

// all 1s
object onesWindow {
  def apply(w: WindowConfig): Seq[Double] = Seq.fill(w.outputWindowSize * w.numTaps)(1.0)
}
