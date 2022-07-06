package workshop.function

import spinal.core._
import spinal.lib._

case class FunctionUnit() extends Component {
  val io = new Bundle {
    val cmd    = slave Flow (Bits(8 bits))
    val valueA = out Bits (8 bits)
    val valueB = out Bits (32 bits)
    val valueC = out Bits (48 bits)
  }

  def patternDetector(str: String) = new Area {
    val hit = False
    // TODO
    val counter = Counter(str.length)
    when(io.cmd.valid) {
      when(
        str.zipWithIndex.map { case (c, i) =>
          counter.value === i && io.cmd.payload === c
        }.orR
      ) {
        when(counter.willOverflowIfInc) {
          hit.set
          counter.clear
        } otherwise {
          counter.increment
        }
      } otherwise {
        counter.clear
      }
    }
  }

  def valueLoader(start: Bool, that: Data) = new Area {
    require(
      widthOf(that) % widthOf(io.cmd.payload) == 0
    ) // You can make the assumption that the 'that' width is always an mulitple of 8
    // TODO
    val BYTE_COUNT = widthOf(that) / widthOf(io.cmd.payload)
    val hit        = RegInit(False) setWhen (start)
    val counter    = Counter(BYTE_COUNT)
    val data       = Reg(Bits(widthOf(that) bits))
    when(hit && io.cmd.valid) {
      data.subdivideIn(BYTE_COUNT slices)(counter) := io.cmd.payload
      hit.clearWhen(counter.willOverflowIfInc)
      counter.increment
    }
    that := data
  }

  val setA  = patternDetector("setValueA")
  val loadA = valueLoader(setA.hit, io.valueA)

  val setB  = patternDetector("setValueB")
  val loadB = valueLoader(setB.hit, io.valueB)

  val setC  = patternDetector("setValueC")
  val loadC = valueLoader(setC.hit, io.valueC)
}
