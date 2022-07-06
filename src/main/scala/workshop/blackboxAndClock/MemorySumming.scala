package workshop.blackboxAndClock

import spinal.core._
import spinal.lib._

// Define a Ram as a BlackBox
case class Ram_1w_1r_2c(wordWidth: Int, addressWidth: Int, writeClock: ClockDomain, readClock: ClockDomain)
    extends BlackBox {
  // TODO define Generics
  addGeneric("wordWidth", wordWidth)
  addGeneric("addressWidth", addressWidth)

  // TODO define IO
  val io = new Bundle {
    val wr = new Bundle {
      val clk  = in Bool ()
      val en   = in Bool ()
      val addr = in UInt (addressWidth bit)
      val data = in Bits (wordWidth bit)
    }
    val rd = new Bundle {
      val clk  = in Bool ()
      val en   = in Bool ()
      val addr = in UInt (addressWidth bit)
      val data = out Bits (wordWidth bit)
    }
  }
  // TODO define ClockDomains mappings
  mapClockDomain(writeClock, io.wr.clk)
  mapClockDomain(readClock, io.rd.clk)
}

// Create the top level and instanciate the Ram
case class MemorySumming(writeClock: ClockDomain, sumClock: ClockDomain) extends Component {
  val io = new Bundle {
    val wr = new Bundle {
      val en   = in Bool ()
      val addr = in UInt (8 bits)
      val data = in Bits (16 bits)
    }

    val sum = new Bundle {
      val start = in Bool ()
      val done  = out Bool ()
      val value = out UInt (16 bits)
    }
  }

  // TODO define the ram
  val ram = new Ram_1w_1r_2c(
    wordWidth = 16,
    addressWidth = 8,
    writeClock = writeClock,
    readClock = sumClock
  )
  // TODO connect the io.wr port to the ram

  io.wr.en   <> ram.io.wr.en
  io.wr.addr <> ram.io.wr.addr
  io.wr.data <> ram.io.wr.data

  val sumArea = new ClockingArea(sumClock) {
    // TODO define the memory read + summing logic
    val sum = Reg(UInt(16 bits)) init (0)

    val readAddr  = Counter(8 bits)
    var doCount = RegInit(False)
    when(io.sum.start) {
      doCount.set
      sum.clearAll
    }
    when(doCount) {
      readAddr.increment
      when(readAddr === readAddr.maxValue) {
        doCount.clear
        readAddr.clear
      }
    }

    val readValid = RegNext(doCount) init (False)
    when(readValid) {
      sum := sum + ram.io.rd.data.asUInt
    }

    ram.io.rd.en   := doCount
    ram.io.rd.addr := readAddr

    io.sum.value := sum
    io.sum.done.clear
    io.sum.done.setWhen(readValid.fall(False))
  }
}
