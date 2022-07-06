package workshop.stream

import spinal.core._
import spinal.lib._

case class MemoryWrite() extends Bundle {
  val address = UInt(8 bits)
  val data    = Bits(32 bits)
}

case class StreamUnit() extends Component {
  val io = new Bundle {
    val memWrite = slave Flow (MemoryWrite())
    val cmdA     = slave Stream (UInt(8 bits))
    val cmdB     = slave Stream (Bits(32 bits))
    val rsp      = master Stream (Bits(32 bits))
  }

  val mem = Mem(Bits(32 bits), 1 << 8)
  // TODO
  mem.write(
    enable = io.memWrite.valid,
    address = io.memWrite.address,
    data = io.memWrite.data
  )

  val readValid = RegInit(False)
  val rspReady  = Bool

  io.cmdA.ready := !readValid || rspReady
  when(io.cmdA.ready) {
    readValid := io.cmdA.valid
  }
  val memPayload = mem.readSync(
    enable = io.cmdA.fire,
    address = io.cmdA.payload
  )

  rspReady      := io.rsp.fire
  io.cmdB.ready := io.rsp.fire

  io.rsp.valid   := readValid && io.cmdB.valid
  io.rsp.payload := memPayload ^ io.cmdB.payload

}
