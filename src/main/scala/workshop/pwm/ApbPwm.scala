package workshop.pwm

import org.scalatest.FunSuite
import spinal.core._
import spinal.lib._

//APB configuration class (generic/parameter)
case class ApbConfig(addressWidth: Int, dataWidth: Int, selWidth: Int)

//APB interface definition
case class Apb(config: ApbConfig) extends Bundle with IMasterSlave {
  // TODO define APB signals
  val PSEL    = Bits(config.selWidth bits)
  val PENABLE = Bool()
  val PWRITE  = Bool()
  val PADDR   = UInt(config.addressWidth bits)
  val PWDATA  = Bits(config.dataWidth bits)
  val PRDATA  = Bits(config.dataWidth bits)
  val PREADY  = Bool()

  override def asMaster(): Unit = {
    // TODO define direction of each signal in a master mode
    out(PSEL, PENABLE, PWRITE, PADDR, PWDATA)
    in(PRDATA, PREADY)

  }
}

case class ApbPwm(apbConfig: ApbConfig, timerWidth: Int) extends Component {
  require(apbConfig.dataWidth == 32)
  require(apbConfig.selWidth == 1)

  val io = new Bundle {
    val apb = slave(Apb(apbConfig)) // TODO
    val pwm = out Bool ()           // TODO
  }

  val logic = new Area {
    // TODO define the PWM logic
    val enable = Reg(False)
    val timer  = Reg(UInt(timerWidth bits)) init (0)
    when(enable) {
      timer := timer + 1
    }
    val dutyCycle = Reg(UInt(timerWidth bits)) init (0)
    val output    = RegInit(False)
    output := timer < dutyCycle
    io.pwm := output
  }

  val control = new Area {
    // TODO define the APB slave logic that will make PWM's registers writable/readable
    val doWrite = io.apb.PSEL(0) && io.apb.PENABLE && io.apb.PWRITE
    io.apb.PRDATA := 0
    io.apb.PREADY := True
    switch(io.apb.PADDR) {
      is(0) {
        io.apb.PRDATA := logic.enable.asBits.resized
        when(doWrite) {
          logic.enable := io.apb.PWDATA(0)
        }
      }
      is(4) {
        io.apb.PRDATA := logic.dutyCycle.asBits.resized
        when(doWrite) {
          logic.dutyCycle := io.apb.PWDATA.asUInt.resized
        }
      }
    }
  }
}
