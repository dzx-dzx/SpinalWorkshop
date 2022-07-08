package workshop.simCounter

import spinal.sim._
import spinal.core._
import spinal.core.sim._

object SimCounterTestbench {
  def main(args: Array[String]): Unit = {
    //Compile the simulator
    val compiled = SimConfig.withWave.compile(new SimCounterDut)

    //Run the simulation
    compiled.doSim{dut =>
      // TODO Fork a thread to generate the reset and the clock with a period of 10 units of time
      dut.clockDomain.forkStimulus(10)

      var counterModel = 0
      for(_ <- 0 until 1000) {
        //TODO Randomize Dut inputs
        dut.io.enable.randomize()

        //TODO Wait until the dut.clockDomain will make the counter register sample its input
        dut.clockDomain.waitSampling()
        //Check that the dut.io.value is matching the expected value.
        assert(dut.io.value.toInt == counterModel, s"dut.io.value mismatch. io.value=${dut.io.value.toInt} counterModel=$counterModel")

        //TODO Update the counterModel
        if (dut.io.enable.toBoolean) {
          counterModel = (counterModel + 1) & 0xFF
        }
      }
    }
  }
}
