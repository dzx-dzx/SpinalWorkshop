package workshop.simStreamJoinFork

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable

import SimStreamUtils._

object SimStreamJoinForkTestbench {
  def main(args: Array[String]): Unit = {
    // Compile the simulator
    val compiled = SimConfig.withWave.compile(new SimStreamJoinForkDut)

    // Run the simulation
    compiled.doSim { dut =>
      // Fork clockdomain stimulus generation and simulation timeouts
      dut.clockDomain.forkStimulus(period = 10)
      SimTimeout(100000 * 10)

      // Queues used to rememeber about cmd transactions, used to check rsp transactions
      val xorCmdAQueue, xorCmdBQueue = mutable.Queue[Long]()
      val mulCmdAQueue, mulCmdBQueue = mutable.Queue[Long]()

      // Scoreboard counters, count number of transactions on rsp streams
      var rspXorCounter, rspMulCounter = 0

      // TODO Fork cmd streams drivers. (Randomize valid and payload signals)
      val cmdADriver = streamMasterRandomizer(dut.io.cmdA, dut.clockDomain)
      val cmdBDriver = streamMasterRandomizer(dut.io.cmdB, dut.clockDomain)

      // TODO Fork rsp streams drivers. (Randomize ready signal)
      val rspXorDriver = streamSlaveRandomizer(dut.io.rspMul, dut.clockDomain)
      val rspMulDriver = streamSlaveRandomizer(dut.io.rspXor, dut.clockDomain)

      // TODO Fork monitors to push the cmd transactions values into the queues

      onStreamFire(dut.io.cmdA, dut.clockDomain) {
        xorCmdAQueue.enqueue(dut.io.cmdA.payload.toLong)
        mulCmdAQueue.enqueue(dut.io.cmdA.payload.toLong)
      }
      onStreamFire(dut.io.cmdB, dut.clockDomain) {
        xorCmdBQueue.enqueue(dut.io.cmdB.payload.toLong)
        mulCmdBQueue.enqueue(dut.io.cmdB.payload.toLong)
      }

      // TODO Fork monitors to check the rsp transactions values

      onStreamFire(dut.io.rspXor, dut.clockDomain) {
        assert(dut.io.rspXor.payload.toLong == (xorCmdAQueue.dequeue() ^ xorCmdBQueue.dequeue()))
        rspXorCounter += 1
      }

      onStreamFire(dut.io.rspMul, dut.clockDomain) {
        assert(dut.io.rspMul.payload.toBigInt == (BigInt(mulCmdAQueue.dequeue()) * BigInt(mulCmdBQueue.dequeue())))
        rspMulCounter += 1
      }

      // Wait until all scoreboards counters are OK
      waitUntil(rspMulCounter > 100 && rspXorCounter > 100)
    }
  }
}
