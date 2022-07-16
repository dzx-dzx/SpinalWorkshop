package workshop.udp
import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{EntryPoint, StateParallelFsm, State, StateMachine}

case class UdpAppCmd() extends Bundle {
  val ip      = Bits(32 bits)
  val srcPort = Bits(16 bits)
  val dstPort = Bits(16 bits)
  val length  = UInt(16 bits)
}

case class UdpAppBus() extends Bundle with IMasterSlave {
  val cmd  = Stream(UdpAppCmd())
  val data = Stream(Fragment(Bits(8 bits)))

  override def asMaster(): Unit = master(cmd, data)
}

object Hello {
  val discoveringCmd = 0x11
  val discoveringRsp = 0x22
}

case class UdpApp(helloMessage: String, helloPort: Int = 37984) extends Component {
  val io = new Bundle {
    val rx = slave(UdpAppBus())
    val tx = master(UdpAppBus())
  }

  // TODO give default value to rx/tx output pins

  io.rx.cmd.ready  := False
  io.rx.data.ready := False

  io.tx.cmd.valid   := False
  io.tx.cmd.ip      := io.rx.cmd.ip
  io.tx.cmd.srcPort := io.rx.cmd.dstPort
  io.tx.cmd.dstPort := io.rx.cmd.srcPort
  io.tx.cmd.length  := 1 + helloMessage.length

  io.tx.data.valid    := False
  io.tx.data.last     := False
  io.tx.data.fragment := 0
  //参考解答, 这里的代码在校验存在有效发送指令, 且数据发送完毕后才进行握手。
  //这令rx.cmd的数据一直保持固定, 从而tx.cmd的数据不需要寄存器保存.
  val fsm = new StateMachine {
    // Filter rx dst ports
    val idle: State = new State with EntryPoint {
      whenIsActive {
        // TODO Check io.rx.cmd dst port
        when(io.rx.cmd.valid) {
          when(io.rx.cmd.dstPort === helloPort) {
            goto(helloHeader)
          } otherwise {
            goto(clearRx)
          }
        }
      }
    }

    // Check the hello protocol Header
    val helloHeader = new State {
      whenIsActive {
        // TODO check that the first byte of the packet payload is equals to Hello.discoveringCmd
        when(io.rx.data.valid) {
          when(io.rx.data.fragment === Hello.discoveringCmd) {
            goto(discoveringRspTx)
          } otherwise {
            goto(clearRx)
          }
        }
      }
    }

    val clearRx = new State {
      onEntry {//它的转变事实上在State值转变之前. 可以视作前一个State的一部分.
        io.rx.cmd.ready.set
      }
      whenIsActive {
        io.rx.data.ready.set
        io.rx.cmd.ready.clear
        when(io.rx.data.last) {
          goto(idle)
        }
      }
    }

    // Send an discoveringRsp packet
    val discoveringRspTx = new StateParallelFsm(
      discoveringRspTxCmdFsm,
      discoveringRspTxDataFsm
    ) {
      whenCompleted {
        // TODO return to IDLE
        report(L"At $REPORT_TIME, whenCompleted invoked.")
        // io.tx.data.valid.clear //这一语句会与sendMessage的set抵消, 导致最后一份数据没有发送出来.
        goto(clearRx)
      }
    }
  }

  // Inner FSM of the discoveringRspTx state
  lazy val discoveringRspTxCmdFsm = new StateMachine {
    val sendCmd = new State with EntryPoint {
      whenIsActive {
        // TODO send one io.tx.cmd transaction
        io.tx.cmd.valid.set
        when(io.tx.cmd.ready) {
          exit()
        }
      }
    }
  }

  // Inner FSM of the discoveringRspTx state
  lazy val discoveringRspTxDataFsm = new StateMachine {
    val sendHeader = new State with EntryPoint {
      whenIsActive {
        // TODO send the io.tx.cmd header (Hello.discoveringRsp)
        io.tx.data.valid.set
        io.tx.data.payload.fragment := Hello.discoveringRsp
        when(io.tx.data.ready) {
          goto(sendMessage)
        }
      }
    }

    val sendMessage = new State {
      val counter = Reg(UInt(log2Up(helloMessage.length) bits))
      onEntry {
        counter := 0
      }
      whenIsActive {
        // TODO send the message on io.tx.cmd header
        io.tx.data.valid.set
        io.tx.data.payload.fragment := helloMessage.toVecOfByte()(counter)
        io.tx.data.payload.last     := counter === helloMessage.length - 1
        when(io.tx.data.ready) {
          counter := counter + 1
          when(io.tx.data.payload.last) {
            report(L"At $REPORT_TIME, exit() executed.")
            exit()//exit的跳转似乎是立即的(存疑), 当它被执行时, 满足条件的whenCompleted会立刻执行.
          }
        }
      }
    }
  }
}
