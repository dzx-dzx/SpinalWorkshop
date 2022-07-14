package workshop.mandelbrot

import spinal.core._
import spinal.lib._

case class PixelSolverGenerics(fixAmplitude: Int, fixResolution: Int, iterationLimit: Int) {
  val iterationWidth = log2Up(iterationLimit + 1)
  def iterationType  = UInt(iterationWidth bits)
  def fixType = SFix(
    peak = fixAmplitude exp,
    resolution = fixResolution exp
  )
}

case class PixelTask(g: PixelSolverGenerics) extends Bundle {
  val x, y = g.fixType
}

case class PixelResult(g: PixelSolverGenerics) extends Bundle {
  val iteration = g.iterationType
}

case class PixelSolver(g: PixelSolverGenerics) extends Component {
  val io = new Bundle {
    val cmd = slave Stream (PixelTask(g))
    val rsp = master Stream (PixelResult(g))
  }

  import g._

  // Id width that allow router reordering
  val idWidth = 3

  // Base context used in each stage of the pipeline
  class Context extends Bundle {
    val id        = UInt(idWidth bits)
    val x0, y0    = fixType
    val iteration = UInt(iterationWidth bits)
    val done      = Bool
  }

  // Context used at each stages of the pipeline
  case class InserterContext() extends Context {
    val x, y = fixType
  }
  case class MulStageContext() extends Context {
    val xx, yy, xy = fixType
  }
  case class AddStageContext() extends Context {
    val x, y = fixType
  }
  case class RouterContext() extends Context {
    val x, y = fixType
  }

  val inserterContext = Stream(InserterContext())
  val mulStageContext = Stream(MulStageContext())
  val addStageContext = Stream(AddStageContext())
  val routerContext   = Stream(RouterContext())

  val inserter = new Area {
    val freeId   = Counter(1 << idWidth, inc = io.cmd.fire)
    val loopback = routerContext
    val output   = inserterContext
    output <-< StreamArbiterFactory.lowerFirst.noLock.onArgs(
      loopback.translateWith {
        val payload = InserterContext()
        payload.assignSomeByName(loopback.payload)
        payload
      },
      io.cmd.translateWith {
        val payload = InserterContext()
        payload.id := freeId
        payload.x0 := io.cmd.payload.x
        payload.y0 := io.cmd.payload.y
        payload.iteration.clearAll
        payload.done.clear
        payload.x := 0
        payload.y := 0
        payload
      }
    )
  }

  val mulStage = new Area {
    val input  = inserterContext
    val output = mulStageContext

    val payload = MulStageContext()
    payload.assignSomeByName(input.payload)
    payload.xx := (input.x * input.x).truncated
    payload.yy := (input.y * input.y).truncated
    payload.xy := (input.x * input.y).truncated

    output <-< input.translateWith(payload)
  }

  val addStage = new Area {
    val input  = mulStageContext
    val output = addStageContext

    val payload = AddStageContext()
    payload.assignSomeByName(input.payload)
    payload.x := input.xx - input.yy + input.x0
    payload.y := input.xy + input.xy + input.y0
    payload.done.allowOverride
    payload.iteration.allowOverride
    payload.done      := input.done || input.xx + input.yy >= 4 || input.iteration === iterationLimit
    payload.iteration := input.iteration + (!payload.done).asUInt

    output <-< input.translateWith(payload)
  }

  val router = new Area {
    val wantedId = Counter(1 << idWidth, inc = io.rsp.fire)
    val input    = addStageContext
    val loopback = routerContext

    val outputs = StreamDemux(input, (input.done && wantedId === input.id).asUInt, 2)

    io.rsp << outputs(1).translateWith {
      val payload = io.rsp.payloadType()
      payload.iteration := outputs(1).iteration
      payload
    }

    loopback </< outputs(0).translateWith {
      val payload = RouterContext()
      payload.assignSomeByName(outputs(0).payload)
      payload
    }

  }
}
