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
  io.rsp.valid.clear
  io.rsp.payload.iteration.clearAll

  val inserter = new Area {
    val freeId   = Counter(1 << idWidth, inc = io.cmd.fire)
    val loopback = routerContext
    val output   = inserterContext
    output.valid := loopback.valid || io.cmd.valid
    when((!loopback.valid) && io.cmd.valid) {
      output.id := freeId
      output.x0 := io.cmd.payload.x
      output.y0 := io.cmd.payload.y
      output.iteration.clearAll
      output.done.clear
      output.x := 0
      output.y := 0
    } otherwise {
      output.payload.assignSomeByName(loopback.payload)
    }
    io.cmd.ready := !loopback.valid && output.ready

    loopback.ready := output.ready
  }

  val mulStage = new Area {
    val input  = inserterContext.stage
    val output = mulStageContext
    output.valid := input.valid
    output.payload.assignSomeByName(input.payload)
    output.xx := (input.x * input.x).truncated
    output.yy := (input.y * input.y).truncated
    output.xy := (input.x * input.y).truncated

    input.ready := output.ready
  }

  val addStage = new Area {
    val input  = mulStageContext.stage
    val output = addStageContext
    output.valid := input.valid
    output.payload.assignSomeByName(input.payload)
    output.x := input.xx - input.yy + input.x0
    output.y := input.xy + input.xy + input.y0
    output.done.allowOverride
    output.iteration.allowOverride
    output.done      := input.done || input.xx + input.yy >= 4 || input.iteration === iterationLimit
    output.iteration := input.iteration + (!output.done).asUInt

    input.ready := output.ready
  }

  val router = new Area {
    val wantedId = Counter(1 << idWidth, inc = io.rsp.fire)
    val input    = addStageContext.stage
    val loopback = routerContext
    when(input.done && wantedId === input.id) {
      io.rsp.valid             := input.valid
      io.rsp.payload.iteration := input.iteration
    }
    input.ready := !(wantedId === input.id && input.done && !io.rsp.fire)

    loopback.payload.assignSomeByName(input.payload)
    loopback.valid := input.valid && (!input.done || wantedId =/= input.id )
  }
}
