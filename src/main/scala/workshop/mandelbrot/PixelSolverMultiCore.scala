package workshop.mandelbrot

import spinal.core._
import spinal.lib._

case class Dispatcher[T <: Data](dataType: T, outputsCount: Int) extends Component {
  val io = new Bundle {
    val input   = slave Stream (dataType)
    val outputs = Vec(master Stream (dataType), outputsCount)
  }
  // TODO
  val counter = Counter(outputsCount)
  for (o <- io.outputs) {
    o.valid.clear
    o.payload := io.input.payload
  }
  io.outputs(counter).valid := io.input.valid
  io.input.ready            := io.outputs(counter).ready
  when(io.outputs(counter).fire) {
    counter.increment
  }
}

// TODO Define the Arbiter component (similar to the Dispatcher)
case class Arbiter[T <: Data](dataType: T, inputsCount: Int) extends Component {
  val io = new Bundle {
    val inputs = Vec(slave Stream (dataType), inputsCount)
    val output = master Stream (dataType)
  }
  val counter = Counter(inputsCount)
  for (i <- io.inputs) {
    i.ready.clear
  }
  io.output.valid          := io.inputs(counter).valid
  io.output.payload        := io.inputs(counter).payload
  io.inputs(counter).ready := io.output.ready
  when(io.output.fire) {
    counter.increment
  }
}

case class PixelSolverMultiCore(g: PixelSolverGenerics, coreCount: Int) extends Component {
  val io = new Bundle {
    val cmd = slave Stream (PixelTask(g))
    val rsp = master Stream (PixelResult(g))
  }

  // TODO instantiate all components
  val dispatcher = Dispatcher(PixelTask(g), coreCount)
  val solver     = List.fill(coreCount)(PixelSolver(g))
  val arbiter    = Arbiter(PixelResult(g), coreCount)

  // TODO interconnect all that stuff
  io.cmd >> dispatcher.io.input
  for (i <- 0 until coreCount) {
    dispatcher.io.outputs(i) >> solver(i).io.cmd
    solver(i).io.rsp >> arbiter.io.inputs(i)
  }
  arbiter.io.output >> io.rsp
}
