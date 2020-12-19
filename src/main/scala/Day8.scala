package aoc2020
import cats._
import cats.data.State
import cats.implicits.given

object day8 extends (List[String] => String) {
  import Machine._

  def apply(input: List[String]): String =
    val code = parseBootCode(input)
    runMachineUntilLoop(code).toString + "\n" + findBugfix(code).toString

  def runMachineUntilLoop(code: Vector[Instruction]): Either[Int, (Int, IPtr)] =
    def run(state: MachineState, visited: Set[IPtr]): Either[Int, (Int, IPtr)] =
      if (state.finished) Left(state.acc)
      else if(visited.contains(state.iptr)) Right((state.acc, state.iptr))
      else run(state.step, visited + state.iptr)
    run(MachineState(code), Set())

  def findBugfix(code: Vector[Instruction]): Option[Int] =
    import Instruction._
    (0 until code.size).view
      .collect(((i: Int) =>
          code(i) match {
            case Nop(x) => code.updated(i, Jmp(x)).some
            case Jmp(x) => code.updated(i, Nop(x)).some
            case _      => None
          }).unlift)
      .collectFirst(terminalValue.unlift)

  def terminalValue(code: Vector[Instruction]): Option[Int] =
    runMachineUntilLoop(code).swap.toOption

  def parseBootCode(input: List[String]): Vector[Instruction] =
    import Instruction._
    input.view.map {
      case s"acc $num" => Acc(num.toInt)
      case s"jmp $num" => Jmp(num.toInt)
      case s"nop $num" => Nop(num.toInt)
    }.toVector


  object Machine {
    opaque type IPtr = Int
    extension (p: IPtr)
      def +(offset: Int): IPtr = p + offset

    enum Instruction {
      case Nop(value: Int)
      case Jmp(offset: Int)
      case Acc(amount: Int)

      def run(mstate: MachineState): MachineState = this match {
        case Nop(_)      => mstate.copy(iptr = mstate.iptr + 1)
        case Jmp(offset) => mstate.copy(iptr = mstate.iptr + offset)
        case Acc(amount) => mstate.copy(iptr = mstate.iptr + 1, acc = mstate.acc + amount)
      }
    }

    case class MachineState(acc: Int, iptr: IPtr, data: Vector[Instruction]) {
      def step: MachineState = data(iptr).run(this)
      def finished: Boolean = iptr >= data.size
    }

    object MachineState {
      def apply(data: Vector[Instruction]): MachineState = MachineState(0, 0, data)
    }
  }
}
