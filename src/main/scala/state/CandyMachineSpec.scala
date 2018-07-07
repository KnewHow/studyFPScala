package fpscala.state

import fpscala.state._
sealed trait Input

case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, cons: Int)

object Candy {
  def update = (i: Input) => (m: Machine) => (i, m) match {
    case(_, Machine(_, 0, _)) => m
    case(Coin, Machine(true, ca, co)) => Machine(false, ca, co+1)
    case(Turn, Machine(false, ca, co)) => Machine(true, ca-1, co)
    case(Turn, Machine(true, _, _)) => m
    case(Coin, Machine(false, _, _)) => m
  }

  def simulateMachine(inputs: List[Input]):State[Machine, (Int, Int)] = {
    for {
      _ <- State.sequence(inputs.map(State.modify[Machine] _ compose update))
      s <- State.get
    }yield(s.candies, s.cons)
  }
}
