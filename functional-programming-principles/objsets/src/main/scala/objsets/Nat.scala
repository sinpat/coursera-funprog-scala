package objsets

import scala.annotation.tailrec

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = true

  def predecessor: Nat = throw new NoSuchElementException("Zero.predecessor")

  def +(that: Nat): Nat = that

  def -(that: Nat): Nat = that match {
    case _: Succ => throw new NoSuchElementException("negative")
    case Zero    => this
  }
}
class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false

  def predecessor: Nat = n

  def +(that: Nat): Nat = new Succ(n + that)

  def -(that: Nat): Nat = that match {
    case _that: Succ => n - _that.predecessor
    case Zero        => this
  }
}
