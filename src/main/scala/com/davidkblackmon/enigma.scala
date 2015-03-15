package com.davidkblackmon.enigma

object Enigma {
  case class Rotor(sideA: List[Char], sideB: List[Char])
  case class Reflector(changes: Map[Char,Char]) {
    def read(c: Char) = changes.getOrElse(c,c)
  }

  case class Machine(r1: Rotor, r2: Rotor, r3: Rotor, reflector: Reflector, plugBoard: Reflector)

  val charList = ('A' to 'Z').toList
  val plugCount = 10

  def buildReflector(size: Int, seed: Int): Reflector = {
    val chars = shuffledChars(seed).take(size)
    Reflector(chars.take(size/2).zip(chars.drop(size/2)).flatMap(x => List(x,x.swap)).toMap)
  }

  def shuffledChars(seed: Int): List[Char] = {
    (new scala.util.Random(seed)).shuffle(charList)
  }

  def buildPlugBoard(seed: Int): Reflector = {
    buildReflector(plugCount,seed)
  }

  def buildRotor(seed1: Int, seed2: Int): Rotor = {
    Rotor(shuffledChars(seed1), shuffledChars(seed2))
  }

  def readRotor(r: Rotor)(reflected: Boolean)(position: Int)(index: Int)(input: Char): Char = {
    if (reflected)
      read(r.sideB,rotate(r.sideA, rotationDist(position, index)))(input)
    else
      read(rotate(r.sideA, rotationDist(position, index)),r.sideB)(input)
  }

  def read(inputSide: List[Char], outputSide: List[Char])(input: Char): Char = {
    outputSide(inputSide.indexOf(input))
  }

  def rotationDist(position: Int, index: Int): Int = position match {
    case 0 => index % 26
    case 1 => if (index % 25 == 0) 1 else 0
    case 2 => if (index % 25*25 == 0) 1 else 0
    case _ => 0
  }

  def rotate[A](xs: List[A], dist: Int): List[A] = {
    xs.drop(dist) ::: xs.take(dist)
  }

  def engimaize(machine: Machine, input: String): String = {
    input.zipWithIndex.map{ case (c,i) =>
      val push = machine.plugBoard.read _ andThen
        readRotor(machine.r1)(false)(0)(i)_ andThen
        readRotor(machine.r2)(false)(1)(i)_ andThen
        readRotor(machine.r3)(false)(2)(i)_ andThen
        machine.reflector.read _ andThen
        readRotor(machine.r3)(true)(2)(i)_ andThen
        readRotor(machine.r2)(true)(1)(i)_ andThen
        readRotor(machine.r1)(true)(0)(i)_ andThen
        machine.plugBoard.read _

      push(c)
     }.mkString
  }
}
