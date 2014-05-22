package org.oopsc

class Position(var line: Int = -1, var column: Int = -1) {
  override def toString() =
    if (line == -1) "<internal>" else s"line $line, column $column"
}