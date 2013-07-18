package io.github.tomykaira.uchronie

import scalaz.NonEmptyList

case class TargetRange(start: Int, end: Int) {
  if (start > end || start < 0)
    throw new TargetRange.InvalidSelectionRangeException(start, end)

  lazy val list: List[TargetRange.Index] = (start to end).toList

  lazy val nel: NonEmptyList[Int] = NonEmptyList.nel(start, (start+1 to end).toList)

  def isSingleton = start == end
}

object TargetRange {
  type Index = Int

  class InvalidSelectionRangeException(val start: Index, val end: Index)
    extends RuntimeException(s"start $start must be less than end $end")

  class RangeNotFilledException(val list: List[Index])
    extends RuntimeException(s"List $list must be a non-empty continuous list")

  def apply(list: List[Index]): TargetRange = {
    if (list.nonEmpty && list.head >= 0 && list.last == list.head + list.length - 1)
      TargetRange(list.head, list.last)
    else
      throw new RangeNotFilledException(list)
  }
}