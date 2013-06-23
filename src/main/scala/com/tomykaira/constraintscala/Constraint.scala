package com.tomykaira.constraintscala

class Constraint[A](getter: => A) {
  var callbackList = List[A => Unit]()
  var cache : Option[A] = None

  def onChange(callback: A => Unit) {
    callbackList = callback :: callbackList
  }

  def get : A = cache.getOrElse({
    val v = getter
    cache = Some(v)
    v
  })

  def invalidate() {
    cache = None
    callbackList.foreach(callback => callback(get))
  }

}
