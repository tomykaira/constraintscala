package com.tomykaira.constraintscala

trait Notifier[+A] {
  private[this] var callbackList = List[A => Unit]()

  def get : A

  def invokeCallbacks() {
    callbackList.foreach(callback => callback(get))
  }

  def onChange(callback: A => Unit) {
    callbackList = callback :: callbackList
    callback(get)
  }

}
