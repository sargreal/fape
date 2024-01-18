package fr.laas.fape.acting

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

/**
  * Created by abitmonn on 11/23/16.
  */
object Clock {
  val interval = 500
  private var currentTime : Long = -interval
  // private var initialTime : Long = -1

  def advanceTime = 
    currentTime += interval

  def time() : Int = 
    timeMilli / interval toInt
  
  def toMilli(time : Int) : Long = 
    time * interval
  
  def toMilliDur(time : Int) = 
    toMilli(time).milli

  def timeMilliDur : FiniteDuration = 
    timeMilli milli

  def timeMilli : Long = {
    return currentTime
    // if(initialTime < 0)
    //   initialTime = System.currentTimeMillis()
    // System.currentTimeMillis() - initialTime
  }
  // def time() : Int = {
  //   currentTime += 1
  //   currentTime.asInstanceOf[Int]
  // }
}
