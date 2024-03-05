package fr.laas.fape.acting

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.TimerScheduler
import fr.laas.fape.acting.messages._
import akka.actor.typed.ActorRef

/**
  * Created by abitmonn on 11/23/16.
  */
object Clock {
  private var interval = 500
  // private var currentTime : Long = -interval
  // private var initialTime : Long = -1

  // def advanceTime = 
  //   currentTime += interval

  // def time() : Int = 
  //   timeMilli / interval toInt
  
  // def toMilli(time : Int) : Long = 
  //   time * interval
  
  // def toMilliDur(time : Int) = 
  //   toMilli(time).milli

  // def timeMilliDur : FiniteDuration = 
  //   timeMilli milli

  // def timeMilli : Long = {
  //   return currentTime
  //   // if(initialTime < 0)
  //   //   initialTime = System.currentTimeMillis()
  //   // System.currentTimeMillis() - initialTime
  // }
  // // def time() : Int = {
  // //   currentTime += 1
  // //   currentTime.asInstanceOf[Int]
  // // }

  final object TickKey

  class ClockData(val currentTime: Int, val listeners: Set[ActorRef[Tick]], val timepointListeners: Set[ReplyAt]) {
    def advanceTime = new ClockData(currentTime + 1, listeners, timepointListeners)

    def addListener(listener: ActorRef[Tick]): ClockData = {
      new ClockData(currentTime, listeners + listener, timepointListeners)
    }

    def removeListener(listener: ActorRef[Tick]): ClockData = {
      new ClockData(currentTime, listeners - listener, timepointListeners)
    }

    def addTimpointListener(timepointListener: ReplyAt): ClockData = {
      new ClockData(currentTime, listeners, timepointListeners + timepointListener)
    }
  }

  class InitData(val interval: Int)
  

  def apply(interval: Int) : Behavior[ClockEvent] = Behaviors.setup { context =>
    this.interval = interval
    Behaviors.withTimers(timers => new Clock(timers).ticking(new ClockData(0, Set(), Set())))
  }
}

import fr.laas.fape.acting.Clock._

class Clock(timers: TimerScheduler[ClockEvent]) {
  timers.startTimerWithFixedDelay(TickKey, InternalTick, 0.seconds, interval.milli)

  def ticking(data: ClockData) : Behavior[ClockEvent] = Behaviors.setup { context =>
    Behaviors.receiveMessage[ClockEvent] {
      case InternalTick => 
        context.log.trace(s"[${data.currentTime}] Tick")
        data.timepointListeners.filter(_.timepoint == data.currentTime).foreach(r => r.actorRef ! TimedReply(data.currentTime, r.reference))
        data.listeners.foreach(_ ! Tick(data.currentTime))
        ticking(data.advanceTime)
      case RegisterTickListener(listener) =>
        ticking(data.addListener(listener))
      case UnregisterTickListener(listener) =>
        ticking(data.removeListener(listener))
      case r: ReplyAt =>
        ticking(data.addTimpointListener(r))
      case StopClock =>
        stopped(data)
      case ResumeClock => 
        Behaviors.same
      // case GetTime(actorRef) =>
      //   actorRef ! Time(data.currentTime)
      //   Behaviors.same
    }
  }

  def stopped(data: ClockData): Behavior[ClockEvent] = Behaviors.receiveMessage[ClockEvent] {
    case StopClock =>
      Behaviors.same
    case InternalTick =>
      Behaviors.same
    case RegisterTickListener(listener) =>
      stopped(data.addListener(listener))
    case UnregisterTickListener(listener) =>
      stopped(data.removeListener(listener))
    case r: ReplyAt =>
      stopped(data.addTimpointListener(r))
    case ResumeClock => 
      ticking(data)
    // case GetTime(actorRef) =>
    //   actorRef ! Time(data.currentTime)
    //   Behaviors.same
  }
  
}
