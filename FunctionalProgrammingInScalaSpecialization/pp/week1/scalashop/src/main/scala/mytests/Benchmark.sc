import org.scalameter
import org.scalameter._

val seq = for {
  i <- 1 to 100
  time = measure {(0 until 1000000).toArray}
} yield (i, time)

seq.map(_._2).sum./(seq.last._1)


val seq2 = for {
  i <- 1 to 100
  time = withWarmer(new Warmer.Default) measure (0 until 1000000).toArray
} yield (i, time)

seq2.map(_._2).sum./(seq2.last._1)

//val time = config(
//  Key.exec.minWarmupRuns -> 20,
//  Key.exec.maxWarmupRuns -> 60,
////  Key.verbose -> true
//) withWarmer(new scalameter.Warmer.Default) measure {
//  (0 until 1000000).toArray
//}

withWarmer(new scalameter.Warmer.Default)
withMeasurer(new Measurer.MemoryFootprint)
measure { (0 until 1000000).toArray }