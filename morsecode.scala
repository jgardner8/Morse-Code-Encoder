object MorseCode extends App {
  def asciiToMorse(s: String): String = {
    val morseArr = Array(".-", "-...", "-.-.", "-..", ".", "..-.", "--.", "....", "..", ".---", 
      "-.-", ".-..", "--", "-.", "---", ".--.", "--.-", ".-.", "...", "-", "..-", "...-", 
      ".--", "-..-", "-.--", "--..")
    def asciiCharToMorse(char: Char): String =
      if (char == ' ') "/"
      else morseArr(char.toLower - 'a'.toInt) 
    s map asciiCharToMorse mkString " "
  }

  def outputText(asciiStr: String, morseStr: String): Unit = {
    val asciiStrPadded = (asciiStr, morseStr.split(' ')).zipped map { (asciiChar, morseChar) => 
      if (asciiChar == ' ') "/ "
      else asciiChar.toString.padTo(morseChar.length + 1, ' ')
    } mkString ""

    println(asciiStrPadded)
    println(morseStr)
  }

  def outputWav(morseStr: String, fileName: String, dotTime: Double, pitchHz: Int): Unit = {
    case class Tone(noise: Boolean, time: Double)

    def morseToTones(morseStr: String, dotTime: Double): Seq[Tone] = {
      val dot = Tone(true, dotTime)
      val dash = Tone(true, dotTime * 3)
      val delayDotDash = Tone(false, dotTime)
      val delayChar = Tone(false, dotTime * 3)
      val delayWord = Tone(false, dotTime * 7)
      
      morseStr.split(' ').foldLeft(List[Tone]()) { (acc, morseChar) =>
        val tones = (morseChar map { char =>
          if (char == '/') Seq(delayWord)
          else Seq(if (char == '.') dot else dash, delayDotDash)
        }).flatten
        if (tones.head == delayWord) acc.init ++ tones
        else acc ++ tones.init :+ delayChar
      }
    }

    def tonesToWavFile(tones: Seq[Tone], fileName: String, pitchHz: Double): Unit = {
      import java.io.File, Math._
      val sampleRate = 44100
      val desiredBatchSize = sampleRate / 100
      val durationSec = tones.foldLeft(0.0) { _ + _.time }
      val numSamples = (durationSec * sampleRate).toLong
      val wavFile = WavFile.newWavFile(new File(fileName), numChannels=1, numSamples, 
        validBits=16, sampleRate)

      val sinWaveRate = pitchHz * 2*PI
      val sinMemo = Memoize1(sin)
      def samples = tones.foldLeft(Stream[Double]()) { (acc, cur) =>
        val numSamples = (cur.time * sampleRate).toInt
        acc append ((0 to numSamples) map { sampleNum => 
          def normaliseToPeriod[T : Numeric](x: T, period: T): T = {
            import Numeric.Implicits._, Ordering.Implicits._
            if (x > period) normaliseToPeriod(x - period, period)
            else x
          }
          val x = (sampleNum / sampleRate.toDouble) * sinWaveRate
          if (cur.noise) sinMemo(normaliseToPeriod(x, 2*PI))
          else 0
        })
      }

      val numBatches = numSamples / desiredBatchSize.toDouble
      (0 to numBatches.toInt).foldLeft(samples) { (samples, batchNum) => 
        val samplesLeft = numSamples - (batchNum * desiredBatchSize)
        val batchSize = min(desiredBatchSize, samplesLeft).toInt
        val batch = samples.take(batchSize).toArray
        wavFile.writeFrames(batch, batchSize)
        samples.drop(batchSize)
      }

      wavFile.close()
    }

    val tones = morseToTones(morseStr, dotTime)
    tonesToWavFile(tones, fileName, pitchHz)
  }

  def parseArgs(args: Seq[String]): (String, Option[String], Double, Int) = {
    val defDotTime = 0.075
    val defPitchHz = 1000

    assert(args.length >= 1 && args.length <= 4, 
      "Usage: morsecode input_string [output_filename] [dot_time] [pitch_hz]\n" +
      s"Defaults: dot_time = $defDotTime, pitchHz = $defPitchHz")
    
    ( args(0) filter { c => c.isLetter || c == ' ' }, 
      if (args.length >= 2) Some(args(1)) else None,
      if (args.length >= 3) args(2).toDouble else defDotTime,
      if (args.length == 4) args(3).toInt else defPitchHz )
  }

  val (asciiStr, fileName, dotTime, pitchHz) = parseArgs(args)
  val morseStr = asciiToMorse(asciiStr)
  outputText(asciiStr, morseStr)
  fileName foreach { outputWav(morseStr, _, dotTime, pitchHz) }
}
