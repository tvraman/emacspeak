import AVFoundation
import Foundation

actor TonePlayerActor {
  private let audioPlayer = AVAudioPlayerNode()
  private let audioEngine = AVAudioEngine()

  func playPureTone(frequencyInHz: Int, amplitude: Float, durationInMillis: Int) async {
    audioEngine.attach(audioPlayer)
    let mixer = audioEngine.mainMixerNode
    let sampleRateHz = Float(mixer.outputFormat(forBus: 0).sampleRate)

    guard
      let format = AVAudioFormat(
        commonFormat: .pcmFormatFloat32, sampleRate: Double(sampleRateHz),
        channels: AVAudioChannelCount(1), interleaved: false)
    else { return }

    audioEngine.connect(audioPlayer, to: mixer, format: format)

    let totalDurationSeconds = Float(durationInMillis) / 1000
    let fadeDurationSeconds = totalDurationSeconds / 5
    let numberOfSamples = AVAudioFrameCount(sampleRateHz * totalDurationSeconds)

    guard let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: numberOfSamples) else {
      return
    }
    buffer.frameLength = numberOfSamples

    if let channelData = buffer.floatChannelData?[0] {
      let angularFrequency = Float(frequencyInHz * 2) * .pi
      for i in 0..<Int(numberOfSamples) {
        let time = Float(i) / sampleRateHz
        var currentAmplitude = amplitude
        // Fade in
        if time < fadeDurationSeconds {
          currentAmplitude *= time / fadeDurationSeconds
        }
        // Fade out
        else if time > totalDurationSeconds - fadeDurationSeconds {
          currentAmplitude *= (totalDurationSeconds - time) / fadeDurationSeconds
        }

        channelData[i] = sinf(Float(i) * angularFrequency / sampleRateHz) * currentAmplitude
      }
    }

    do {
      try audioEngine.start()
      audioPlayer.play()
      audioPlayer.scheduleBuffer(buffer, completionCallbackType: .dataPlayedBack) { _ in

        // Execution continues after the buffer is fully played
      }
    } catch {
      print("Error: Engine start failure")
    }
  }

  func stop() {
    audioPlayer.stop()
    audioEngine.stop()
  }
}
