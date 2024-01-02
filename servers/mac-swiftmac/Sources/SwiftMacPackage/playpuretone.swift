import AVFoundation
import AppKit
import Darwin
import Foundation

let audioPlayer = AVAudioPlayerNode()
let audioEngine = AVAudioEngine()
let semaphore = DispatchSemaphore(value: 1)

func playPureTone(
  frequencyInHz: Int, amplitude: Float,
  durationInMillis: Int
) async {
  #if DEBUG
    debugLogger.log("in playPureTone")
  #endif
  semaphore.wait()
  audioEngine.attach(audioPlayer)
  let mixer = audioEngine.mainMixerNode
  let sampleRateHz = Float(mixer.outputFormat(forBus: 0).sampleRate)

  guard
    let format = AVAudioFormat(
      commonFormat: AVAudioCommonFormat.pcmFormatFloat32,
      sampleRate: Double(sampleRateHz),
      channels: AVAudioChannelCount(1), interleaved: false)
  else {
    semaphore.signal()
    return
  }

  audioEngine.connect(audioPlayer, to: mixer, format: format)

  let numberOfSamples = AVAudioFrameCount(
    (Float(durationInMillis)
      / 1000 * sampleRateHz))

  guard
    let buffer = AVAudioPCMBuffer(
      pcmFormat: format,
      frameCapacity: numberOfSamples)
  else {
    semaphore.signal()
    return
  }
  buffer.frameLength = numberOfSamples

  let channels = UnsafeBufferPointer(
    start: buffer.floatChannelData, count: Int(format.channelCount))
  let floats = UnsafeMutableBufferPointer<Float>(
    start: channels[0], count: Int(numberOfSamples))

  let angularFrequency = Float(frequencyInHz * 2) * .pi

  for i in 0..<Int(numberOfSamples) {
    let waveComponent =
      sinf(Float(i) * angularFrequency / sampleRateHz)
    floats[i] = waveComponent * amplitude
  }
  do {
    try audioEngine.start()
  } catch {
    debugPrint("Error: Engine start failure")
    semaphore.signal()
    return
  }

  audioPlayer.play()
  audioPlayer.scheduleBuffer(buffer, at: nil, options: .interrupts) {
    semaphore.signal()
  }

  semaphore.wait()
  semaphore.signal()
}
