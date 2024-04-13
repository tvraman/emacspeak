import AVFoundation
import AppKit
import Darwin
import Foundation
import OggDecoder

public actor StateStore {
  private var _allCapsBeep: Bool = false
  public var allCapsBeep: Bool {
    get { _allCapsBeep }
    set { _allCapsBeep = newValue }
  }

  private var _characterScale: Float = 1.2
  public var characterScale: Float {
    get { _characterScale }
    set { _characterScale = newValue }
  }

  private var _pendingQueue: [(String, String)] = []
  public var pendingQueue: [(String, String)] {
    get { _pendingQueue }
    set { _pendingQueue = newValue }
  }

  public func appendToPendingQueue(_ item: (String, String)) {
    _pendingQueue.append(item)
  }

  public func popFromPendingQueue() -> (String, String)? {
    if !_pendingQueue.isEmpty {
      return _pendingQueue.removeFirst()
    }
    return nil
  }

  // Clear the pending queue
  public func clearPendingQueue() {
    _pendingQueue.removeAll()
  }

  private var _pitchMultiplier: Float = 1.0
  public var pitchMultiplier: Float {
    get { _pitchMultiplier }
    set { _pitchMultiplier = newValue }
  }

  private var _postDelay: TimeInterval = 0
  public var postDelay: TimeInterval {
    get { _postDelay }
    set { _postDelay = newValue }
  }

  private var _preDelay: TimeInterval = 0
  public var preDelay: TimeInterval {
    get { _preDelay }
    set { _preDelay = newValue }
  }

  private var _punctuations: String = "all"
  public var punctuations: String {
    get { _punctuations }
    set { _punctuations = newValue }
  }

  private var _audioTarget: String = "None"
  public var audioTarget: String {
    get { _audioTarget.lowercased() }
    set { _audioTarget = newValue }
  }

  private var _soundVolume: Float = 1
  public var soundVolume: Float {
    get { _soundVolume }
    set { _soundVolume = newValue }
  }

  private var _speechRate: Float = 0.5
  public var speechRate: Float {
    get { _speechRate }
    set { _speechRate = newValue }
  }

  private var _splitCaps: Bool = false
  public var splitCaps: Bool {
    get { _splitCaps }
    set { _splitCaps = newValue }
  }

  private var _toneVolume: Float = 1
  public var toneVolume: Float {
    get { _toneVolume }
    set { _toneVolume = newValue }
  }

  private var _ttsDiscard: Bool = false
  public var ttsDiscard: Bool {
    get { _ttsDiscard }
    set { _ttsDiscard = newValue }
  }

  private var _voice: AVSpeechSynthesisVoice = AVSpeechSynthesisVoice()
  public var voice: AVSpeechSynthesisVoice {
    get { _voice }
    set { _voice = newValue }
  }

  private var _voiceVolume: Float = 1
  public var voiceVolume: Float {
    get { _voiceVolume }
    set { _voiceVolume = newValue }
  }

  public init() async {
    self.soundVolume = 1.0
    if let f = Float(self.getEnvironmentVariable("SWIFTMAC_SOUND_VOLUME")) {
      self.soundVolume = f
    }

    self.toneVolume = 1.0
    if let f = Float(self.getEnvironmentVariable("SWIFTMAC_TONE_VOLUME")) {
      self.toneVolume = f
    }

    if let f = Float(self.getEnvironmentVariable("SWIFTMAC_VOICE_VOLUME")) {
      self.voiceVolume = f
    }

    self.audioTarget = self.getEnvironmentVariable("SWIFTMAC_AUDIO_TARGET")

    debugLogger.log("soundVolume \(self.soundVolume)")
    debugLogger.log("toneVolume \(self.toneVolume)")
    debugLogger.log("voiceVolume \(self.voiceVolume)")
  }

  public func getCharacterRate() async -> Float {
    return Float(Float(self.speechRate) * self.characterScale)
  }

  private func getEnvironmentVariable(_ variable: String) -> String {
    return ProcessInfo.processInfo.environment[variable] ?? ""
  }

  public func setAllCapsBeep(_ value: Bool) {
    self._allCapsBeep = value
  }

  public func setCharacterScale(_ value: Float) {
    self._characterScale = value
  }

  public func setPitchMultiplier(_ value: Float) {
    self._pitchMultiplier = value
  }

  public func setPostDelay(_ value: TimeInterval) {
    self._postDelay = value
  }

  public func setPreDelay(_ value: TimeInterval) {
    self._preDelay = value
  }

  public func setPunctuations(_ value: String) {
    self._punctuations = value
  }

  public func setSoundVolume(_ value: Float) {
    self._soundVolume = value
  }

  public func setSpeechRate(_ value: Float) {
    self._speechRate = value
  }

  public func setSplitCaps(_ value: Bool) {
    self._splitCaps = value
  }

  public func setToneVolume(_ value: Float) {
    self._toneVolume = value
  }

  public func setTtsDiscard(_ value: Bool) {
    self._ttsDiscard = value
  }

  func parseLang(_ input: String) -> (String?, String?) {
    let components = input.split(separator: ":", maxSplits: 1)

    switch components.count {
    case 1:
      if input.hasPrefix(":") {
        return (nil, String(components[0]))
      } else {
        return (String(components[0]), nil)
      }
    case 2:
      return (String(components[0]), String(components[1]))
    default:
      return (nil, nil)
    }
  }

  public func setVoice(_ value: String) {
    let (l, v) = parseLang(value)
    let voiceIdentifier = self.getVoiceIdentifier(language: l, voiceName: v)
    if let voice = AVSpeechSynthesisVoice(identifier: voiceIdentifier) {
      self._voice = voice
    } else {
      self._voice = AVSpeechSynthesisVoice()
    }
  }

  private func getVoiceIdentifier(language: String?, voiceName: String?) -> String {
    debugLogger.log("Enter: getVoiceIdentifier")

    let defaultVoice = AVSpeechSynthesisVoice()

    let voices = AVSpeechSynthesisVoice.speechVoices()

    // Check if both language and voiceName are provided
    if let language = language, let voiceName = voiceName {
      if let voice = voices.first(where: { $0.language == language && $0.name == voiceName }) {
        return voice.identifier
      }
    }

    // Check if only language is provided
    if let language = language {
      if let voice = voices.first(where: { $0.language == language }) {
        return voice.identifier
      }
    }

    // Check if only voiceName is provided
    if let voiceName = voiceName {
      if let voice = voices.first(where: { $0.name == voiceName }) {
        return voice.identifier
      }
    }

    // If no matching voice is found, return the default voice identifier
    return defaultVoice.identifier
  }
  public func setVoiceVolume(_ value: Float) {
    self._voiceVolume = value
  }
}
