import AVFoundation
import AppKit
import Darwin
import Foundation

// Due to being fully async, handling the state is a bit of a pain,
// we have to store it all in a class and gate access to it, the good
// news is the only syncronise bits are on reading the data out.
class StateStore {
  private var backlog: String = ""
  // private var voiceq = defaultVoice
  private var splitCaps: Bool = defaultSplitCaps
  private var voice = defaultVoice
  private var beepCaps: Bool = defaultBeepCaps
  private var charScale: Float = defaultCharScale
  private var punct: String = defaultPunct
  private let queue = DispatchQueue(
    label: "org.emacspeak.server.swiftmac.state",
    qos: .userInteractive)

  func clearBacklog() {
    #if DEBUG
      debugLogger.log("Enter: clearBacklog")
    #endif

    queue.async {
      self.backlog = ""
    }
  }

  func pushBacklog(_ with: String, code: Bool = false) {
    #if DEBUG
      debugLogger.log("Enter: pushBacklog")
    #endif
    let punct = self.getPunct().lowercased()
    queue.async { [weak self] in
      guard let self = self else { return }
      var w = stripSpecialEmbeds(with)
      if !code {
        switch punct {
        case "all":
          w = replaceAllPuncs(w)
        case "some":
          w = replaceSomePuncs(w)
        case "none":
          w = replaceBasePuncs(w)
        default:
          w = replaceCore(w)
        }
      }
      self.backlog += w
    }
  }

  func popBacklog() -> String {
    #if DEBUG
      debugLogger.log("Enter: popBacklog")
    #endif
    var result: String = ""
    queue.sync {
      result = self.backlog
    }
    self.clearBacklog()
    return result
  }

  func setCharScale(_ r: Float) {
    #if DEBUG
      debugLogger.log("Enter: setCharScale")
    #endif
    queue.async {
      self.charScale = r
    }
  }

  func getCharScale() -> Float {
    #if DEBUG
      debugLogger.log("Enter: getCharScale")
    #endif
    return queue.sync {
      return self.charScale
    }
  }

  func setPunct(_ s: String) {
    #if DEBUG
      debugLogger.log("Enter: setPunct")
    #endif
    queue.async {
      self.punct = s
    }
  }

  func getPunct() -> String {
    #if DEBUG
      debugLogger.log("Enter: getPunct")
    #endif
    return queue.sync {
      return self.punct
    }
  }

  func setSplitCaps(_ b: Bool) {
    #if DEBUG
      debugLogger.log("Enter: setSplitCaps")
    #endif
    queue.async {
      self.splitCaps = b
    }
  }

  func getSplitCaps() -> Bool {
    #if DEBUG
      debugLogger.log("Enter: getSplitCaps")
    #endif
    return queue.sync {
      return self.splitCaps
    }
  }

  func setBeepCaps(_ b: Bool) {
    #if DEBUG
      debugLogger.log("Enter: setBeepCaps")
    #endif
    queue.async {
      self.beepCaps = b
    }
  }

  func getBeepCaps() -> Bool {
    #if DEBUG
      debugLogger.log("Enter: getBeepCaps")
    #endif
    return queue.sync {
      return self.beepCaps
    }
  }
}
