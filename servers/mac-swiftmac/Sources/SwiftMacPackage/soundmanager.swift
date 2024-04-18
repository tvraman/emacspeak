import AVFoundation
import Foundation

actor SoundManager {
    static let shared = SoundManager()
    
    func playSound(from url: URL, volume: Float) {
        Task.detached {
            do {
                let player = try AVAudioPlayer(contentsOf: url)
                player.volume = volume
                player.prepareToPlay()
                player.play()
                
                // Keep the player alive until it finishes playing
                try await Task.sleep(nanoseconds: UInt64(player.duration * 1_000_000_000))
            } catch {
                print("Error loading sound: \(error)")
            }
        }
    }
}
