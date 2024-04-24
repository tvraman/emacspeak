import AVFoundation
import Foundation

actor SoundManager {
    static let shared = SoundManager()
    private var audioPlayers: [UUID: AVAudioPlayer] = [:]

    func playSound(from url: URL, volume: Float) async {
        do {
            let player = try AVAudioPlayer(contentsOf: url)
            player.volume = volume
            player.prepareToPlay()
            player.play()

            // Generate a unique ID for each player instance to handle multiple instances for the same URL
            let playerId = UUID()
            audioPlayers[playerId] = player

            // Use Task.sleep to delay the cleanup
            try await Task.sleep(nanoseconds: UInt64(player.duration * 1_000_000_000))

            // Cleanup the player after it finishes
            audioPlayers[playerId] = nil
        } catch {
            print("Error loading sound: \(error)")
        }
    }
}
