import AVFoundation
import Foundation

actor SoundManager {
    static let shared = SoundManager()
    
    private var soundCache: [URL: AVAudioPlayer] = [:]
    private var currentSoundURL: URL?
    
    func playSound(from url: URL, volume: Float) async {
        // Stop the currently playing sound
        await stopCurrentSound()
        
        let player: AVAudioPlayer
        
        // Try to reuse an existing sound player if available
        if let cachedPlayer = soundCache[url], cachedPlayer.volume == volume {
            player = cachedPlayer
        } else {
            // Create a new player if necessary
            do {
                player = try AVAudioPlayer(contentsOf: url)
                player.volume = volume
                soundCache[url] = player // Cache the newly created player
            } catch {
                print("Error loading sound: \(error)")
                return
            }
        }
        
        player.play()
        currentSoundURL = url
    }
    
    func stopCurrentSound() async {
        guard let url = currentSoundURL, let player = soundCache[url], player.isPlaying else {
            return
        }
        
        player.stop()
        player.currentTime = 0 // Reset playback if needed
    }
}
