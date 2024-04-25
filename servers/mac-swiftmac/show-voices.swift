import AVFoundation

// List all available voices and their identifiers
let voices = AVSpeechSynthesisVoice.speechVoices()
print("Available Voices and Their Identifiers:")

for voice in voices {
    print("Language: \(voice.language), Identifier: \(voice.identifier), Name: \(voice.name), Quality: \(voice.quality.rawValue)")
}

// Optionally, you can speak a sample text using each voice
let synthesizer = AVSpeechSynthesizer()
for voice in voices {
    let utterance = AVSpeechUtterance(string: "Hello, this is a sample text in \(voice.language).")
    utterance.voice = voice
    synthesizer.speak(utterance)
}
