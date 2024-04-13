#$Id$
# Type make, run ./servers/swiftmac for running the server.

INSTALL = install
PREFIX = /usr
LIBPARENTDIR = ${PREFIX}/share/emacs/site-lisp
LIBDIR =$(LIBPARENTDIR)/emacspeak/servers/mac-swiftmac

SWIFT=swift
TTS=../
RELEASE=./.build/release/swiftmac
OGG=./.build/release/ogg.framework
VORBIS=./.build/release/vorbis.framework

all:   $(TTS)

$(TTS): $(RELEASE)
	cp -f $(RELEASE) $(TTS)
	cp -rf $(OGG) $(TTS)
	cp -rf $(VORBIS) $(TTS)

$(RELEASE): clean
	$(SWIFT) build -c release

clean:
	$(SWIFT) package clean

tidy:
	swift-format Package.swift > temp
	cp temp Package.swift
	swift-format Sources/SwiftMacPackage/logger.swift > temp
	cp temp Sources/SwiftMacPackage/logger.swift 
	swift-format Sources/SwiftMacPackage/main.swift > temp
	cp temp Sources/SwiftMacPackage/main.swift 
	swift-format Sources/SwiftMacPackage/toneplayer.swift > temp
	cp temp Sources/SwiftMacPackage/toneplayer.swift 
	swift-format Sources/SwiftMacPackage/statestore.swift > temp
	cp temp Sources/SwiftMacPackage/statestore.swift 
	rm temp

# NOTE: untested path, disabled for now
# install: $(TTS)
#	$(INSTALL) -d $(DESTDIR)$(LIBDIR)
#	$(INSTALL) $< $(DESTDIR)$(LIBDIR)
