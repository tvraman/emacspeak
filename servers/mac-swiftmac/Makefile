#$Id$
# Type make, run ./servers/swiftmac for running the server.

INSTALL = install
PREFIX = /usr
LIBPARENTDIR = ${PREFIX}/share/emacs/site-lisp
LIBDIR =$(LIBPARENTDIR)/emacspeak/servers/mac-swiftmac

SWIFT=swift
TTS=../swiftmac
RELEASE=./.build/release/swiftmac

all:   $(TTS)

$(TTS): $(RELEASE)
	cp $(RELEASE) $(TTS)

$(RELEASE):
	$(SWIFT) build -c release

clean:
	rm -rf .build

tidy:
	swift-format Package.swift > temp
	cp temp Package.swift
	swift-format Sources/SwiftMacPackage/logger.swift > temp
	cp temp Sources/SwiftMacPackage/logger.swift 
	swift-format Sources/SwiftMacPackage/main.swift > temp
	cp temp Sources/SwiftMacPackage/main.swift 
	swift-format Sources/SwiftMacPackage/playpuretone.swift > temp
	cp temp Sources/SwiftMacPackage/playpuretone.swift 
	swift-format Sources/SwiftMacPackage/statestore.swift > temp
	cp temp Sources/SwiftMacPackage/statestore.swift 
	rm temp

# TODO: find someone who actually installs this way and confirm works
install: $(TTS)
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)
	$(INSTALL) $< $(DESTDIR)$(LIBDIR)
