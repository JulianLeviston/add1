# Makefile for a self-contained Agda project (Like PLFA)

AGDA=agda
AGDAFLAGS= --no-libraries -i /workspace/src -i /workspace/stdlib/src --verbose=0

# Default target: check all Agda files
check:
# 	$(AGDA) $(AGDAFLAGS) src/Add1/Everything.agda
# use a shell expansion to include all agda files
	$(AGDA) $(AGDAFLAGS) $(shell find src -name '*.agda')

# For future: clean generated Haskell files (if using MAlonzo, which we're not yet)
clean:
	find src -name '*.agdai' -delete
	find src -name '*.hs' -delete
