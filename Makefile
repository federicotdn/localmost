.PHONY: build test repl validate install lint fmt vendor

build:
	cabal build -j

test:
	cabal test -j --enable-tests

repl:
	cabal repl -j

validate:
	cabal run -j localmost -- validate

install:
	cabal install -j -f -dev --overwrite-policy=always

lint:
	hlint src app test

fmt:
	@ormolu -m inplace $$(git ls-files '*.hs')

define AWK_EXTRACT
/^ *any\./ {
	sub(/^ *any\./, "")
	sub(/ ==.*/, "")
	if ($$0 !~ /^rts|system-cxx-std-lib$$/) print
}
endef
export AWK_EXTRACT

vendor:
	rm -rf vendor
	cabal freeze
	cabal unpack --destdir=vendor $$(awk "$$AWK_EXTRACT" cabal.project.freeze)
	rm -f cabal.project.freeze
