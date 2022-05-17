SRC_DIR     = src
PARSER_DIR  = ${SRC_DIR}/Parser
BNFC        = bnfc
BNFC_OPTS   = -o ${PARSER_DIR} --haskell --functor -m
BUILD_DIR   = build
GHC         = ghc
GHC_OPTS    = --make -O2 -outputdir ${BUILD_DIR} -i${SRC_DIR}:${PARSER_DIR} -Wall

.PHONY : all clean distclean interpreter wc

all: interpreter

${BUILD_DIR}/Hawat: ${SRC_DIR}/Hawat.cf
	${BNFC} ${BNFC_OPTS} $<
	make -C ${PARSER_DIR} all clean
	mkdir -p build
	touch $@

interpreter: ${BUILD_DIR}/Hawat
	${GHC} ${GHC_OPTS} -o $@ ${SRC_DIR}/Main.hs

clean:
	-make -C ${PARSER_DIR} distclean
	-rm -rf build


distclean: clean
	-rm -f interpreter

wc:
	find ${SRC_DIR} -path ${PARSER_DIR} -prune -o -type f -exec wc -l {} +