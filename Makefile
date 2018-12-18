.PHONY: clean

SOURCE_DIR=src
TARGET=docs/js
SOURCE=${SOURCE_DIR}/Wish.elm
ARTIFACT=${TARGET}/wish.js
MINIFIED_ARTIFACT=${TARGET}/wish.min.js

${MINIFIED_ARTIFACT}: ${ARTIFACT}
	uglifyjs ${ARTIFACT} --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=${MINIFIED_ARTIFACT}

${ARTIFACT}: ${SOURCE} ${SOURCE_DIR}/*.elm ${SOURCE_DIR}/*/*.elm
	elm make $< --optimize --output=${TARGET}/wish.js

clean:
	rm -f ${ARTIFACT}
	rm -f ${MINIFIED_ARTIFACT}
