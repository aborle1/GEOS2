TARGET = ncwa
TEMPLATE = app
CONFIG -= qt
CONFIG += debug_and_release

CONFIG( debug, debug|release ) {
        win32:LIBS += ../libnco/debug/libnco.lib
        unix:LIBS  += ../libnco/debug/liblibnco.a
} else {
        win32:LIBS += ../libnco/release/libnco.lib
        unix:LIBS  += ../libnco/release/liblibnco.a
}

include (../nco.pri)

unix {
HEADERS = ../../src/nco/ncap_yacc.h
SOURCES   = ../../src/nco/ncwa.c \
../../src/nco/ncap_utl.c 
}
win32 {
SOURCES   = ../../src/nco/ncwa.c 
}

unix {
FLEXSOURCES = ../../src/nco/ncap_lex.l
BISONSOURCES = ../../src/nco/ncap_yacc.y	 

flex.name = flex ${QMAKE_FILE_IN}
flex.input = FLEXSOURCES
flex.output = ../../src/nco/ncap_lex.c
flex.commands = flex -Pnco_yy -o ../../src/nco/ncap_lex.c ${QMAKE_FILE_IN}
flex.variable_out = SOURCES
silent:flex.commands = @echo Lex ${QMAKE_FILE_IN} && $$flex.commands
QMAKE_EXTRA_COMPILERS += flex

bison.name = bison ${QMAKE_FILE_IN}
bison.input = BISONSOURCES
bison.commands = bison -d -p nco_yy -o ../../src/nco/ncap_yacc.c ${QMAKE_FILE_IN}
bison.output = ../../src/nco/ncap_yacc.c
bison.variable_out = SOURCES
bison.CONFIG += target_predeps
silent:bison.commands = @echo Bison ${QMAKE_FILE_IN} && $$bison.commands
QMAKE_EXTRA_COMPILERS += bison

bisonheader.commands = bison -d -p nco_yy -o ../../src/nco/ncap_yacc.c ${QMAKE_FILE_IN}
bisonheader.input = BISONSOURCES
bisonheader.output = ../../src/nco/ncap_yacc.c
bisonheader.variable_out = HEADERS
bisonheader.name = bisonheader ${QMAKE_FILE_IN}
silent:bisonheader.commands = @echo Bison ${QMAKE_FILE_IN} && $$bison.commands
QMAKE_EXTRA_COMPILERS += bisonheader
}




