# Makefile for basic ASDL tests
#
# COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
# All rights reserved.
#
# src/tests/sequences/Makefile.  Generated from Makefile.in by configure.
#

SHELL =			/bin/sh


ASDL_LIBDIR =		/home/sarath/Documents/sml/asdl/lib

ASDLGEN =		/home/sarath/Documents/sml/asdl/bin/asdlgen
CXX =			g++
CXXFLAGS =		-g -O2
CPPFLAGS =		 -I/home/sarath/Documents/sml/asdl/include
LDFLAGS =		 -L$(ASDL_LIBDIR)
LIBS =			 -lasdl

CXX_SRCS =          	cxx-test.cxx test-spec.cxx
CXX_OBJS =          	$(patsubst %.cxx,%.o,$(CXX_SRCS))

build:		cxx-test sml-test

##### C++ test
#
cxx-test:	$(CXX_OBJS)
	$(CXX) -o $@ $(CXX_OBJS) $(LDFLAGS) $(LIBS)

%.o : %.cxx
	$(CXX) -c $(CPPFLAGS) $(CXXFLAGS) $<

cxx-test.cxx:		cxx-files
test-spec.cxx:		cxx-files

# generate the C++ implementation
#
.PHONY:		cxx-files
cxx-files:	test-spec.asdl
	$(ASDLGEN) c++ test-spec.asdl

##### SML test
#
sml-test:	sml-files

# generate the SML implementation
#
.PHONY:		sml-files
sml-files:	test-spec.asdl
	$(ASDLGEN) sml test-spec.asdl

.PHONY:		build

build:

#################### Cleanup ####################

CLEAN_FILES =		cxx-test \
			$(CXX_OBJS) \
			test-spec.hxx test-spec.cxx

DISTCLEAN_FILES +=      Makefile

DEVCLEAN_FILES =

include /home/sarath/Documents/sml/asdl/mk/clean-rules.gmk
