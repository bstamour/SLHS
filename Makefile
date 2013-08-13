#--------------------------------------------------------------------------------------
# Settings.

CXX = g++
LD = g++
CXXFLAGS = -std=c++1y -O3 -Wall -Wextra
CXXLNFLAGS = -L/usr/lib64 -lpqxx -lpq -lboost_program_options

SRCS = main.cxx
OBJS = $(SRCS:.cxx=.o)
EXE = main

.SUFFIXES:
.SUFFIXES: .cxx .o

.cxx.o :
	$(CXX) $(CXXFLAGS) -c $<

all : $(EXE)

#--------------------------------------------------------------------------------------
# A simple way to keep track of header dependencies.

depend: .depend

.depend: $(SRCS)
	rm -f ./.depend
	$(CXX) $(CXXFLAGS) -MM $^ > ./.depend;

include .depend

#--------------------------------------------------------------------------------------
# Targets.

$(EXE) : $(OBJS)
	$(LD) -o $@ $(OBJS) $(CXXLNFLAGS)

clean :
	rm -f $(EXE) $(OBJS) .depend
