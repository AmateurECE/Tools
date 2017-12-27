###############################################################################
# NAME:		    Makefile
#
# AUTHOR:	    Ethan D. Twardy
#
# DESCRIPTION:	    This is the makefile. What do you think it does?
#
# CREATED:	    10/05/2017
#
# LAST EDITED:	    12/27/2017
###

TOP:=$(PWD)
CFLAGS = -g -Wall -O0 -Iinclude/
CC = gcc

SRCS += tree.c

OBJS = $(addprefix src/,$(patsubst %.c,%.o,$(SRCS)))

.PHONY: force clean

all: force flowdoc clean

force:

flowdoc: force $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o flowdoc

$(OBJS): force

clean:
	rm -f $(TOP)/src/*.o

###############################################################################
