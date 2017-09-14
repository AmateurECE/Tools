/*******************************************************************************
 * NAME:	    test.c
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    
 *
 * CREATED:	    09/12/2017
 *
 * LAST EDITED:	    09/12/2017
 ***/

#include "map_working.h"

#define CREATE_FN(name) \
  int name(void);	\

EVAL(MAP(CREATE_FN, fn1, fn2, fn3, fn4))
