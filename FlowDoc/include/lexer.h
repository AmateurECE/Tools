/*******************************************************************************
 * NAME:	    lexer.h
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    This file contains the public interface for the source code
 *		    contained in lexer.c.
 *		    TODO: Fix lines to 79 chars.
 *
 * CREATED:	    09/29/2017
 *
 * LAST EDITED:	    12/27/2017
 ***/

#ifndef __ET_LEXER_H__
#define __ET_LEXER_H__

/*******************************************************************************
 * INCLUDES
 ***/

#include <stdio.h>

#include "tree.h"

/*******************************************************************************
 * TYPE DEFINITIONS
 ***/

/* Insert Struct definition containing some tree nodes? */

/*******************************************************************************
 * API FUNCTION PROTOTYPES
 ***/



int lexer_parse(FILE * fp);


#endif /* __ET_LEXER_H__ */

/******************************************************************************/
