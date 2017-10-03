/*******************************************************************************
 * NAME:	    arg-parse.h
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    This file contains the declaration of a few symbols useful
 *		    in other files.
 *
 * CREATED:	    10/02/2017
 *
 * LAST EDITED:	    10/02/2017
 ***/

#ifndef __ARG_PARSE_H__
#define __ARG_PARSE_H__

/*******************************************************************************
 * INCLUDES
 ***/

#include <stdbool.h>

/*******************************************************************************
 * TYPE DEFINITONS
 ***/

/* Example struct */
struct parser_info {
  char * infile;
  char * outfile;

  char ** includedirs;
  size_t numdirs;
  size_t maxdirs;

  bool todo_warnings;
};

/*******************************************************************************
 * EXTERNAL REFERENCES
 ***/

extern struct parser_info clargs; /* The data parsed from the command line */

#endif /* __ARG_PARSE_H__ */

/******************************************************************************/
