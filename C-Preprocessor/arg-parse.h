/*******************************************************************************
 * NAME:	    arg-parse.h
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    This file is part of the argument parser/main() routine
 *		    found in arg-parse.c. This header contains the declaration
 *		    of the argument struct, so that its members may be used in
 *		    other translation units.
 *
 * CREATED:	    10/02/2017
 *
 * LAST EDITED:	    10/23/2017
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
extern char * yyin_filename; /* Name of the current parsed file */
extern bool parsing_error;

#endif /* __ARG_PARSE_H__ */

/******************************************************************************/
