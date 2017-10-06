/*******************************************************************************
 * NAME:	    main-parse.h
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

#ifndef __MAIN_PARSE_H__
#define __MAIN_PARSE_H__

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

#endif /* __MAIN_PARSE_H__ */

/******************************************************************************/
