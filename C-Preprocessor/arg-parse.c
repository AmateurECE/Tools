/******************************************************************************
 * NAME:	    arg-parse.c
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    This file contains most of the source code for an argument
 *		    parser that I built for a U-Boot script preprocessor. It
 *		    fully supports command line argument parsing. Spaces
 *		    between switches and switch arguments are optional.
 *		    Switches may have arguments or be boolean. In addition, up
 *		    to one argument may be passed without requiring a switch.
 *		    It is by no means a 'minimal example.'
 *
 * CREATED:	    09/25/2017
 *
 * LAST EDITED:	    12/27/2017
 ***/

/******************************************************************************
 * INCLUDES
 ***/

#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <errno.h>
#include <ctype.h>
#include <stdint.h>

#include "arg-parse.h"
#include "error.h"

/******************************************************************************
 * MACRO DEFINITIONS
 ***/

#define DEBUG

/******************************************************************************
 * STATIC FUNCTION PROTOTYPES
 ***/

static int try_get_clargs(int argc, char *** argv);
static void add_include(char * directory);
static void add_warning(char * warning);
#ifdef DEBUG
static void print_clargs(); /* For debugging, mostly. */
#endif
static void next_arg(int * pArgc, char *** pArgv);
static void destroy_clargs();

/******************************************************************************
 * GLOBAL VARIABLES
 ***/

struct parser_info clargs = (struct parser_info){
  .todo_warnings = true,    /* Default: Warn on /TODO/ */
};
int parsing_error = 0;

/******************************************************************************
 * MAIN
 ***/

int main(int argc, char * argv[])
{
  StopIf(argc < 2, 1, "Fatal Error: No input given.\n");

  int left = 0;
  /* Calling the function twice allows us to have one argument that doesn't
   * take a switch--the input file. */
  if ((left = try_get_clargs(argc, &argv)) > 0) {
    argv += (argc - left);
    clargs.infile = *argv;
    left = try_get_clargs(left, &argv);
  }

  StopIf(left > 0, 2,
	 "Fatal error: Could not parse command line arguments \"%s\"\n",
	 argv[argc - left - 1]);
  StopIf(clargs.infile == NULL, 11,
	 "Fatal error: Could not determine input file from command line"
	 " arguments.\n");

  /* Do ya thang */

#ifdef DEBUG
  print_clargs();
#endif
  destroy_clargs();

  return parsing_error;
}

/******************************************************************************
 * STATIC FUNCTIONS
 ***/

/******************************************************************************
 * FUNCTION:	    try_get_clargs
 *
 * DESCRIPTION:	    Get all command line arguments that are specified with a
 *		    flag. Upon encountering one that does not have a flag,
 *		    returns and main() attempts to parse the next arg as the
 *		    input file. Then main() attempts another call to this fn.
 *
 * ARGUMENTS:	    argc: (int) -- Straight from main().
 *		    argv: (char **) -- Straight from main().
 *
 * RETURN:	    int -- number of command line arguments left.
 *
 * NOTES:	    none.
 ***/
static int try_get_clargs(int argc, char *** args)
{
  char ** argv = *args;
  while (--argc > 0 && **++argv == '-') {
    while (*++*argv) {
      switch (**argv) {
      case 'o':
	next_arg(&argc, &argv);
	clargs.outfile = *argv;
	goto NXTARG;
      case 'I':
	next_arg(&argc, &argv);
	add_include(*argv);
	goto NXTARG;
      case 'W':
	next_arg(&argc, &argv);
	add_warning(*argv);
	goto NXTARG;
      default:
	StopIf(1, 5, "Fatal error: Unknown command line switch: \"%c\"\n",
	       **argv);
      }
    }
  NXTARG:
    ;
  }

  return argc;
}

/******************************************************************************
 * FUNCTION:	    add_includes
 *
 * DESCRIPTION:	    Add a directory name to the end of the member 'includedirs'
 *		    in the static info struct.
 *
 * ARGUMENTS:	    directory: (char *) -- the name of the directory.
 *
 * RETURN:	    void.
 *
 * NOTES:	    none.
 ***/
static void add_include(char * directory)
{
  if (clargs.includedirs == NULL) {
    clargs.includedirs = calloc(5, sizeof(char *));
    clargs.maxdirs = 5;
    clargs.numdirs = 0;
  } else if (clargs.numdirs == clargs.maxdirs) {
    char ** temp = calloc(clargs.numdirs, sizeof(char *));
    memcpy((void *)temp, (const void *)clargs.includedirs,
	   clargs.numdirs * sizeof(char *));
    clargs.includedirs = realloc(clargs.includedirs,
				 (clargs.maxdirs + 5) * sizeof(char *));
    memcpy((void *)clargs.includedirs, (const void *)temp,
	   clargs.numdirs * sizeof(char *));
  }

  clargs.includedirs[clargs.numdirs++] = directory;
}

/******************************************************************************
 * FUNCTION:	    add_warning
 *
 * DESCRIPTION:	    Toggles a warning switch in clargs, if the warning is
 *		    recognized.
 *
 * ARGUMENTS:	    warning: (char *) -- the warning to attempt a toggle of.
 *
 * RETURN:	    void.
 *
 * NOTES:	    none.
 ***/
static void add_warning(char * warning)
{
  StopIf(warning == NULL, 6, "Fatal error: -W switch takes an argument.\n");

  if (!strcmp(warning, "no-TODO")) {
    clargs.todo_warnings = false;
  } else {
    StopIf(1, 7, "Fatal error: Unknown command line argument \"W%s\"",
	   warning);
  }
}

/******************************************************************************
 * FUNCTION:	    print_clargs
 *
 * DESCRIPTION:	    Prints out the struct 'clargs' in a pretty way. Mostly for
 *		    debugging.
 *
 * ARGUMENTS:	    none.
 *
 * RETURN:	    void.
 *
 * NOTES:	    none.
 ***/
#ifdef DEBUG
static void print_clargs()
{
  printf("Input file: %s\n", clargs.infile);
  printf("Output file: %s\n", clargs.outfile);
  printf("Included directories:\n");
  for (int i = 0; i < clargs.numdirs; i++)
    printf("\t%s\n", clargs.includedirs[i]);
  printf("Number of included directories: %d\n", (int)clargs.numdirs);
  printf("TODO Warnings: %s\n", clargs.todo_warnings ? "true" : "false");
}
#endif

/******************************************************************************
 * FUNCTION:	    next_arg
 *
 * DESCRIPTION:	    Increments the argument (argv) to the next argument. This
 *		    function implements the optional space between command line
 *		    switches and their arguments.
 *
 * ARGUMENTS:	    pArgc: (int *) -- pointer to argc.
 *		    pArgv: (char ***) -- pointer to argv.
 *
 * RETURN:	    void.
 *
 * NOTES:	    none.
 ***/
static void next_arg(int * pArgc, char *** pArgv)
{
  if (*++**pArgv == '\0') {
    (*pArgv)++;
    (*pArgc)--;
  }
}

/******************************************************************************
 * FUNCTION:	    destroy_clargs
 *
 * DESCRIPTION:	    Free memory held by clargs, the static info-struct.
 *
 * ARGUMENTS:	    None.
 *
 * RETURN:	    void.
 *
 * NOTES:	    As members are added to clargs that require freeing, they
 *		    will be freed here.
 ***/
static void destroy_clargs()
{
  free(clargs.includedirs);
}

/*****************************************************************************/
