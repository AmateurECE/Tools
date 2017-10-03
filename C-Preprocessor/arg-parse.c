/*******************************************************************************
 * NAME:	    arg-parse.c
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    C source file containing the main function for the parser
 *		    in lex.yy.c or like.
 *
 * CREATED:	    09/25/2017
 *
 * LAST EDITED:	    09/25/2017
 ***/

/*
 * TODO: manpages?
 * TODO: Add call to mkimage.
 * TODO: -E switch, which performs only preprocessing.
 */

/*******************************************************************************
 * INCLUDES
 ***/

#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "grammar.tab.h"
#include "arg-parse.h"

/*******************************************************************************
 * MACRO DEFINITIONS
 ***/

/* #define DEBUG */

/*******************************************************************************
 * STATIC FUNCTION PROTOTYPES
 ***/

static inline void error_exit(char * s);
static int try_get_clargs(int argc, char *** argv);
static void add_include(char * directory);
static void add_warning(char * warning);
#ifdef DEBUG
static void print_clargs(); /* For debugging, mostly. */
#endif
static void next_arg(int * pArgc, char *** pArgv);
static void destroy_clargs();

/*******************************************************************************
 * GLOBAL VARIABLES
 ***/

struct parser_info clargs = (struct parser_info){ .todo_warnings = true };

/*******************************************************************************
 * MAIN
 ***/

int main(int argc, char * argv[])
{
  if (argc < 2)
    error_exit("No arguments given.");

  int left = 0;
  if ((left = try_get_clargs(argc, &argv)) > 0) {
    argv += (argc - left);
    clargs.infile = strdup(*argv);
    left = try_get_clargs(left, &argv);
  }

  if (left > 0) {
    fprintf(stderr, "Error when parsing command line arguments: %s\n",
	    argv[argc - left - 1]);
    exit(1);
  }

  print_clargs();
  destroy_clargs();
}

/*******************************************************************************
 * STATIC FUNCTIONS
 ***/

/*******************************************************************************
 * FUNCTION:	    error_exit
 *
 * DESCRIPTION:	    Prints a message to STDERR, then exits with a status of 1.
 *
 * ARGUMENTS:	    s: (char *) -- the string to print to STDERR.
 *
 * RETURN:	    void. No Return.
 *
 * NOTES:	    none.
 ***/
static inline void error_exit(char * s)
{
  fprintf(stderr, "%s\n", s);
  exit(1);
}

/*******************************************************************************
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
	clargs.outfile = strdup(*argv);
	goto NXTARG;
      case 'I':
	next_arg(&argc, &argv);
	add_include(*argv); /* *++argv */
	goto NXTARG;
      case 'W':
	next_arg(&argc, &argv);
	add_warning(*argv);
	goto NXTARG;
      }
    }
  NXTARG:
    ;
  }

  return argc;
}

/*******************************************************************************
 * FUNCTION:	    add_include
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

/*******************************************************************************
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
  if (warning == NULL)
    return;

  if (!strcmp(warning, "no-TODO")) {
    clargs.todo_warnings = false;
  } else {
    char * errorstr = NULL;
    if (asprintf(&errorstr,
		 "Unknown command line argument \"-W%s\"", warning) >= 0)
      error_exit("Error when copying string");
    else
      error_exit(errorstr);
  }
}

/*******************************************************************************
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

/*******************************************************************************
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

/*******************************************************************************
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
  free(clargs.infile);
  free(clargs.outfile);

  /* ... */
}

/******************************************************************************/
