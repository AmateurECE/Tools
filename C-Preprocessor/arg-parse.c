/*******************************************************************************
 * NAME:	    main-parse.c
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
 * TODO: @if (<condition>) token.
 * TODO: -D flag
 */

/*******************************************************************************
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

#include "grammar.tab.h"
#include "main-parse.h"
#include "define.h"
#include "error.h"

/*******************************************************************************
 * MACRO DEFINITIONS
 ***/

/* #define DEBUG */

/*******************************************************************************
 * EXTERNAL REFERENCES
 ***/

/* flex does not export a header file, so we must use external refs to get at
 * its symbols. */
extern FILE * yyin;
extern FILE * yyout;
extern int yyparse();
extern int yylex_destroy(void);

/*******************************************************************************
 * STATIC FUNCTION PROTOTYPES
 ***/

static int try_get_clargs(int argc, char *** argv);
static void add_include(char * directory);
static void add_warning(char * warning);
static void add_define(char * define);
#ifdef DEBUG
static void print_clargs(); /* For debugging, mostly. */
#endif
static void next_arg(int * pArgc, char *** pArgv);
static void destroy_clargs();

/*******************************************************************************
 * GLOBAL VARIABLES
 ***/

struct parser_info clargs = (struct parser_info){
  .todo_warnings = true,    /* Default: Warn on /TODO/ */
};
char * yyin_filename;
bool parsing_error = false;

/*******************************************************************************
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

  yyin = fopen(clargs.infile, "r");
  StopIf(yyin == NULL, 3, "Fatal error: Could not open input file.\n");
  yyin_filename = clargs.infile;

  if (clargs.outfile) {
    yyout = fopen(clargs.outfile, "w");
    StopIf(yyout == NULL, 4,
	   "Fatal error: Could not open output file.\n");
  }

  yyparse();
  yylex_destroy();
  destroy_clargs();

  return (int)parsing_error;
}

/*******************************************************************************
 * STATIC FUNCTIONS
 ***/

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
      case 'D':
	next_arg(&argc, &argv);
	add_define(*argv);
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

/*******************************************************************************
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
  StopIf(warning == NULL, 6, "Fatal error: -W switch takes an argument.\n");

  if (!strcmp(warning, "no-TODO")) {
    clargs.todo_warnings = false;
  } else {
    StopIf(1, 7, "Fatal error: Unknown command line argument \"W%s\"", warning);
  }
}

/*******************************************************************************
 * FUNCTION:	    add_define
 *
 * DESCRIPTION:	    This function adds a -D flag argument to the clargs struct.
 *		    These arguments come in the form of either:
 *
 *			-D ?[[:word:]]+(?:=[[:word:]]+)
 *
 *		    Where the space and suffix is optional. The first word is a
 *		    name, the name of a define'd token. This is optionally
 *		    followed by a '=' and another sequence of non-space chars
 *		    to be used as the value for the define'd token. This
 *		    functions similarly to the #define token in C, except it
 *		    cannot be used to perform text substitutions outside the
 *		    context of other preprocessor directives.
 *
 * ARGUMENTS:	    define: (char *) -- the argument to the -D flag.
 *
 * RETURN:	    void.
 *
 * NOTES:	    none.
 ***/
static void add_define(char * define)
{
  StopIf(define == NULL, 12, "Fatal error: -D switch takes an argument.\n");
  char * c = strchr(define, '=');
  size_t size;
  int num;

  if (c == NULL) { /* Case of -D<VAR> */
    goto do_null;
  } else {
    *(c++) = '\0';
    char * nul = strchr(c, '\0');
    if (nul == NULL)
      goto error_exit;
    size = (uintptr_t)nul - (uintptr_t)c;
    if (size == 0) /* Happens in the case of -D<VAR>= */
      goto do_null;

    if (isxdigit(*c)) {
      int dodigit = 1;
      for (int i = 1; i < size; i++) {
	if (i == 1 && c[1] == 'x') /* Case of -D <VAR>=0x... */
	  continue;
	if (!isxdigit(c[i])) {
	  dodigit = 0;
	  break;
	}
      }

      if (dodigit) { /* If we have a hex string */
	errno = 0;
	num = strtoul(c, NULL, 16);
	if (errno & (EINVAL | ERANGE))
	  goto error_exit;
	goto do_int;
      } else { /* if (dodigit) {... */
	goto do_string;
      }
    } else { /* if (isxdigit(*c)) {... */
      goto do_string;
    }
  }

 do_null:
  if (define_insert(define, NULL, 0, OTHER_T)) goto error_exit;
  return;
 do_string:
  if(define_insert(define, c, size, STR_T)) goto error_exit;
  return;
 do_int:
  if (define_insert(define, &num, sizeof(int), INT_T)) goto error_exit;
  return;

 error_exit:
  StopIf(1, 19, "Fatal error when parsing -D switch.\n");
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
  define_print(stdout);
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
  free(clargs.includedirs);

  if (define_hash != NULL)
    define_destroy();
}

/******************************************************************************/
