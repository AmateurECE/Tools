/******************************************************************************
 * NAME:	    test.c
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    This source file is a generic testing framework for C APIs.
 *		    To use this file, sed replace every instance of 'module'
 *		    (case sensitive) with the name of your program. Include it
 *		    in your Makefile, and be sure to define the macro 
 *		    CONFIG_DEBUG_MODULE. You can define the macro
 *		    CONFIG_LOG_FILENAME to specify the name of the log file.
 *
 * CREATED:	    01/19/2018
 *
 * LAST EDITED:	    01/19/2018
 ***/

/******************************************************************************
 * INCLUDES
 ***/

#ifdef CONFIG_DEBUG_MODULE
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <errno.h>
#endif

/******************************************************************************
 * MACRO DEFINITIONS
 ***/

#ifdef CONFIG_DEBUG_MODULE
#define FAIL "\033[1;31m"

/* This is specific to my terminal, I think. But since it only affects the
 * colors of the test output, it doesn't really matter
 */
#ifdef __APPLE__
#   define PASS "\033[1;32m"
#else
#   define PASS "\033[1;39m"
#endif /* __APPLE__ */

#define NC	"\033[0m"

#define error_exit(...) {			\
    fprintf(stderr,  __VA_ARGS__);		\
    printf("\n");				\
    exit(1);					\
  }

#ifdef CONFIG_TEST_LOG
#   ifndef CONFIG_LOG_FILENAME
#	define CONFIG_LOG_FILENAME "./log.txt"
#   endif

#   define log(...) fprintf(logfile, __VA_ARGS__);
#   define log_fail(...) {			\
    log(__VA_ARGS__);				\
    failures++;					\
    return 0;					\
  }

#   define CONFIG_LOG_PREAMBLE			\
  "MODULE - TEST LOG\n"				\
  "%s\n"					\
  "====================\n\n"
#else
#   define log(...)
#   define log_fail(...)
#endif /* CONFIG_TEST_LOG */
#endif /* CONFIG_DEBUG_MODULE */

/******************************************************************************
 * STATIC VARIABLES
 ***/

#ifdef CONFIG_TEST_LOG
static FILE * logfile = NULL;
#endif

static int failures = 0;

/******************************************************************************
 * LOCAL PROTOTYPES
 ***/

static int module_test();

/******************************************************************************
 * MAIN
 ***/

#ifdef CONFIG_DEBUG_MODULE
int main(int argc, char * argv[])
{
#ifdef CONFIG_TEST_LOG
  /* Initialize log */
  printf("Initializing log in '"CONFIG_LOG_FILENAME"'...\n");
  logfile = fopen(CONFIG_LOG_FILENAME, "w");
  if (logfile == NULL)
    error_exitf("Could not initialize log file: %s\n", strerror(errno));
  char * buff = NULL;
  if ((buff = malloc(64)) == NULL)
    error_exit("Could not allocate space for time buffer.");
  time_t raw;
  time(&raw);
  strftime(buff, 63, "%+", localtime(&raw));
  log(CONFIG_LOG_PREAMBLE, buff);
  free(buff);
#endif  

  printf("Test test (module_test):\t%s\n",

	 module_test() ? PASS"PASS"NC : FAIL"FAIL"NC
	 );

#ifdef CONFIG_TEST_LOG
  /* Close log */
  fprintf("END OF LOG\n");
  printf("Closing log file...\n");
  fclose(logfile);
#endif

  return failures;
}
#endif

/******************************************************************************
 * LOCAL FUNCTIONS
 ***/

/******************************************************************************
 * FUNCTION:	    module_test
 *
 * DESCRIPTION:	    tests the module. (Replace this with your description).
 *
 * ARGUMENTS:	    none.
 *
 * RETURN:	    int -- 1 if the test passes, 0 otherwise.
 *
 * NOTES:	    Test cases:
 *			1 - (None).
 ***/
static int module_test()
{
  return 1 ? 1 : log_fail("module_test: 1 failed--true is false.\n");
}

/*****************************************************************************/
