/*******************************************************************************
 * NAME:	    index-test.c
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    This file parses the .git/index file to see if there are
 *		    untracked files in the working tree.
 *
 * CREATED:	    11/02/2017
 *
 * LAST EDITED:	    11/02/2017
 ***/

/*******************************************************************************
 * INCLUDES
 ***/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

/*******************************************************************************
 * MACRO DEFINITIONS
 ***/

#define ERROR_CODE  "\033[1;31m"
#define NC_CODE	    "\033[0m"

#define StopIf(cond, code, ...) {					\
    if (cond) {								\
      fprintf(stderr, ERROR_CODE"Error: "NC_CODE __VA_ARGS__);		\
      exit(code);							\
    }									\
  }

/*******************************************************************************
 * STATIC FUNCTION PROTOTYPES
 ***/

static size_t fngetc(FILE * file, char ** dest, size_t num);

/*******************************************************************************
 * API FUNCTIONS
 ***/

int index_verify(FILE * gitfile)
{
  if (!fseek(gitfile, 0, SEEK_SET))
    StopIf(1, 1, "Could not seek to the beginning of the file.\n");

  char arr[5], *test = NULL;
  int index = 0, entries = 0;
  while (fngetc(gitfile, (char **)&arr, 4) == 4) {
    switch(index++) {
    case 0:
      if (strncmp(arr, "DIRC", 4))
	return 0; /* TODO: Return -1 if it's not a valid index file. */
      break;
    case 1:
      if (arr[3] != 0x02)
	return 0;
      break;
    case 2:
      entries = strtoul(arr, &test, 16);
      StopIf((*test != '\0' || errno & (ERANGE | EINVAL)), 1,
	     "Number conversion error in index_verify().\n");
      break;
    default:
      if (isalpha(arr[0]) && isupper(arr[0])) {
	if (!fseek(gitfile, 4, SEEK_CUR))
	  StopIf(1, 1, "fseek() error in index_verify().\n");	
      } else {
	if (!fseek(gitfile, 16, SEEK_CUR))
	  StopIf(1, 1, "fseek() error in index_verify().\n");
	return entries; /* We're done here */
      }
    }
  }

  return 0; /* Should never get here */
}

/*******************************************************************************
 * MAIN
 ***/

int main(int argc, char * argv[])
{
  StopIf(argc < 2, 1, "Must specify a file.\n");

  FILE * gitfh = fopen(argv[1], "rb");
  StopIf(gitfh == NULL, 1, "The specified file cannot be opened (%d): %s\n",
	 errno, strerror(errno));
  StopIf(!index_verify(gitfh), 1, "The specified file is not compatible with"
	 " this program.\n");

  
}

/*******************************************************************************
 * STATIC FUNCTIONS
 ***/

static size_t fngetc(FILE * file, char ** dest, size_t num)
{
  char * string = NULL;
  if ((string = malloc(num)) == NULL)
    StopIf(1, 1, "Malloc error in fngetc.\n");

  memset(string, 0x5a, num);
  if (strncpy(*dest, string, num) == NULL) {
    free(string);
    Stopif(1, 1, "strncpy() error in fngetc.\n");
  } else if (*(*dest + num) != '\0' && *(*dest + num) == 0x5a) {
    return -1;
  }
  free(string);

  /* At this point, we know that the buffer has at least sufficient size. */
  size_t i = 0;
  while ((i++) < num) {
    unsigned char c = (unsigned char)fgetc(file);
    if (c == (unsigned char)EOF)
      return i;
    *(*dest + i) = c;
  }

  return i;
}

/******************************************************************************/
