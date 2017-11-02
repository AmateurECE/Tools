/*******************************************************************************
 * NAME:	    error.h
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    This file contains a few macros that are helpful for
 *		    handling errors. Use these by invoking parse_error(), which
 *		    is a macro defined here.
 *
 * CREATED:	    09/26/2017
 *
 * LAST EDITED:	    09/26/2017
 ***/

#ifndef __ERROR_H__
#define __ERROR_H__

/*******************************************************************************
 * INCLUDES
 ***/

#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "arg-parse.h"

/*******************************************************************************
 * EXTERNAL REFERENCES
 ***/

extern int yylineno;
extern char * yytext;

/*******************************************************************************
 * MACRO DEFINITIONS
 ***/

#define ERROR	1
#define WARN	0

#define ERROR_CODE  "\033[1;31m"
#define BOLD_CODE   "\033[1m"
#define NC_CODE	    "\033[0m"
#define WARN_CODE   "\033[1;35m"

#define ERROR_PRINT ""BOLD_CODE"%s:%d:"ERROR_CODE" error: "NC_CODE
#define WARN_PRINT  ""BOLD_CODE"%s:%d:"WARN_CODE" warning: "NC_CODE

#define parse_error(action, ...)		\
    IF_ELSE(action)(				\
	PARSE_ERROR(__VA_ARGS__)		\
    )(						\
	PARSE_WARN(__VA_ARGS__)			\
    )

#define internal_error(action, ...)		\
    IF_ELSE(action)(				\
	INTERNAL_ERROR(__VA_ARGS__)		\
    )(						\
	INTERNAL_WARN(__VA_ARGS__)		\
    )

#define INTERNAL_ERROR(format, ...)	{				\
    PARSE_DO_FPRINTF(stderr, ERROR_PRINT""format"\n\n",			\
		     yyin_filename, yylineno				\
		     IF_ELSE(HAS_ARGS(__VA_ARGS__))			\
		     (, __VA_ARGS__)());				\
    parsing_error = true;						\
}

#define INTERNAL_WARN(format, ...)	{			\
    PARSE_DO_FPRINTF(stderr, WARN_PRINT""format"\n\n",		\
		     yyin_filename, yylineno			\
		     IF_ELSE(HAS_ARGS(__VA_ARGS__))		\
		     (, __VA_ARGS__)());			\
}

#define PARSE_WARN(format, ...)	    {				\
    PARSE_DO_FPRINTF(stderr, WARN_PRINT""format"\n\t%s\n\t^\n",	\
		     yyin_filename, yylineno,			\
		     IF_ELSE(HAS_ARGS(__VA_ARGS__))		\
		     (__VA_ARGS__,)() yytext);			\
}

#define PARSE_ERROR(format, ...)    {					\
    PARSE_DO_FPRINTF(stderr, ERROR_PRINT""format"\n\t%s\n\t^\n",	\
		     yyin_filename, yylineno,				\
		     IF_ELSE(HAS_ARGS(__VA_ARGS__))			\
		     (__VA_ARGS__,)() yytext);				\
    parsing_error = true;						\
}

#define PARSE_DO_FPRINTF(...)	fprintf(__VA_ARGS__)

#define StopIf(condition, code, ...) {		\
    if (condition)				\
      fprintf(stderr, __VA_ARGS__);		\
    if (condition && code)			\
      exit(code);				\
}

/* The following macros implement the macro-logic */

#define FIRST(a, ...) a
#define SECOND(a, b, ...) b

#define IS_PROBE(...) SECOND(__VA_ARGS__, 0)
#define PROBE() ~, 1

#define CAT(a,b) a ## b

#define NOT(x) IS_PROBE(CAT(_NOT_, x))
#define _NOT_0 PROBE()

#define BOOL(x) NOT(NOT(x))

#define IF_ELSE(condition) _IF_ELSE(BOOL(condition))
#define _IF_ELSE(condition) CAT(_IF_, condition)

#define _IF_1(...) __VA_ARGS__ _IF_1_ELSE
#define _IF_0(...)             _IF_0_ELSE

#define _IF_1_ELSE(...)
#define _IF_0_ELSE(...) __VA_ARGS__

#define HAS_ARGS(...) BOOL(FIRST(_END_OF_ARGUMENTS_ __VA_ARGS__)())
#define _END_OF_ARGUMENTS_() 0

#endif /* __ERROR_H__ */

/******************************************************************************/
