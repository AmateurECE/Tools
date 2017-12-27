/******************************************************************************
 * NAME:	    sanitize.h
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    This is an idea for a macro I had that would declare a fn
 *		    which sanitizes inputs to a function based on supplied bool
 *		    expressions. It is available in the public domain.
 *
 * CREATED:	    09/12/2017
 *
 * LAST EDITED:	    12/27/2017
 ***/

#ifndef __ET_SANITIZE_H__
#define __ET_SANITIZE_H__

/******************************************************************************
 * MACRO DEFINITIONS
 ***/

#define SAN_FN4(def1, bool1, )

#define GET_MACRO(_1, _2, _3, _4, NAME, ...) NAME

#define Declare_Sanitizer(...)						\
  GET_MACRO(__VA_ARGS__, SAN_FN4, SAN_FN3, SAN_FN2, SAN_FN1)(__VA_ARGS__)

#define RECURSIVE_MACRO(_1, ...) _1; RECURSIVE_MACRO(__VA_ARGS__)
  
#endif /* __ET_SANITIZE_H__ */

/*****************************************************************************/
