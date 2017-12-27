/******************************************************************************
 * NAME:	    hash-functions.c
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    This function implements the function interface presented
 *		    in hash-functions.h.
 *		    TODO: Remove this file.
 *
 * CREATED:	    10/03/2017
 *
 * LAST EDITED:	    12/27/2017
 ***/

/******************************************************************************
 * INCLUDES
 ***/

#include "hash-functions.h"

/******************************************************************************
 * API FUNCTIONS
 ***/

size_t str_hash1(char * string, size_t len)
{
  size_t hash = 0, pos = 0;
  while (*(string++) != '\0' && pos++ < len) {
    hash |= (*string) << (pos % sizeof(size_t));
  }
  return hash;
}

size_t str_hash2(char * str)
{
    size_t hash = 5381;
    while (*(str++) != '\0')
        hash = ((hash << 5) + hash) + *str; /* hash * 33 + c */
    return hash;
}

/*****************************************************************************/
