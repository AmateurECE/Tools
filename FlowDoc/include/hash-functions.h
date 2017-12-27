/******************************************************************************
 * NAME:	    hash-functions.h
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    This function contains the public interface for the
 *		    hash function defined in hash-functions.c. These functions
 *		    present a method of double-hashing, which does its best to
 *		    distribute data even in the event of multiple collisions.
 *
 * CREATED:	    10/03/2017
 *
 * LAST EDITED:	    12/27/2017
 ***/

#ifndef __ET_HASH_FUNCTIONS_H__
#define __ET_HASH_FUNCTIONS_H__

/******************************************************************************
 * API FUNCTION PROTOTYPES
 ***/

extern size_t str_hash1(char * string, size_t len);
extern size_t str_hash2(char * string);

#endif /* __ET_HASH_FUNCTIONS_H__ */

/*****************************************************************************/
