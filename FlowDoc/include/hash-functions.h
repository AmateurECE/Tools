/*******************************************************************************
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
 * LAST EDITED:	    10/03/2017
 ***/

#ifndef __ET_HASH_FUNCTIONS_H__
#define __ET_HASH_FUNCTIONS_H__

/*******************************************************************************
 * API FUNCTION PROTOTYPES
 ***/

/**
 * \brief This function computes the hash of the pointer value
 * \param data The pointer to use as a starting value.
 * \return int The hashed value.
 */
extern int hash_double(const void * data);

#endif /* __ET_HASH_FUNCTIONS_H__ */

/******************************************************************************/
