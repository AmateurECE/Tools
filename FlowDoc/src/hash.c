/*******************************************************************************
 * NAME:	    hash.c
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    This file contains the source code implementation of the
 *		    functions described in hash.h.
 *
 * CREATED:	    10/03/2017
 *
 * LAST EDITED:	    10/03/2017
 ***/

/*******************************************************************************
 * INCLUDES
 ***/

#include "hash.h"
#include "hash-functions.h"

/*******************************************************************************
 * API FUNCTIONS
 ***/

/*******************************************************************************
 * FUNCTION:	    hash_create
 *
 * DESCRIPTION:	    This function creates a hash_t structure and returns a
 *		    pointer to it.
 *
 * ARGUMENTS:	    compare: (void * (*)(const void *, const void *)) -- This is
 *			a pointer to a user defined function which compares the
 *			data referenced by the two pointers given as arguments,
 *			and returns the pointer which is greater, or NULL if
 *			they are the same.
 *		    destroy: (void (*)(void *)) -- pointer to a user defined
 *			function which will dispose of a data pointer, or NULL
 *			if the memory should remain untouched.
 *		    capacity: (size_t) -- the capacity of the hash table. It
 *			would be possible to allow the hash table to grow and
 *			shrink dynamically, but this functionality is left to
 *			the user.
 *
 * RETURN:	    hash_t * -- pointer to the hash struct, or NULL otherwise.
 *
 * NOTES:	    none.
 ***/
hash_t * hash_create(void * (*compare)(const void *, const void *),
		     void (*destroy)(void *),
		     size_t capacity)
{
  hash_t newhash = malloc(sizeof(hash_t));
  newhash = (hash_t){
    .capacity = capacity,
    .compare = compare,
    .destroy = destroy,
    .hash = hash_double, /* Defined in hash-functions.c */
    .size = 0
  };

  if ((newhash.buckets = calloc(capacity, sizeof(bucket_t *))) == NULL) {
    free(newhash);
    return NULL;
  }

  return &newhash;
}

/******************************************************************************/
