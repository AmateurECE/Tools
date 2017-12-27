/*******************************************************************************
 * NAME:	    hash.h
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    This file contains the public interface for the very simple
 *		    hash table implemented in hash.c. This implementation
 *		    requires that the table's capacity be a power of two. If the
 *		    user does not provide this, hash_create() will round up to
 *		    the next power of two. Because of this, it is suggested that
 *		    the user utilize the ISFULL() macro to check if the table
 *		    is full.
 *		    TODO: Fix lines to 79 chars.
 *
 * CREATED:	    10/04/2017
 *
 * LAST EDITED:	    12/27/2017
 ***/

#ifndef __HASH_H__
#define __HASH_H__

/*******************************************************************************
 * INCLUDES
 ***/

#include <stdlib.h>

/*******************************************************************************
 * MACRO DEFINITIONS
 ***/

#define ISFULL(hash)	((hash)->size == (hash)->capacity)
#define ISEMPTY(hash)	((hash)->size == 0)

/*******************************************************************************
 * TYPE DEFINITIONS
 ***/

typedef struct {

  size_t size;
  size_t capacity;

  void (*destroy)(void *);
  void * (*compare)(void *, void *);
  size_t (*hashfn)(const void *);

  struct {
    void * value;
    int vacated; /* Field for internal use only */
  } **buckets;

} hash_t;

/*******************************************************************************
 * API FUNCTION PROTOTYPES
 ***/

extern hash_t * hash_create(size_t capacity,
			    void (*destroy)(void *),
			    void * (*compare)(void *, void *),
			    size_t (*hashfn)(const void *));
extern int hash_insert(hash_t * hash, void * key);
extern void * hash_get(hash_t * hash, void * key);
extern int hash_remove(hash_t * hash, void * key);
extern void hash_traverse(hash_t * hash, void (*func)(void *));
extern void hash_destroy(hash_t ** hash);

#endif /* __HASH_H__ */

/******************************************************************************/
