/*******************************************************************************
 * NAME:	    hash.h
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    This file contains the public interface for the hashmap
 *		    source code in hash.c. This hash is designed to work with
 *		    the tree that is in tree.h, so it has a few caveats. It
 *		    is an associative hash, meaning that the interface functions
 *		    are passed a data point and the hashing function will return
 *		    a pointer to that data. This is used in conjunction with
 *		    the tree to prevent bloating of the tree. Because of this,
 *		    this implementation is not very useful when it comes to data
 *		    serialization.
 *
 * CREATED:	    10/02/2017
 *
 * LAST EDITED:	    10/02/2017
 ***/

#ifndef __ET_HASH_H__
#define __ET_HASH_H__

/*******************************************************************************
 * TYPE DEFINTIONS
 ***/

typedef struct {
  
  size_t size;
  size_t capacity;
  int (*hash)(const void * key);
  void * (*compare)(const void * q, const void * r);
  void (*destroy)(void * q);
  bucket_t ** buckets;

} hash_t;

typedef struct {

  void * key; /* The key to compare the key given (Ensure data is correct) */
  void * data; /* Data */
  
} bucket_t;

/*******************************************************************************
 * API FUNCTION PROTOTYPES
 ***/

extern hash_t * hash_create(void * (*compare)(const void *, const void *),
		     void (*destroy)(void *),
		     size_t capacity);
extern int hash_insert(hash_t * hash, void * key, void * data);
extern void * hash_get(hash_t * hash, void * key);
extern int hash_remove(hash_t * hash, void * key);
extern void hash_destroy(hash_t * hash);

#endif /* __ET_HASH_H__ */

/******************************************************************************/
