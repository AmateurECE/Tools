/*******************************************************************************
 * NAME:	    hash.c
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    This file contains the source code implementation of the
 *		    interface described in hash.h. This is a very simple
 *		    implementation of a hashing table, used to keep track of
 *		    preprocessor tokens created with @define tokens, and used
 *		    in @if (etc...) tokens.
 *		    TODO: Remove this file.
 *
 * CREATED:	    10/04/2017
 *
 * LAST EDITED:	    12/27/2017
 ***/

/* TODO: Make the hash dynamically sizeable. */

/*******************************************************************************
 * INCLUDES
 ***/

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

#include "hash.h"
#include "error.h"

/*******************************************************************************
 * MACRO DEFINITIONS
 ***/

#define HASH(k, i, m)	((HASH1((k), (m)) + (i * HASH2((k), (m)))) % (m))
#define HASH1(k, m)	((k) % (m))
#define HASH2(k, m)	HASH3(k, m) & 0x1 ? HASH3(k, m) : (HASH3(k, m) + 1)
#define HASH3(k, m)	((k) % ((m) - 1))
#define IS_POF2(x)	(((x) != 0) && (((x) & ((x) - 1)) == 0))
#define KEY(hash, x)	((hash)->buckets[(x)]->value)
#define VACATED(hash, x)    ((hash)->buckets[(x)]->vacated)

/* #define DEBUG */

#ifdef DEBUG
#define debug(...) fprintf(stderr, __VA_ARGS__)
#else
#define debug(...)
#endif

/*******************************************************************************
 * TYPE DEFINITIONS
 ***/

typedef struct {
  void * value;
  int vacated;
} bucket_t;

/*******************************************************************************
 * STATIC FUNCTION PROTOTYPES
 ***/

static size_t next_pof2(size_t);

/*******************************************************************************
 * API FUNCTIONS
 ***/

/*******************************************************************************
 * FUNCTION:	    hash_create
 *
 * DESCRIPTION:	    Allocates a new hash_t struct and returns a pointer to it.
 *
 * ARGUMENTS:	    capacity: (size_t) -- the capacity of the hash.
 *		    destroy: (void (*)(void *)) -- pointer to a user-defined
 *			function that will free any memory used by the data/key
 *			parameters, or NULL if the memory should remain in use.
 *
 * RETURN:	    hash_t * -- pointer to the new hash_t struct, or NULL.
 *
 * NOTES:	    none.
 ***/
hash_t * hash_create(size_t capacity,
		     void (*destroy)(void *),
		     void * (*compare)(void *, void *),
		     size_t (*hashfn)(const void *))
{
  if (capacity == 0 || compare == NULL || hashfn == NULL)
    return NULL;
  
  hash_t * newhash = malloc(sizeof(hash_t));
  StopIf(newhash == NULL, 7,
	 "Fatal error: Could not allocate memory for hash_t struct.\n");

  if (!IS_POF2(capacity)) {
    debug("Number given was not a power of 2: %lu; ", capacity);
    capacity = next_pof2(capacity);
    debug("Number is now: %lu\n", capacity);
  }

  *newhash = (hash_t){
    .capacity = capacity,
    .destroy = destroy,
    .compare = compare,
    .hashfn = hashfn,
    .buckets = NULL
  };

  newhash->buckets = malloc(capacity * sizeof(bucket_t *));
  StopIf(newhash->buckets == NULL, 8,
	 "Fatal error: Could not allocate memory for hash_t struct.\n");
  memset(newhash->buckets, 0, capacity * sizeof(bucket_t *));

  return newhash;
}

/*******************************************************************************
 * FUNCTION:	    hash_insert
 *
 * DESCRIPTION:	    Insert a new member into the hash table. If the 'key' is
 *		    already in the hash table, change the value member to
 *		    'value.'
 *
 * ARGUMENTS:	    hash: (hash_t *) -- the hash to insert the data into.
 *		    key: (void *) -- the 'key' for the data.
 *		    value: (void *) -- the 'value' for the data. In some cases,
 *			this may be the same as the key.
 *
 * RETURN:	    int -- 0 on success, -1 otherwise.
 *
 * NOTES:	    none.
 ***/
int hash_insert(hash_t * hash, void * key)
{
  if (key == NULL || hash == NULL || hash->buckets == NULL)
    return -1;

  int i = 0;
  size_t keyval = HASH(hash->hashfn(key), i, hash->capacity) % hash->capacity;
  StopIf(!(HASH2(hash->hashfn(key), hash->capacity) % 2), 10,
	 "HASH2 returned an even number: %lu, given %lu\n",
	 HASH2(hash->hashfn(key), hash->capacity), hash->hashfn(key));

  debug("Inserting key (%p) @ pos %lu", key, keyval);
  while (hash->buckets[keyval] != NULL
	 && hash->compare(KEY(hash, keyval), key)
	 && !VACATED(hash, keyval)
	 && i < hash->capacity) {
    i++;
    keyval = HASH(hash->hashfn(key), i, hash->capacity) % hash->capacity;
    debug(" %lu", keyval);
  }
  debug("\n");  

  if (i == hash->capacity || keyval >= hash->capacity)
    return -1;

  if (hash->buckets[keyval] == NULL) {
    hash->buckets[keyval] = malloc(sizeof(bucket_t));
    if (hash->buckets[keyval] == NULL)
      return -1;
    memset(hash->buckets[keyval], 0, sizeof(bucket_t)); /* Just in case */
    VACATED(hash, keyval) = 0;
  }

  if (hash->compare(KEY(hash, keyval), key) || VACATED(hash, keyval))
    hash->size++;

  KEY(hash, keyval) = key;
  VACATED(hash, keyval) = 0;
  return 0;
}

/*******************************************************************************
 * FUNCTION:	    hash_get
 *
 * DESCRIPTION:	    This function returns the value from the key/value pair
 *		    of the hash table entry with the key matching 'key.'
 *
 * ARGUMENTS:	    hash: (hash_t *) -- the hash table.
 *		    key: (void *) -- the key to look for.
 *
 * RETURN:	    void * -- the 'value' from the key/value pair with the key
 *			matching key, or NULL if it could not be found.
 *
 * NOTES:	    none.
 ***/
void * hash_get(hash_t * hash, void * key)
{
  if (key == NULL || hash == NULL || hash->buckets == NULL)
    return NULL;

  int i = 0;
  size_t keyval = HASH(hash->hashfn(key), i, hash->capacity) % hash->capacity;
  StopIf(!(HASH2(hash->hashfn(key), hash->capacity) % 2), 10,
	 "HASH2 returned an even number: %lu, given %lu\n",
	 HASH2(hash->hashfn(key), hash->capacity), hash->hashfn(key));

  debug("Searching for key %p @ pos %lu", key, keyval);
  while (hash->buckets[keyval] != NULL
	 && KEY(hash, keyval) != key
	 && !VACATED(hash, keyval)
	 && i < hash->capacity) {
    i++;
    keyval = HASH(hash->hashfn(key), i, hash->capacity) % hash->capacity;
    debug(" %lu", keyval);
  }
  debug("\n");

  if (i < hash->capacity && hash->buckets[keyval] != NULL)
    return KEY(hash, keyval);
  else
    return NULL;
}

/*******************************************************************************
 * FUNCTION:	    hash_remove
 *
 * DESCRIPTION:	    If the element exists in the hash table, remove it (Set its
 *		    vacated flag to true). If hash->destroy != NULL, call it
 *		    on member key and then member value.
 *
 * ARGUMENTS:	    hash: (hash_t *) -- the hash table.
 *		    key: (void *) -- the key to look for.
 *
 * RETURN:	    int -- 0 on success, -1 otherwise.
 *
 * NOTES:	    none.
 ***/
int hash_remove(hash_t * hash, void * key)
{
  if (key == NULL || hash == NULL || hash->buckets == NULL)
    return -1;

  int i = 0;
  size_t keyval = HASH(hash->hashfn(key), i, hash->capacity) % hash->capacity;
  StopIf(!(HASH2(hash->hashfn(key), hash->capacity) % 2), 10,
	 "HASH2 returned an even number: %lu, given %lu\n",
	 HASH2(hash->hashfn(key), hash->capacity), hash->hashfn(key));

  while (hash->buckets[keyval] != NULL
	 && KEY(hash, keyval) != key
	 && i < hash->capacity) {
    i++;
    keyval = HASH(hash->hashfn(key), i, hash->capacity) % hash->capacity;
  }

  if (hash->buckets[keyval] == NULL
      || VACATED(hash, keyval)
      || i >= hash->capacity)
    return -1;

  VACATED(hash, keyval) = 1;
  if (hash->destroy) {
    hash->destroy(KEY(hash, keyval));
    KEY(hash, keyval) = NULL;
  }
  KEY(hash, keyval) = NULL;
  hash->size--;
  return 0;
}

/*******************************************************************************
 * FUNCTION:	    hash_traverse
 *
 * DESCRIPTION:	    This function traverses the hash and invokes 'func' on each
 *		    key/value pair in the hash. The first parameter is the key,
 *		    and the second is the value. This function visits only the
 *		    positions in the hash table which are non-NULL.
 *
 * ARGUMENTS:	    hash: (hash_t *) -- pointer to the hash.
 *		    func: (void (*)(void *, void *)) -- pointer to a user-
 *			defined function which returns void and takes two
 *			parameters--the first is the key, second is the value.
 *
 * RETURN:	    void.
 *
 * NOTES:	    none.
 ***/
void hash_traverse(hash_t * hash, void (*func)(void *))
{
  if (hash == NULL || func == NULL)
    return;
  
  for (int i = 0; i < hash->capacity; i++) {
    if (hash->buckets[i] == NULL || VACATED(hash, i))
      continue;
    func(KEY(hash, i));
  }
}

/*******************************************************************************
 * FUNCTION:	    hash_destroy
 *
 * DESCRIPTION:	    Destroys all memory associated with the hash. iteratively
 *		    calls hash_remove on each element in the list, then frees
 *		    the struct and all allocated members. If hash->destroy is
 *		    non-NULL, then calls hash->destroy() on each key/value
 *		    pair in the table.
 *
 * ARGUMENTS:	    hash: (hash_t *) -- pointer to the hash table.
 *
 * RETURN:	    void.
 *
 * NOTES:	    none.
 ***/
void hash_destroy(hash_t ** hash)
{
  for (int i = 0; i < (*hash)->capacity; i++) {
    if ((*hash)->buckets[i] != NULL) {
      if ((*hash)->destroy && !VACATED((*hash), i)) {
	(*hash)->destroy(KEY((*hash), i));
	KEY((*hash), i) = NULL;
      }
      free((*hash)->buckets[i]);
      (*hash)->buckets[i] = NULL;
    }
  }

  free((*hash)->buckets);
  free(*hash);
  *hash = NULL;
}

/*******************************************************************************
 * STATIC FUNCTIONS
 ***/

/*******************************************************************************
 * FUNCTION:	    next_pof2
 *
 * DESCRIPTION:	    Returns the next highest power of two.
 *
 * ARGUMENTS:	    number: (size_t) -- the starting point.
 *
 * RETURN:	    size_t -- which should be a power of two.
 *
 * NOTES:	    none.
 ***/
static size_t next_pof2(size_t number)
{
  number--;
  number |= number >> 1;
  number |= number >> 2;
  number |= number >> 4;
  number |= number >> 8;
  number |= number >> 16;
  return ++number;
}

/******************************************************************************/
