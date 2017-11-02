/*******************************************************************************
 * NAME:	    index-test.h
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    Header file, containing external declarations of functions
 *		    for use in other programs.
 *
 * CREATED:	    11/02/2017
 *
 * LAST EDITED:	    11/02/2017
 ***/

#ifndef __ET_INDEX_TEST_H__
#define __ET_INDEX_TEST_H__

/*******************************************************************************
 * INCLUDES
 ***/

#include <stdio.h>
#include <stdint.h>

/*******************************************************************************
 * MACRO DEFINITIONS
 ***/

#define __USING_GITV2__ /* Ensures that we are using git version 2. */

/*******************************************************************************
 * TYPE DEFINITIONS
 ***/

typedef struct {

  uint32_t  metamod;
  uint32_t  cfrac;
  uint32_t  datamod;
  uint32_t  mfrac;
  uint32_t  dev;
  uint32_t  ino;
  uint32_t  mode;
  uint32_t  uid;
  uint32_t  gid;
  uint32_t  size;
  uint8_t   sha1[20];
  uint16_t  flags;
#ifdef __USING_GITV3__
  uint32_t  extflags;
#endif

  /* Arbitrary padding within the struct will mess up our calls to fread() */
} git_info __attribute__((packed));

typedef struct {

  struct    git_info; /* All members of git_info can be accessed directly */
  size_t    pathsize;
  char *    pathname;
  
  /* Arbitrary padding within the struct will mess up our calls to fread() */
} index_info __attribute__((packed));

/*******************************************************************************
 * API FUNCTIONS
 ***/

/**
 * This function determines if the given file matches the git index header, and
 * if the version specified is compatible with this program. The file position
 * will be the start of the index entries, if the function call is successful.
 * \param gitfile An open filehandle that can be read from.
 * \return the number of index entries if the file matches the spec, 0
 * otherwise.
 */
extern int index_verify(FILE * gitfile);

/**
 * This function returns the number of entries in the index file. This function
 * assumes that the file is a valid git index file.
 * \param gitfile An open filehandle pointing to the index file.
 * \return The number of entries in the index file.
 */
extern int index_number(FILE * gitfile);

/* Return file mode */
/* https://github.com/git/git/blob/867b1c1bf68363bcfd17667d6d4b9031fa6a1300/\
Documentation/technical/index-format.txt#L38 */
extern int index_mode(index_info * info);

/* Return some flags */
/* https://github.com/git/git/blob/867b1c1bf68363bcfd17667d6d4b9031fa6a1300/\
Documentation/technical/index-format.txt#L38 */
extern int index_flags(index_info * info);

#endif /* __ET_INDEX_TEST_H__ */

/******************************************************************************/
