/*******************************************************************************
 * NAME:	    tree.h
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    This file contains the public interface for the source in
 *		    tree.c, which is an n-ary tree.
 *
 * CREATED:	    09/29/2017
 *
 * LAST EDITED:	    09/29/2017
 ***/

#ifndef __ET_TREE_H__
#define __ET_TREE_H__

/*******************************************************************************
 * INCLUDES
 ***/

/*******************************************************************************
 * TYPE DEFINITIONS
 ***/

/**
 * \brief N-Ary Tree Node struct.
 * \member children An array of pointers to this node's children.
 * \member data A pointer to some data provided by the user.
 */
typedef struct _nary_node {

  void * data;
  _nary_node * parent;
  _nary_node ** children;

} nary_node_t;

/**
 * \brief N-Ary Tree Struct.
 * \member 
 */
typedef struct {

  _nary_node * root;
  /* Other members here. */

} nary_tree_t;

/*******************************************************************************
 * API FUNCTION PROTOTYPES
 ***/

nary_tree_t * nary_tree_create(void);
int nary_tree_insert(nary_tree_t * tree, nary_tree_t * node);
int nary_tree_remove(nary_tree_t * tree, nary_tree_t * node);
void nary_tree_destroy(nary_tree_t * tree);

#endif /* __ET_TREE_H__ */

/******************************************************************************/
