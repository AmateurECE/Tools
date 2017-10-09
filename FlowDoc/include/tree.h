/*******************************************************************************
 * NAME:	    tree.h
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    This file contains the public interface for the source in
 *		    tree.c, which is an n-ary tree. This tree is designed for
 *		    the FlowDoc program, so it has a few caveats. For example,
 *		    only one copy of each node may remain in memory at any time.
 *		    This is done by using an associative hash map to keep track
 *		    of all the data in the tree. This prevents bloating when
 *		    analyzing larger products that may call strtoul() hundreds
 *		    of times, for example. This tree is not a search tree. The
 *		    nodes of the children are not ordered, however the general
 *		    theme is to insert children to the right of any previously
 *		    existing children, so that the tree may be generally viewed
 *		    chronologically when traversed inorder.
 *
 * CREATED:	    09/29/2017
 *
 * LAST EDITED:	    09/29/2017
 ***/

#ifndef __ET_TREE_H__
#define __ET_TREE_H__

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

/* TODO: Maybe keep track of all of the nodes that reference the function? */

/**
 * \brief N-Ary Tree Struct.
 * \member root The root node.
 * \member compare Function to compare two data points.
 * \member destroy Function to destroy a data point.
 */
typedef struct {

  _nary_node * root;
  void * (*compare)(void *, void *)
  void (*destroy)(void *);

} nary_tree_t;

/*******************************************************************************
 * API FUNCTION PROTOTYPES
 ***/

extern nary_tree_t * nary_tree_create(void * (*compare)(void *, void *),
				      void (*destroy)(void * data));
extern int nary_tree_insert(nary_tree_t * tree, nary_tree_t * node);
extern int nary_tree_remove(nary_tree_t * tree, nary_tree_t * node);
extern void nary_tree_destroy(nary_tree_t * tree);

#endif /* __ET_TREE_H__ */

/******************************************************************************/
