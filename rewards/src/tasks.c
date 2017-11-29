/*******************************************************************************
 * NAME:	    tasks.c
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    This is a tool which rewards the user for doing things that
 *		    they set out to do. A reward/incentive system.>
 *
 * CREATED:	    11/28/2017
 *
 * LAST EDITED:	    11/29/2017
 ***/

/*******************************************************************************
 * INCLUDES
 ***/

#include <stdio.h>

#include "tasks.h"
#include "../C-Preprocessor/error.h"

/*******************************************************************************
 * MACRO DEFINITIONS
 ***/

#if !defined(COSTS_FILE) || !defined(TASKS_FILE)
#error COSTS_FILE and TASKS_FILE must be defined.
#endif

#define COSTS	0
#define TASKS	1

/*******************************************************************************
 * TYPE DEFINITIONS
 ***/

typedef struct {
  float * value;
  char * description;
} payme_data_t;

/*******************************************************************************
 * STATIC FUNCTION PROTOTYPES
 ***/

static payme_data_t * init_data(int data);
static int save_data(payme_data_t *);

/*******************************************************************************
 * API FUNCTIONS
 ***/

int audit();
int costs();
int reset();
int spent();
int tasks(int aflag);

/*******************************************************************************
 * MAIN
 ***/

int main(int argc, char * argv[])
{
  if (argc < 2) {
    /* TODO: Interactive shell-mode */
    StopIf(1, 1, "This has not yet been implemented.");
  }

  /* TODO: Implement tasks.c here */
}

/*******************************************************************************
 * STATIC FUNCTIONS
 ***/

/*******************************************************************************
 * FUNCTION:	    init_data
 *
 * DESCRIPTION:	    This function initializes an array of data structs from the
 *		    location of the data.
 *
 * ARGUMENTS:	    data: (int) -- the type of data to read; (COSTS or TASKS).
 *
 * RETURN:	    payme_data_t * -- Pointer to the array of structs.
 *
 * NOTES:	    none.
 ***/
static payme_data_t * init_data(int data)
{
  FILE * fin = NULL;

  switch(data) {
  case COSTS:
    if ((fin = fopen(COSTS_LOCATION, "r")) == NULL)
      return NULL;
    break;
  case TASKS:
    break;
  default:
  }

  return NULL;
}

/*******************************************************************************
 * FUNCTION:	    save_data
 *
 * DESCRIPTION:	    This function writes the data out its location. The type of
 *		    data is specified.
 *
 * ARGUMENTS:	    data: (payme_data_t *) -- the data to save
 *		    type: (int) -- the type of data (COSTS or TASKS).
 *
 * RETURN:	    int -- 0 on success, -1 otherwise.
 *
 * NOTES:	    none.
 ***/
static int save_data(payme_data_t * data, int type)
{
  switch(type) {
  case COSTS:
    break;
  case TASKS:
    break;
  default:
  }

  return -1;
}

/******************************************************************************/
