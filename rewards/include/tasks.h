/*******************************************************************************
 * NAME:	    tasks.h
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    This contains the header file, in case the source is to be
 *		    used elsewhere.
 *
 * CREATED:	    11/28/2017
 *
 * LAST EDITED:	    11/29/2017
 ***/

/*******************************************************************************
 * API FUNCTION PROTOTYPES
 ***/

/* Run an audit, to see how much you've earned! */
int audit();

/* List all of the registered expenses for the last week */
int costs();

/* Reset the data (as in at the start of the week). */
int reset();

/* Add a new total to the list of expenses */
int spent();

/* Print out the tasks that haven't been completed, or all of the tasks */
int tasks(int aflag);

/******************************************************************************/
