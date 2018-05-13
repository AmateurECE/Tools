/******************************************************************************
 * NAME:	    bits.c
 *
 * AUTHOR:	    Ethan D. Twardy
 *
 * DESCRIPTION:	    TODO: bits.c
 *
 * CREATED:	    05/11/2018
 *
 * LAST EDITED:	    05/13/2018
 ***/

/******************************************************************************
 * INCLUDES
 ***/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

/******************************************************************************
 * MACRO DEFINITIONS
 ***/

/* If an error has occurred, print a message and exit */
#define Error_exit(...) {			\
    fprintf(stderr, __VA_ARGS__);		\
    exit(1);					\
  }

/* Get the bit `i' in the number `n'. The LSB is at index 0. */
#define bits(n, i) (1 & ((n) >> (i)))

#define USAGE "\n"\
  "Usage: bits <format> <num>\n\n"\
  "<format>\tIs one of [h2d, h2b, b2h, b2d, d2h, d2b] and specifies the\n\t\t"\
  "conversion taking place (h2d is hex to decimal, etc.)\n"\
  "<num>\t\tA number in the source base. Hex numbers may begin with an\n\t\t"\
  "optional '0x'."


/******************************************************************************
 * STATIC FUNCTION PROTOTYPES
 ***/

static void binary(unsigned int num);
static void decimal(unsigned int num);
static void hex(unsigned int num);
static int verify(const char * spec);

/******************************************************************************
 * MAIN
 ***/

int main(int argc, char ** argv)
{
  /* Verify command line arguments */
  if (argc < 2)
    Error_exit("%s\n", USAGE);
  if (!verify(argv[1]))
    Error_exit("Malformed conversion specifier: %s\n", argv[1]);

  /* Parse the base */
  int base = 0;
  switch (argv[1][0]) {
  case 'h':
    base = 16;
    break;
  case 'd':
    base = 10;
    break;
  case 'b':
    base = 2;
    break;
  default:
    Error_exit("Malformed conversion specifier: %s\n", argv[1])
  }

  /* Convert the number */
  char * endptr = NULL;
  unsigned int num = strtol(argv[2], &endptr, base);
  if (errno & (EINVAL | ERANGE))
    Error_exit("Malformed numerical input.\n");

  /* Parse the output format and print */
  switch (argv[1][2]) {
  case 'h':
    hex(num);
    break;
  case 'd':
    decimal(num);
    break;
  case 'b':
    binary(num);
    break;
  default:
    Error_exit("Malformed conversion specifier: %s\n", argv[1]);
  }
}

/******************************************************************************
 * STATIC FUNCTIONS
 ***/

/******************************************************************************
 * FUNCTION:	    binary
 *
 * DESCRIPTION:	    Print the binary representation of the number `num'.
 *
 * ARGUMENTS:	    num: (unsigned int) -- The number to print
 *
 * RETURN:	    void
 *
 * NOTES:	    none.
 ***/
static void binary(unsigned int num)
{
  for (int i = 8 * (signed)sizeof(unsigned int); i >= 4; i -= 4) {
    for (int j = i; j > i - 4; j--)
      printf("%d", bits(num, j - 1));
    if ((i / 4) % 2 == 0)
      printf("_");
    else
      printf(" ");
  }

  printf("\n");
}

/******************************************************************************
 * FUNCTION:	    decimal
 *
 * DESCRIPTION:	    Print the decimal representation of the number `num'.
 *
 * ARGUMENTS:	    num: (unsigned int) -- The number to print
 *
 * RETURN:	    void
 *
 * NOTES:	    none.
 ***/
static void decimal(unsigned int num)
{
  printf("%u\n", num);
}

/******************************************************************************
 * FUNCTION:	    hex
 *
 * DESCRIPTION:	    Print the hexadecimal representation of the number `num'.
 *
 * ARGUMENTS:	    num: (unsigned int) -- The number to print.
 *
 * RETURN:	    void
 *
 * NOTES:	    none.
 ***/
static void hex(unsigned int num)
{
  printf("%x\n", num);
}

/******************************************************************************
 * FUNCTION:	    verify
 *
 * DESCRIPTION:	    Verify the conversion specifier, passed as `spec'. Valid
 *		    conversion specifiers consist follow the regular expression
 *		    [hbd]2[hbd]. Examples: h2d, d2b, b2d.
 *
 * ARGUMENTS:	    spec: (const char *) -- Pointer to the supposed specifier.
 *
 * RETURN:	    int -- 1 if `spec' follows the rules, 0 otherwise.
 *
 * NOTES:	    none.
 ***/
static int verify(const char * spec)
{
  if (spec[1] != '2')
    return 0;

  for (int i = 0; i < 3; i += 2)
    switch (spec[i]) {
    case 'h':
      break;
    case 'd':
      break;
    case 'b':
      break;
    default:
      return 0;
    }

  return 1;
}

/*****************************************************************************/
