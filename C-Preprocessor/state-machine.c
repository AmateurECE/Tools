#include <stdio.h>

typedef enum
  {
    eEVENT_X,
    eEVENT_Y,
    eEVENT_Z,
    eEVENT_END,
    NEVENTS
  } Event;

typedef enum
  {
    eSTATE_X,
    eSTATE_Y,
    eSTATE_Z,
    eSTATE_END,
    NSTATES
  } State;

/* call back utility */
typedef State (*state_transition)(Event *);

State function_eXsX(Event * e)
{
  printf("xx\n");
  *e = eEVENT_X;
  return eSTATE_Y;
}
State function_eXsY(Event * e)
{
  printf("xy\n");
  *e = eEVENT_X;
  return eSTATE_Z;
}
State function_eXsZ(Event * e)
{
  printf("xz\n");
  *e = eEVENT_Y;
  return eSTATE_X;
}
State function_eYsX(Event * e)
{
  printf("yx\n");
  *e = eEVENT_Y;
  return eSTATE_Y;
}
State function_eYsY(Event * e)
{
  printf("yy\n");
  *e = eEVENT_Y;
  return eSTATE_Z;
}
State function_eYsZ(Event * e)
{
  printf("yz\n");
  *e = eEVENT_Z;
  return eSTATE_X;
}
State function_eZsX(Event * e)
{
  printf("zx\n");
  *e = eEVENT_Z;
  return eSTATE_Y;
}
State function_eZsY(Event * e)
{
  printf("zy\n");
  *e = eEVENT_Z;
  return eSTATE_Z;
}
State function_eZsZ(Event * e)
{
  printf("zz\n");
  *e = eEVENT_END;
  return eSTATE_END;
}

state_transition stateTrans[NEVENTS][NSTATES] =
  {
    {
      function_eXsX,
      function_eXsY,
      function_eXsZ
    },
    {
      function_eYsX,
      function_eYsY,
      function_eYsZ
    },
    {
      function_eZsX,
      function_eZsY,
      function_eZsZ
    },
  };

int applyEvent(State * s, Event * e)
{
  if (s == NULL || e == NULL || *s >= NSTATES - 1 || *e >= NEVENTS - 1)
    return 0;
  *s = stateTrans[*e][*s](e);
  return 1;
}

int main(int argc,char *argv[])
{

  Event e = eEVENT_X;
  State s = eSTATE_X;
  while(applyEvent(&s, &e))
    ;
}
