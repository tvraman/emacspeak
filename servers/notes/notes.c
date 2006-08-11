/*$ID:$*/
/*License: GPL*/
/* {{{ cpp directives*/

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <fcntl.h>


#define BUFLEN 256
#define BUFSIZE 80

/* }}} */
/* {{{ globals*/

static char buffer[BUFLEN];
static char *progname;

static int notes[9][12] = {
  {1002, 946, 893, 843, 795, 751, 709, 669, 631, 596, 562, 531},
  {501, 473, 446, 421, 398, 375, 354, 334, 316, 298, 281, 265},
  {250, 236, 223, 211, 199, 188, 177, 167, 158, 149, 141, 133},
  {125, 118, 112, 105, 99, 94, 89, 84, 79, 74, 70, 66},
  {63, 59, 56, 53, 50, 47, 44, 42, 39, 37, 35, 33},
  {31, 30, 28, 26, 25, 23, 22, 21, 20, 19, 18, 17},
  {16, 15, 14, 13, 12, 11, 11, 10, 10, 9, 9, 8},
  {7, 7, 7, 7, 6, 6, 6, 5, 5, 5, 4, 4},
  {4, 4, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2}
};

static int conv[7] = { 9, 11, 0, 2, 4, 5, 7 };

/* }}} */
/* {{{play notes*/

static void
play_notes (float volume, int length, int tone, float decay, char *buf)
{
  int oct, n, f, period, j, i, k;
  f = open ("/dev/audio", O_WRONLY);
  if (f < 0)
    {
      fprintf (stderr, "Within play_notes:%s  \n", progname);
      fprintf (stderr, "%s: Cannot open /dev/audio \n", progname);
      exit (1);
    }

  oct = buf[0] - '0';
  n = conv[buf[1] - 'a'];
  if (buf[2] == 'b')
    {
      --n;
    }
  else if (buf[2] == '#')
    {
      ++n;
    }
  n = (n + tone) % 12;
  period = notes[oct][n];
  i = 0;
  k = 0;
  for (;;)
    {
      for (j = 0; j < period >> 1; ++j)
	{
	  buffer[i++] = 0;
	  if ((volume = volume - decay) < 0.0)
	    volume = 0.0;
	  if (i >= BUFLEN)
	    {
	      write (f, buffer, BUFLEN);
	      if (++k >= length)
		{
		  break;
		}
	      i = 0;
	    }
	}
      for (j = 0; j < ((period >> 1) + (period & 0x1)); ++j)
	{
	  buffer[i++] = (int) volume;
	  if ((volume = volume - decay) < 0.0)
	    {
	      volume = 0.0;
	    }
	  if (i >= BUFLEN)
	    {
	      write (f, buffer, BUFLEN);
	      if (++k >= length)
		{
		  if (close (f))
		    {
		      printf ("closed /dev/audio before exiting \n");
		      printf ("Tried but failed \n");
		      exit (1);
		    }
		  else
		    {
		      return;
		    }
		}
	      i = 0;
	    }
	}
    }
  /* close before exiting. */
  if (close (f))
    {
      printf ("closed /dev/audio before exiting \n");
      printf ("Tried but failed \n");
      exit (1);
    }
  return;
}

/* }}} */
/* {{{main*/

int
main ()
{
  char input[BUFSIZE];
  char octave[5];
  int tone = 0;
  float volume = 2550;
  float decay = 0;
  int length = 10;
  while (fgets (input, BUFLEN, stdin))
    {
      sscanf (input, "%f %d %d %f %s",
	      &volume, &length, &tone, &decay, octave);
      /* fprintf(stderr, "volume = %f, */
      /* tone = %d, */
      /* length = %d, */
      /* decay = %f, */
      /* octave = %s\n", */
      /*             volume, tone, length, decay, octave); */
      play_notes (volume, length, tone, decay, octave);
    }
  return 0;
}

 /* }}} */
