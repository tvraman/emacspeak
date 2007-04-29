/*$Id: aoutput.cpp 4433 2007-03-26 03:35:40Z tv.raman.tv $*/
//<Usage:

/*
 * utility functions to produce output via alsa
 */

//>
//<includes

#include <sys/time.h>
#include <dlfcn.h>

#include <alsa/asoundlib.h>

//>
//< alsa: globals  and defines

#define DEFAULT_FORMAT          SND_PCM_FORMAT_S16
#define DEFAULT_SPEED           11025

/* globals */

static snd_pcm_t *AHandle = NULL;
static snd_output_t *Log = NULL;
short *waveBuffer = NULL;

//>
//<decls and function prototypes

static int alsa_init ();
static void alsa_reset ();      //drop handle and reset
static size_t alsa_configure (void);
int alsa_close ();

//>
//<alsa: set hw and sw params

static size_t
alsa_configure (void)
{
  //<init:
  size_t chunk_bytes, bits_per_sample, bits_per_frame = 0;
  snd_pcm_uframes_t chunk_size, buffer_size = 0;
  snd_pcm_hw_params_t *params;
  unsigned int rate = DEFAULT_SPEED;
  int err;
  snd_pcm_hw_params_alloca (&params);
  //>
  //<defaults:

  err = snd_pcm_hw_params_any (AHandle, params);
  if (err < 0)
    {
      fprintf (stderr,
               "Broken configuration for this PCM: no configurations available");
      exit (EXIT_FAILURE);
    }

  //>
  //<Format:

  err = snd_pcm_hw_params_set_format (AHandle, params, DEFAULT_FORMAT);
  if (err < 0)
    {
      fprintf (stderr, "Sample format non available");
      exit (EXIT_FAILURE);
    }

  //>
  //<Channels:

  err = snd_pcm_hw_params_set_channels (AHandle, params, 1);
  if (err < 0)
    {
      fprintf (stderr, "Channels count non available");
      exit (EXIT_FAILURE);
    }

  //>
  //<Rate:

  err = snd_pcm_hw_params_set_rate_near (AHandle, params, &rate, 0);
  assert (err >= 0);

  //>
  //<Access Mode:
  err = snd_pcm_hw_params_set_access (AHandle, params,
                                      SND_PCM_ACCESS_RW_INTERLEAVED);
  if (err < 0)
    {
      fprintf (stderr, "Access type not available");
      exit (EXIT_FAILURE);
    }
  //>
  //< Set things explicitly if DEBUG
#ifdef DEBUG

  //<Compute buffer_time:
  unsigned int period_time = 0;
  unsigned int buffer_time = 0;
  snd_pcm_uframes_t period_frames = 0;
  snd_pcm_uframes_t buffer_frames = 0;
  // affected by defined buffer_size  (e.g. via asoundrc)
  if (buffer_time == 0 && buffer_frames == 0)
    {
      err = snd_pcm_hw_params_get_buffer_time (params, &buffer_time, 0);
      assert (err >= 0);
      if (buffer_time > 500000) //usecs
        buffer_time = 500000;
    }
  //>
  //<Compute period_time:

  if (period_time == 0 && period_frames == 0)
    {
      if (buffer_time > 0)
        period_time = buffer_time / 4;
      else
        period_frames = buffer_frames / 4;
    }
  if (period_time > 0)
    err = snd_pcm_hw_params_set_period_time_near (AHandle, params,
                                                  &period_time, 0);
  else
    err = snd_pcm_hw_params_set_period_size_near (AHandle, params,
                                                  &period_frames, 0);
  assert (err >= 0);
  if (buffer_time > 0)
    {
      err = snd_pcm_hw_params_set_buffer_time_near (AHandle, params,
                                                    &buffer_time, 0);
    }
  else
    {
      err = snd_pcm_hw_params_set_buffer_size_near (AHandle, params,
                                                    &buffer_frames);
    }
  assert (err >= 0);

  //>
#endif

  //>
  //<Commit hw params:
  err = snd_pcm_hw_params (AHandle, params);
  if (err < 0)
    {
      fprintf (stderr, "Unable to install hw params:");
      exit (EXIT_FAILURE);
    }
  //>
  //<finalize chunk_size and buffer_size:

  snd_pcm_hw_params_get_period_size (params, &chunk_size, 0);
  snd_pcm_hw_params_get_buffer_size (params, &buffer_size);
  if (chunk_size == buffer_size)
    {
      fprintf (stderr, "Can't use period equal to buffer size (%lu == %lu)",
               chunk_size, buffer_size);
      exit (EXIT_FAILURE);
    }

  //>
  //< If DEBUG: SW Params Configure transfer:

#ifdef DEBUG
  size_t n;
  snd_pcm_uframes_t xfer_align;
  snd_pcm_uframes_t start_threshold, stop_threshold;
  int start_delay = 5;
  int stop_delay = 0;
  snd_pcm_sw_params_t *swParams;
  snd_pcm_sw_params_alloca (&swParams);
  snd_pcm_sw_params_current (AHandle, swParams);
  err = snd_pcm_sw_params_get_xfer_align (swParams, &xfer_align);
  if (err < 0)
    {
      fprintf (stderr, "Unable to obtain xfer align\n");
      exit (EXIT_FAILURE);
    }
  // round up to closest transfer boundary
  n = (buffer_size / xfer_align) * xfer_align;
  if (start_delay <= 0)
    {
      start_threshold =
        (snd_pcm_uframes_t) (n + (double) rate * start_delay / 1000000);
    }
  else
    start_threshold =
      (snd_pcm_uframes_t) ((double) rate * start_delay / 1000000);
  if (start_threshold < 1)
    start_threshold = 1;
  if (start_threshold > n)
    start_threshold = n;
  err =
    snd_pcm_sw_params_set_start_threshold (AHandle, swParams,
                                           start_threshold);
  assert (err >= 0);
  if (stop_delay <= 0)
    stop_threshold =
      (snd_pcm_uframes_t) (buffer_size +
                           (double) rate * stop_delay / 1000000);
  else
    stop_threshold =
      (snd_pcm_uframes_t) ((double) rate * stop_delay / 1000000);
  err =
    snd_pcm_sw_params_set_stop_threshold (AHandle, swParams, stop_threshold);
  assert (err >= 0);

  err = snd_pcm_sw_params_set_xfer_align (AHandle, swParams, xfer_align);
  assert (err >= 0);

  if (snd_pcm_sw_params (AHandle, swParams) < 0)
    {
      fprintf (stderr, "unable to install sw params:");
      exit (EXIT_FAILURE);
    }
#endif

  //>
  bits_per_sample = snd_pcm_format_physical_width (DEFAULT_FORMAT);
  bits_per_frame = bits_per_sample * 1; //mono
  chunk_bytes = chunk_size * bits_per_frame / 8;
  return chunk_bytes;
}

//>
//<xrun and suspend

#ifndef timersub

#define timersub(a, b, result)                          \
  do {                                                  \
    (result)->tv_sec = (a)->tv_sec - (b)->tv_sec;       \
    (result)->tv_usec = (a)->tv_usec - (b)->tv_usec;    \
    if ((result)->tv_usec < 0) {                        \
      --(result)->tv_sec;                               \
      (result)->tv_usec += 1000000;                     \
    }                                                   \
  } while (0)
#endif

static void
xrun (void)
{
  snd_pcm_status_t *status;
  int res;

  snd_pcm_status_alloca (&status);
  if ((res = snd_pcm_status (AHandle, status)) < 0)
    {
      fprintf (stderr, "status error: %s", snd_strerror (res));
      exit (EXIT_FAILURE);
    }
  if (snd_pcm_status_get_state (status) == SND_PCM_STATE_XRUN)
    {
      struct timeval now, diff, tstamp;
      gettimeofday (&now, 0);
      snd_pcm_status_get_trigger_tstamp (status, &tstamp);
      timersub (&now, &tstamp, &diff);
      fprintf (stderr, "Underrun!!! (at least %.3f ms long)\n",
               diff.tv_sec * 1000 + diff.tv_usec / 1000.0);
      if ((res = snd_pcm_prepare (AHandle)) < 0)
        {
          fprintf (stderr, "xrun: prepare error: %s", snd_strerror (res));
          exit (EXIT_FAILURE);
        }
      return;                   // ok, data should be accepted again
    }

  fprintf (stderr, "read/write error, state = %s",
           snd_pcm_state_name (snd_pcm_status_get_state (status)));
  exit (EXIT_FAILURE);
}

static void
suspend (void)
{
  int res;


  fprintf (stderr, "Suspended. Trying resume. ");
  fflush (stderr);
  while ((res = snd_pcm_resume (AHandle)) == -EAGAIN)
    sleep (1);                  /* wait until suspend flag is released */
  if (res < 0)
    {

      fprintf (stderr, "Failed. Restarting stream. ");
      fflush (stderr);
      if ((res = snd_pcm_prepare (AHandle)) < 0)
        {
          fprintf (stderr, "suspend: prepare error: %s", snd_strerror (res));
          exit (EXIT_FAILURE);
        }
    }

  fprintf (stderr, "Done.\n");
}

//>
//<alsa: pcm_write

static ssize_t
pcm_write (short *data, size_t count)
{
  ssize_t r;
  ssize_t result = 0;
  while (count > 0)
    {
      r = snd_pcm_writei (AHandle, data, count);
      if (r == -EAGAIN || (r >= 0 && (size_t) r < count))
	{
	  snd_pcm_wait (AHandle, 1000);
	}
      else if (r == -EPIPE)
	{
	  xrun ();
	}
      else if (r == -ESTRPIPE)
	{
	  suspend ();
	}
      else if (r < 0)
	{
	  fprintf (stderr, "write error: %s", snd_strerror (r));
	  exit (EXIT_FAILURE);
	}
      if (r > 0)
	{
	  result += r;
	  count -= r;
	  data += r;
	}
    }
  return result;
}

//>
//<alsa_reset

void
alsa_reset ()
{
  snd_pcm_drop (AHandle);
  snd_pcm_prepare (AHandle);
}

//>
//<alsa_init

int
alsa_init ()
{
  int err;
  char *device = "default";
  size_t chunk_bytes = 0;
  if ((err = snd_pcm_open (&AHandle, device, SND_PCM_STREAM_PLAYBACK, 0)) < 0)
    {
      fprintf (stderr, "Playback open error: %s\n", snd_strerror (err));
      exit (1);
    }
  err = snd_output_stdio_attach (&Log, stderr, 0);
  assert (err >= 0);
  chunk_bytes = alsa_configure ();
  return chunk_bytes;
}

//>
//<alsa_close

int
alsa_close ()
{
  //shut down alsa
  snd_pcm_close (AHandle);
  free (waveBuffer);
  return 0;
}

//>

//<alsa_setup

int
alsa_setup ()
{
  
  size_t chunk_bytes = 0;
  //<initialize alsa
  chunk_bytes = alsa_init ();
  //<Finally, allocate  waveBuffer

  fprintf (stderr, "allocating %d samples\n", chunk_bytes);
  waveBuffer = (short *) malloc (chunk_bytes * sizeof (short));
  if (waveBuffer == NULL)
    {
      fprintf (stderr, "not enough memory");
      exit (EXIT_FAILURE);
    }

  //>
  //>
  
  
  
  
                      return 0;
}

//>
//<playTTS

int
playTTS (int count)
{
  pcm_write (waveBuffer, count);
  return 0;
}

//>
//<end of file
//local variables:
//folded-file: t
//end:
//>
