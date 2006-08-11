/* This is a very simple utility which will dump out some of your
   soundcard hardware parameters. I found this very useful in finding
   appropriate values for my .asoundrc file to get atcleci.so working
   too many underruns, which result in duplicate utterances at the
   end of speech chunks. 
   I have been a bit excessive in error handling, but figured if someone
   was having problems, this might provide the clue they need
   Compile with
   gcc -o soundHwParams soundHwParams.c -lasound
   Tim Cross 15/05/06
*/
#define ALSA_PCM_NEW_HW_PARAMS_API
#include <stdlib.h>
#include <alsa/asoundlib.h>
void showErrorAndExit(int, char *);
void showErrorAndExit(int err, char *msg) {
  fprintf(stderr, msg, snd_strerror(err));
  exit(1);
}
int main() {
  int result;
  snd_pcm_t *handle = NULL;
  snd_pcm_hw_params_t *params = NULL;
  snd_pcm_uframes_t frames;
  snd_pcm_access_t access;
  snd_pcm_format_t format;
  unsigned int rate = 44100;
  unsigned int val, val2;
  int dir = 0;
  result = snd_pcm_open(&handle, "default", SND_PCM_STREAM_PLAYBACK, 0);
  if (result < 0) 
    showErrorAndExit(result, "Error: Unable to open PCM device: %s\n");
  snd_pcm_hw_params_alloca(&params);
  result = snd_pcm_hw_params_any(handle, params);
  if (result < 0) 
    showErrorAndExit(result, "Error: Cannot read HW params: %s\n");
  result = snd_pcm_hw_params_set_access(handle, params,
                                        SND_PCM_ACCESS_RW_INTERLEAVED);
  if (result < 0)
    showErrorAndExit(result, "Could not set access method: %s\n");
  result = snd_pcm_hw_params_set_format(handle, params, SND_PCM_FORMAT_S16_LE);
  if (result < 0)
    showErrorAndExit(result, "Error: Could not set output format: %s\n");
  result = snd_pcm_hw_params_set_channels(handle, params, 1);
  if (result < 0)
    showErrorAndExit(result, "Error: Cannot set to 1 channel: %s\n");
  result = snd_pcm_hw_params_set_rate_near(handle, params, &rate, &dir);
  if (result < 0)
    showErrorAndExit(result, "Error: Could not set rate: %s\n");
  result = snd_pcm_hw_params(handle, params);
  if (result < 0)
    showErrorAndExit(result, "Error: Could not write HW params: %s\n");
  printf("Card Parameters\n");
  printf("%30s: %s\n", "Alsa Library Version", snd_asoundlib_version());
  result = snd_pcm_hw_params_get_access(params, &access);
  if (result < 0) {
    showErrorAndExit(result, "Error: Could not retrieve access mode: %s\n");
  } else {
    printf("%30s: %s\n", "Access Method", snd_pcm_access_name(access));
  }
  result = snd_pcm_hw_params_get_format(params, &format);
  if (result < 0) {
    showErrorAndExit(result, "Error: Unable to retrieve format type: %d\n");
  } else {
    printf("%30s: %s\n", "Format", snd_pcm_format_name(format));
  }
  result = snd_pcm_hw_params_get_channels(params, &val);
  if (result < 0) {
    showErrorAndExit(result, "Error: Unable to retrieve channel count: %s\n");
  } else {
    printf("%30s: %d\n", "Channels", val);
  }
  result = snd_pcm_hw_params_get_rate(params, &val, &dir);
  if (result < 0) {
    showErrorAndExit(result, "Error: Unable to retrieve rate: %s\n");
  } else {
    printf("%30s: %d bps\n", "Rate", val);
  }
  result = snd_pcm_hw_params_get_period_time(params, &val, &dir);
  if (result < 0) {
    showErrorAndExit(result, "Error: Unable to retrieve period time: %s\n");
  } else {
    printf("%30s: %d us\n", "Period Time", val);
  }
  result = snd_pcm_hw_params_get_period_size(params, &frames, &dir);
  if (result < 0) {
    showErrorAndExit(result, "Error: Unable to retrieve period size: %s\n");
  } else {
    printf("%30s: %d frames\n", "Period Size", (int) frames);
  }
  result = snd_pcm_hw_params_get_buffer_time(params, &val, &dir);
  if (result < 0) {
    showErrorAndExit(result, "Error: Unable to retrieve buffer time: %s\n");
  } else {
    printf("%30s: %d us\n", "Buffer Time", val);
  }
  result = snd_pcm_hw_params_get_buffer_size(params, &frames);
  if (result < 0) {
    showErrorAndExit(result, "Error: Unable to retrieve buffer size: %s\n");
  } else {
    printf("%30s: %d frames\n", "Buffer Size", (int) frames);
  }
  result = snd_pcm_hw_params_get_periods(params, &val, &dir);
  if (result < 0) {
    showErrorAndExit(result, "Error: Unable to get buffer periods: %s\n");
  } else {
    printf("%30s: %d frames\n", "Periods per Buffer", val);
  }
  result = snd_pcm_hw_params_get_rate_numden(params, &val, &val2);
  if (result < 0) {
    showErrorAndExit(result, "Error: Unable to get rate numerator/denominator: %s\n");
  } else {
    printf("%30s: %d/%d bps\n", "Exact Rate", val, val2);
  }
  val = snd_pcm_hw_params_get_sbits(params);
  printf("%30s: %d\n", "Significant Bits", val);
  result = snd_pcm_hw_params_get_tick_time(params, &val, &dir);
  if (result < 0) {
    showErrorAndExit(result, "Error: Could not retrieve tick time: %s\n");
  } else {
    printf("%30s: %d\n", "Tick Time", val);
  }
  printf("Card Capabilities\n");
  val = snd_pcm_hw_params_is_batch(params);
  printf("%30s: %s\n", "Batch Transfer", (val ? "Yes" : "No"));
  val = snd_pcm_hw_params_is_block_transfer(params);
  printf("%30s: %s\n", "Block Transfer", (val ? "Yes" : "No"));
  val = snd_pcm_hw_params_is_double(params);
  printf("%30s: %s\n", "Double Buffering", (val ? "Yes" : "No"));
  val = snd_pcm_hw_params_is_half_duplex(params);
  printf("%30s: %s\n", "Half Duplex Only", (val ? "Yes" : "No"));
  val = snd_pcm_hw_params_is_joint_duplex(params);
  printf("%30s: %s\n", "Joint Duplex Capable", (val ? "Yes" : "No"));
  val = snd_pcm_hw_params_can_overrange(params);
  printf("%30s: %s\n", "Support Overrange Detection", (val ? "Yes" : "No"));
  val = snd_pcm_hw_params_can_mmap_sample_resolution(params);
  printf("%30s: %s\n", "Support Sample-res Mmap", (val ? "Yes" : "No"));
  val = snd_pcm_hw_params_can_pause(params);
  printf("%30s: %s\n", "Can Pause", (val ? "Yes" : "No"));
  val = snd_pcm_hw_params_can_resume(params);
  printf("%30s: %s\n", "Can Resume", (val ? "Yes" : "No"));
  val = snd_pcm_hw_params_can_sync_start(params);
  printf("%30s: %s\n", "Support Sync Start", (val ? "Yes" : "No"));
  result = snd_pcm_close(handle);
  if (result < 0) 
    showErrorAndExit(result, "Error: Could not close PCM device: %s\n");
  return 0;
}
