// Display system information right dzen2 status bar
// Sat 27 Feb 2010 03:54:49 AM IST  
// Takes refresh rate in tenths of seconds as an optional first argument
#include<sys/sysinfo.h>
#include<string.h>
#include<stdlib.h>
#include<stdio.h>
#include<unistd.h>
#include<ctype.h>
#include<time.h>

#define DUAL_CORE_PROCESSOR 1

#define COLOR(x) " ^fg(" x ") "

#define BATTERY_PATH "/sys/class/power_supply/"
#define BATTERY_NUMBER "BAT1"

const size_t  Kmax_time_length = 200;
const size_t  Kmax_line_length = 256;
const char Kmeminfo_file[] = "/proc/meminfo";

const battery_medium = 70;
const battery_low = 20;

/* The first four lines of the mem_info file looks like
MemTotal:        2051808 kB
MemFree:          343716 kB
Buffers:          518204 kB
Cached:           537780 kB
*/

long int GetFirstNumberFromLine(char *buffer)
{
  int start_of_number = 0;
  long result;

  for(start_of_number=0; !isdigit(buffer[start_of_number]); start_of_number++);
  result = atol(buffer+start_of_number);
  // printf("%s || %ld \n", buffer+start_of_number, result);
  return result;
}

long int GetNextMemoryStatistic(FILE* fp)
{
  char buffer[Kmax_line_length];
  fgets(buffer, Kmax_line_length, fp);

  return GetFirstNumberFromLine(buffer);
}

long int ReadFirstNumberFromFile( const char filename[] )
{
  FILE *fp;
  char buffer[Kmax_line_length];
  fp = fopen( filename, "r" );
  fgets(buffer, Kmax_line_length, fp);
  fclose( fp );
  return GetFirstNumberFromLine( buffer );
};

long int GetMemoryUsage()
{
  long int mem_total, mem_free, mem_buffers, mem_cached;
  long int mem_used;

  FILE* fp;


  fp = fopen(Kmeminfo_file, "r");
  mem_total   = GetNextMemoryStatistic(fp);
  mem_free    = GetNextMemoryStatistic(fp);
  mem_buffers = GetNextMemoryStatistic(fp);
  mem_cached  = GetNextMemoryStatistic(fp);
  fclose(fp);

  // printf("%ld %ld %ld %ld\n", mem_total, mem_free, mem_buffers, mem_cached);

  mem_used = mem_total - mem_free - (mem_cached + mem_buffers);
  // printf("%ld || ", mem_used);
  return mem_used >> 10;
}

double GetBatteryPercentage()
{
  long int full_charge = ReadFirstNumberFromFile( BATTERY_PATH BATTERY_NUMBER "/charge_full" );
  long int now_charge  = ReadFirstNumberFromFile( BATTERY_PATH BATTERY_NUMBER "/charge_now" );
  return (double) (now_charge) * 100 / (double) full_charge;
}

int GetCPULoadAverage()
{
  double load_average[3];
  int result;

  getloadavg(load_average, 3);
#if DUAL_CORE_PROCESSOR == 1
  load_average[0] /= 2;
#endif
  result = (int)(load_average[0] * 100);
  return result;
}

void GetTime(char * time_string)
{
  time_t t;
  struct tm *time_tm;

  t = time(NULL);
  time_tm = localtime(&t);
  if (time_tm == NULL) {
    perror("localtime");
    exit(EXIT_FAILURE);
  }

  if (strftime(time_string, Kmax_time_length, "%R", time_tm) == 0) {
    fprintf(stderr, "strftime returned 0");
    exit(EXIT_FAILURE);
  }
}



int main(int argc, char *argv[])
{
  long int mem_used = 0;
  int cpu_usage = 0;
  int refresh_rate = 6;
  double battery_percentage = 0;
  char time[Kmax_time_length];

  if(argc == 1) {
    int refresh_rate = atoi(argv[0]);
  }

  while(1)
  {
    mem_used = GetMemoryUsage();
    cpu_usage = GetCPULoadAverage();
    battery_percentage = GetBatteryPercentage();
    GetTime(time);

    printf(COLOR("#0E93FF") "%2d%%", cpu_usage);
    printf(COLOR("#84EF96") "%4ldM ", mem_used);
    printf(COLOR("#FF8E1D") ":-) ");

    if( battery_percentage > battery_medium )
    	printf(COLOR("#DDDDDD") "%2.0f%%", battery_percentage );
    else if( battery_percentage > battery_low )
    	printf(COLOR("#888888") "%2.0f%%", battery_percentage );
    else
    	printf(COLOR("#DD0000") "%2.0f%%", battery_percentage );

    printf(COLOR("#B6E9EF") "%s  \n", time);
    fflush(stdout);
    usleep(refresh_rate * 100000);
  }
}
