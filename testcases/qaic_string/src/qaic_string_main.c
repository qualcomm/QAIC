#include "qaic_string_test.h"
#include "rpcmem.h"
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

static void print_usage()
{
  printf( "Usage:\n"
    "    qaic_string [-d domain] [-U unsigned_PD]\n\n"
    "Options:\n"
    "-d domain: Run on a specific domain.\n"
    "    0: Run the example on ADSP\n"
    "    3: Run the example on CDSP\n"
    "    1: Run the example on MDSP\n"
    "    2: Run the example on SDSP\n"
    "        Default Value: 3(CDSP) for targets having CDSP and 0(ADSP) for targets not having CDSP like Agatti.\n"
    " -U unsigned_PD: 0=signed PD, 1=unsigned PD\n"
    "        Default Value: 1\n"
    );
}

int main(int argc, char* argv[])
{
  int nErr = 0;
  int num = 7;
  int domain_id = 3;
  int requested_pd = 0;
  bool is_signedpd_requested = false;
  int option = 0;

  while ((option = getopt(argc, argv,"d:U:")) != -1) {
    switch (option) {
      case 'd' : domain_id= atoi(optarg);
        break;
      case 'U' : requested_pd = atoi(optarg);
        break;
      default:
        print_usage();
      return -1;
    }
  }
  if (requested_pd == 0) {
      is_signedpd_requested = true;
  } else {
      is_signedpd_requested = false;
  }

printf("Starting qaic_string test: domain=%d, num=%d, SignedPD=%s\n", 
         domain_id, num, is_signedpd_requested ? "True" : "False");

  nErr = qaic_string_test(num, domain_id, is_signedpd_requested);

  if (nErr == 0) {
    printf("---Success\n");
  } else {
    printf("---Failure (Error code: %d)\n", nErr);
  }

  return nErr;
}
