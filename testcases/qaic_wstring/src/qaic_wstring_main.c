#include "qaic_wstring_test.h"
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
    "    qaic_wstring [-d domain] [-U unsigned_PD] [-r run_locally] \n\n"
    "Options:\n"
    "-d domain: Run on a specific domain.\n"
    "    0: Run the example on ADSP\n"
    "    3: Run the example on CDSP\n"
    "    1: Run the example on MDSP\n"
    "    2: Run the example on SDSP\n"
    "        Default Value: 3(CDSP) for targets having CDSP and 0(ADSP) for targets not having CDSP like Agatti.\n"
    "-U unsigned_PD: Run on signed or unsigned PD.\n"
    "    0: Run on signed PD.\n"
    "    1: Run on unsigned PD.\n"
    "        Default Value: 1\n"
    );
}

int main(int argc, char* argv[])
{
  int nErr = 0;
  int runLocal = 1;
  int num = 0;
  int option = 0;

  while ((option = getopt(argc, argv,"r:n:")) != -1) {
    switch (option) {
      case 'r' : runLocal = atoi(optarg);
        break;
      case 'n' : num = atoi(optarg);
        break;
      default:
        print_usage();
      return -1;
    }
  }
  setbuf(stdout,NULL);
  printf("\n---Starting qaic_wstring test\n");

  nErr = qaic_wstring_test(runLocal, num);

  if (nErr) {
    printf("\n---Usage: %s <1/0 run locally> <uint32 size>\n\n", argv[0]);
  } else {
    printf("---Success\n\n");
  }

  return nErr;
}
