//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "array_test.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "util.h"

static void print_usage()
{
  printf( "Usage:\n"
    "    array [-d domain] [-U unsigned_PD] -n array_size\n\n"
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
    "-n array_size: Natural number up to which sum is calculated from 0 to (n-1)\n"
    "        Default Value: 1\n"
    );
}

int main(int argc, char* argv[])
{
  int nErr = 0;
  int num = 1;
  int domain_id = 0;
  int requested_pd = 1;
  int option = 0;

  while ((option = getopt(argc, argv,"d:U:n:")) != -1) {
     switch (option) {
      case 'd' : domain_id = atoi(optarg);
        break;
      case 'U' : requested_pd = atoi(optarg);
        break;
      case 'n' : num = atoi(optarg);
        break;
      default:
        print_usage();
      return -1;
  }
  }
  if (domain_id < 0 || domain_id > 3) {
     nErr = -1;
     printf("\nInvalid domain %d\n", domain_id);
     goto bail;
  }
  printf("\n- Starting array test on domain %d\n", domain_id);
  nErr = set_unsigned_module_loading(domain_id, (requested_pd == 0));
  if (nErr) {
    printf("Warning: Failed to set module loading mode: 0x%x\n", nErr);
  }
  nErr = array_test(domain_id, num);

bail:
  if (nErr) {
    printf("- array example failed with nErr = %d\n\n", nErr);
  } else {
    printf("- success\n\n");
  }
  return nErr;
}
