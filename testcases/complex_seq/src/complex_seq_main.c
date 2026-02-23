//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "complex_seq_test.h"
#include "rpcmem.h"
#include "remote.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "AEEStdErr.h"

static void print_usage()
{
  printf( "Usage:\n"
    "    complex_seq [-d domain] [-U unsigned_PD] -n array_size\n\n"
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
    "        Default Value: 4\n"
    );
}

int main(int argc, char* argv[])
{
  int nErr = 0;
  int num = 4;
  int domain = 0;
  int requested_pd = 0;
  bool is_signedpd_requested = false;
  int option = 0;

  while ((option = getopt(argc, argv,"d:U:n:")) != -1) {
    switch (option) {
      case 'd' : domain = atoi(optarg);
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


  if (domain < 0 || domain > 3) {
     nErr = -1;
     printf("\nInvalid domain %d\n", domain);
     goto bail;
  }

  if (requested_pd == 0)
    is_signedpd_requested = true;
  else if (requested_pd == 1)
    is_signedpd_requested = false;
  else {
    nErr = AEE_EBADPARM;
    printf("\nERROR 0x%x: Invalid unsigned PD flag %d\n", nErr, requested_pd);
    print_usage();
    goto bail;
  }


  printf("\n- Starting complex_seq test on domain %d\n", domain);
  nErr = complex_seq_test(domain, num, is_signedpd_requested);

bail:
  if (nErr) {
    printf("- complex_seq example failed with nErr = %d\n\n", nErr);
  } else {
    printf("- success\n\n");
  }

  return nErr;
}
