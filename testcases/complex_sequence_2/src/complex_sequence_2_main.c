//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "complex_sequence_2_test.h"
#include "rpcmem.h"
#include "remote.h"
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include "AEEStdErr.h"

static void print_usage()
{
  printf( "Usage:\n"
    "    complex_sequence_2 [-d domain] [-U unsigned_PD] [-r run_locally] -n array_size\n\n"
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
  int domain_id = -1;
  int requested_pd = 1;
  bool is_signedpd_requested = false;
  int option = 0;

  while ((option = getopt(argc, argv,"d:U:")) != -1) {
    switch (option) {
      case 'd' : domain_id = atoi(optarg);
        break;
      case 'U' : requested_pd = atoi(optarg);
        break;
      default:
        print_usage();
      return -1;
    }
  }


  if (domain_id == -1) domain_id = CDSP_DOMAIN_ID;
  if (domain_id < 0 || domain_id > 3) {
    nErr = AEE_EBADPARM;
    printf("\nERROR 0x%x: Invalid domain %d\n", nErr, domain_id);
    print_usage();
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


  printf("\nStarting complex_sequence_2 test\n");
  printf("Attempting to run on %s PD on domain %d\n", is_signedpd_requested?"signed":"unsigned", domain_id);


  nErr = complex_sequence_2_test(domain_id, is_signedpd_requested);
  if (nErr) {
    printf("ERROR 0x%x: complex_sequence_2 test failed\n\n", nErr);
  }

bail:
  if (nErr) {
    printf("ERROR 0x%x: complex_sequence_2 example failed\n\n", nErr);
  } else {
    printf("Success\n\n");
  }

  return nErr;
}
