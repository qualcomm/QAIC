//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "complex_sequence_2_test.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "AEEStdErr.h"
#include "util.h"

static void print_usage()
{
  printf( "Usage:\n"
    "    complex_sequence_2 [-d domain] [-U unsigned_PD] \n\n"
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
  printf("\nStarting complex_sequence_2 test on domain %d\n", domain_id);
  nErr = set_unsigned_module_loading(domain_id, (requested_pd == 0));
  if (nErr) {
    printf("Warning: Failed to set module loading mode: 0x%x\n", nErr);
  }
  nErr = complex_sequence_2_test(domain_id);
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
