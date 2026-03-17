//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "qaic_string_test.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "util.h"

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
    "        Default Value: 3(CDSP) for targets having CDSP \n"
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

printf("Starting qaic_string test: domain=%d, num=%d, SignedPD=%s\n", 
         domain_id, num, (requested_pd == 0) ? "True" : "False");
nErr = set_unsigned_module_loading(domain_id, (requested_pd == 0));
if (nErr) {
  printf("Warning: Failed to set module loading mode: 0x%x\n", nErr);
}
  nErr = qaic_string_test(num, domain_id);

  if (nErr == 0) {
    printf("---Success\n");
  } else {
    printf("---Failure (Error code: %d)\n", nErr);
  }
  return nErr;
}
