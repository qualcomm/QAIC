//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "buffer_argument_testcase_test.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "util.h"

static void print_usage()
{
  printf( "Usage:\n"
    "    buffer_argument_testcase [-d domain] [-U unsigned_PD] \n\n"
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
  printf("\n- Starting buffer_argument_testcase on (%s) with unsigned_pd=%d\n",
         get_domain_name(domain_id), requested_pd);
  nErr = set_unsigned_module_loading(domain_id, (requested_pd == 0));
  if (nErr) {
    if (requested_pd == 1) {
      printf("ERROR 0x%x: Failed to enable unsigned module loading; aborting\n", nErr);
      return nErr;
  }
  printf("Warning: Failed to set module loading mode: 0x%x (continuing)\n", nErr);
}
  nErr = buffer_argument_testcase_test(domain_id);
  if (nErr) {
    printf("ERROR 0x%x: buffer_argument_testcase example failed\n\n", nErr);
  } else {
    printf("Success\n\n");
  }
  return nErr;
}
