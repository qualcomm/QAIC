//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "AEEStdErr.h"
#include "buffer_argument_testcase.h"
#include "buffer_argument_testcase_test.h"
#include "util.h"

int testcase1(remote_handle64 handle);
int testcase2(remote_handle64 handle);
int testcase3(remote_handle64 handle);
int testcase4(remote_handle64 handle);

int buffer_argument_testcase_test(int domain_id) {
  int nErr = AEE_SUCCESS;
  int retry = 10;
  remote_handle64 handle1 = -1;
  char *uri = NULL;

  printf("Compute sum on domain %d\n", domain_id);
  
/* Build URI using util helper */
    nErr = get_uri(domain_id, buffer_argument_testcase_URI,
                   (int)strlen(buffer_argument_testcase_URI), &uri);
    if (nErr) { 
        printf("ERROR: get_uri failed (%d)\n", nErr); 
        goto bail; 
    }
do {
    if (AEE_SUCCESS == (nErr = buffer_argument_testcase_open(uri, &handle1))) {
      printf("\nCall method1 on the DSP\n");
      if(AEE_SUCCESS ==(nErr = testcase1(handle1)))
        printf("Completion of method1 on the DSP\n");
    }

    if (!nErr) {
      break;
    } else {
      if (nErr == AEE_ECONNRESET) {
        /* In case of a Sub-system restart (SSR), AEE_ECONNRESET is returned by FastRPC
        and errno is set to ECONNRESET by the kernel.*/
        retry--;
      } else if (nErr == AEE_ENOSUCH || nErr == AEE_EBADSTATE) {

        /* AEE_ENOSUCH is returned when Protection domain restart (PDR) happens and
        AEE_EBADSTATE is returned from PD when exiting or crashing.*/
        /* Refer to AEEStdErr.h for more info on error codes*/
        retry -= 2;
      } else {
        break;
      }
    }
    /* Close the handle and retry handle open */
    if (handle1 != -1) {
      if (AEE_SUCCESS != (nErr = buffer_argument_testcase_close(handle1))) {
        printf("ERROR 0x%x: Failed to close handle\n", nErr);
      }
    }
  } while(retry);

  if (nErr) {
    printf("Retry attempt unsuccessful. Timing out....\n");
    printf("ERROR 0x%x: Failed to run method1 on domain %d\n", nErr, domain_id);
    goto bail;
  }

  printf("Call method2 on the DSP\n");
  if (AEE_SUCCESS == (nErr = testcase2(handle1))) {
    printf("Completion of method2 on the DSP\n");
  }
  if (nErr) {
    printf("ERROR 0x%x: Failed to run method2 on domain %d\n", nErr, domain_id);
  }
  if ((nErr = testcase3(handle1))) {
    printf("ERROR 0x%x: Failed to run method3 on domain %d\n", nErr, domain_id);
  }
  if ((nErr = testcase4(handle1))) {
    printf("ERROR 0x%x: Failed to run method4 on domain %d\n", nErr, domain_id);
  }

  if (nErr) {
    printf("ERROR 0x%x: Failed to run testcase1 on domain %d\n", nErr, domain_id);
  }

bail:
if (handle1 != -1) {
        int cerr = buffer_argument_testcase_close(handle1);
        if (AEE_SUCCESS != cerr) printf("ERROR 0x%x: Failed to close handle\n", cerr);
    }
    if (uri) free(uri);

  return nErr;
}