//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "AEEStdErr.h"
#include "include_idl_1.h"
#include "include_idl_1_test.h"
#include "rpcmem.h"
#include <stdlib.h>
#include <stdio.h>
#include "unistd.h"
#include "util.h"

int include_idl_1_test(int domain, int num) {
  int nErr = AEE_SUCCESS;
  int* test = NULL;
  int  len = 0;
  int retry = 10;
  remote_handle64 handleSum = -1;
  remote_handle64 handleDiff = -1;
  char *uri = NULL;
  num = 15;

  len = sizeof(*test) * num;
  printf("\n- allocate %d bytes from ION heap\n", len);

  int heapid = RPCMEM_HEAP_ID_SYSTEM;
#if defined(SLPI) || defined(MDSP)
  heapid = RPCMEM_HEAP_ID_CONTIG;
#endif

  if (0 == (test = (int *)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, len))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }

  printf("- creating sequence of numbers from 0 to %d\n", num - 1);
  for(int i=0;i<num;i++)
	{
      test[i]=i;
	}

    printf("- compute sum on domain %d\n", domain);
  nErr = get_uri(domain, include_idl_1_URI, strlen(include_idl_1_URI), &uri);
  if (nErr) {
    printf("ERROR 0x%x: get_uri failed\n", nErr);
    goto bail;
  }
  do {
      if (AEE_SUCCESS == (nErr = include_idl_1_open(uri, &handleSum))) {
        printf("\n- call include_idl_1_sum on the DSP\n");
        nErr = include_idl_1_sum(handleSum, test, num);
      }

      if (!nErr) {
        printf("- Sum ran successfully!! \n");
        break;
      } else {
        if (nErr == AEE_ECONNRESET) {
          /* AEE_ECONNRESET is returned when Sub-system restart (SSR) happens */
          retry--;
        } else if (nErr == AEE_ENOSUCH || nErr == AEE_EBADSTATE) {
          /* AEE_ENOSUCH is returned when Protection domain restart (PDR) happens and
          AEE_EBADSTATE is returned when PD is exiting or crashing.*/
          retry -= 2;
        } else {
          break;
        }
      }
      /* Close the handle and retry handle open */
      if (handleSum != -1) {
        if (AEE_SUCCESS != (nErr = include_idl_1_close(handleSum))) {
          printf("ERROR 0x%x: Failed to close handle\n", nErr);
        }
      }
    } while(retry);

    if (nErr) {
      printf("- retry attempt unsuccessful. Timing out....\n");
      printf("ERROR 0x%x: Failed to compute sum on domain %d\n", nErr, domain);
    }

    if (AEE_SUCCESS == (nErr = include_idl_1_open(uri, &handleDiff))) {
        printf("\n- call include_idl_1_diff on the DSP\n");
        if (AEE_SUCCESS == (nErr = include_idl_1_diff(handleDiff, test, num))) {
            printf("Compute include_idl_1_diff successful on DSP\n");
        }
      }

    if (nErr) {
      printf("ERROR 0x%x: Failed to find diff on domain %d\n", nErr, domain);
	  }

    if (handleSum != -1) {
      if (AEE_SUCCESS != (nErr = include_idl_1_close(handleSum))) {
        printf("ERROR 0x%x: Failed to close handleSum\n", nErr);
      }
    }
    if (handleDiff != -1) {
      if (AEE_SUCCESS != (nErr = include_idl_1_close(handleDiff))) {
        printf("ERROR 0x%x: Failed to close handleDiff\n", nErr);
      }
    }
bail:
  if (test) {
    rpcmem_free(test);
  }
  return nErr;
}
