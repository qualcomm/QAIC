//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "AEEStdErr.h"
#include "include_idl_3.h"
#include "include_idl_3_test.h"
#include "rpcmem.h"
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "remote.h"
#include "unistd.h"

#pragma weak remote_session_control
#ifdef __hexagon__
#define sleep(x) {/* Do nothing for simulator */}
#endif

int local_include_idl_3_sum(int* vec, int vecLen)
{
    int sum=0;
	for(int j=0;j<vecLen;j++)
	{
		sum+=vec[j];
	}
	if(sum!=105)
	{
		return -1;
	}
  	return 0;
}

int include_idl_3_test(int domain, int num, bool is_signedpd_requested) {
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

    if (domain == ADSP_DOMAIN_ID)
      uri = include_idl_3_URI ADSP_DOMAIN;
    else if (domain == CDSP_DOMAIN_ID)
      uri = include_idl_3_URI CDSP_DOMAIN;
    else if (domain == MDSP_DOMAIN_ID)
      uri = include_idl_3_URI MDSP_DOMAIN;
    else if (domain == SDSP_DOMAIN_ID)
      uri = include_idl_3_URI SDSP_DOMAIN;
    else {
      nErr = AEE_EINVALIDDOMAIN;
      printf("ERROR 0x%x: unsupported domain %d\n", nErr, domain);
      goto bail;
    }

      if(remote_session_control) {
        struct remote_rpc_control_unsigned_module data;
        data.domain = domain;
        if (is_signedpd_requested)
          data.enable = 0;
        else
          data.enable = 1;
        if (AEE_SUCCESS != (nErr = remote_session_control(DSPRPC_CONTROL_UNSIGNED_MODULE, (void*)&data, sizeof(data)))) {
          printf("ERROR 0x%x: remote_session_control failed for CDSP\n", nErr);
          goto bail;
        }
      } else {
        nErr = AEE_EUNSUPPORTED;
        printf("ERROR 0x%x: remote_session_control interface is not supported on this device\n", nErr);
        goto bail;
      }

    do {
      if (AEE_SUCCESS == (nErr = include_idl_3_open(uri, &handleSum))) {
        printf("\n- call include_idl_3_sum on the DSP\n");
        nErr = include_idl_3_sum1(handleSum, test, num);
      }

      if (!nErr) {
        printf("- Sum ran successfully!! \n");
        break;
      } else {
        if (nErr == AEE_ECONNRESET) {
          /* AEE_ECONNRESET is returned when Sub-system restart (SSR) happens */
          retry--;
          sleep(5);
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
        if (AEE_SUCCESS != (nErr = include_idl_3_close(handleSum))) {
          printf("ERROR 0x%x: Failed to close handle\n", nErr);
        }
      }
    } while(retry);

    if (nErr) {
      printf("- retry attempt unsuccessful. Timing out....\n");
      printf("ERROR 0x%x: Failed to compute sum on domain %d\n", nErr, domain);
    }

    if (AEE_SUCCESS == (nErr = include_idl_3_open(uri, &handleDiff))) {
        printf("\n- call include_idl_3_diff on the DSP\n");
        if (AEE_SUCCESS == (nErr = include_idl_3_diff(handleDiff, test, num))) {
            printf("Compute include_idl_3_diff successful on DSP\n");
        }
      }

    if (nErr) {
      printf("ERROR 0x%x: Failed to find diff on domain %d\n", nErr, domain);
	  }

    if (handleSum != -1) {
      if (AEE_SUCCESS != (nErr = include_idl_3_close(handleSum))) {
        printf("ERROR 0x%x: Failed to close handleSum\n", nErr);
      }
    }
    if (handleDiff != -1) {
      if (AEE_SUCCESS != (nErr = include_idl_3_close(handleDiff))) {
        printf("ERROR 0x%x: Failed to close handleDiff\n", nErr);
      }
    }
bail:
  if (test) {
    rpcmem_free(test);
  }
  return nErr;
}
