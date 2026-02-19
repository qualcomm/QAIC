/*==============================================================================
  Copyright (c) 2012-2014,2017,2020 Qualcomm Technologies, Inc.
  All rights reserved. Qualcomm Proprietary and Confidential.
==============================================================================*/

#include "AEEStdErr.h"
#include "include_idl_2.h"
#include "include_idl_2_test.h"
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

int local_include_idl_2_sum(int* vec, int vecLen)
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

int include_idl_2_test(int domain, int num, bool is_signedpd_requested) {
  int nErr = AEE_SUCCESS;
  int* test = NULL;
  int  len = 0;
  int retry = 10;
  include_idl_2_name* name;
  remote_handle64 handleSum = -1;
  remote_handle64 handleFunc = -1;
  remote_handle64 handleStructFunc = -1;
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
    printf("ERROR 0x%x: memory alloc failed for test\n", nErr);
    goto bail;
  }

  if (0 == (name = (include_idl_2_name *)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(*name)*1))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed for name\n", nErr);
    goto bail;
  }

  printf("- creating sequence of numbers from 0 to %d\n", num - 1);


	for(int i=0;i<num;i++)
	{
		test[i]=i;
	}

  name->x = 100;

    printf("- compute sum on domain %d\n", domain);

    if (domain == ADSP_DOMAIN_ID)
      uri = include_idl_2_URI ADSP_DOMAIN;
    else if (domain == CDSP_DOMAIN_ID)
      uri = include_idl_2_URI CDSP_DOMAIN;
    else if (domain == MDSP_DOMAIN_ID)
      uri = include_idl_2_URI MDSP_DOMAIN;
    else if (domain == SDSP_DOMAIN_ID)
      uri = include_idl_2_URI SDSP_DOMAIN;
    else {
      nErr = AEE_EINVALIDDOMAIN;
      printf("ERROR 0x%x: unsupported domain %d\n", nErr, domain);
      goto bail;
    }

      if(remote_session_control) {
        struct remote_rpc_control_unsigned_module data;
        data.domain = domain;
        if(is_signedpd_requested)
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
      if (AEE_SUCCESS == (nErr = include_idl_2_open(uri, &handleSum))) {
        printf("\n- call include_idl_2_sum on the DSP\n");
        nErr = include_idl_2_sum(handleSum, test, num);
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
        if (AEE_SUCCESS != (nErr = include_idl_2_close(handleSum))) {
          printf("ERROR 0x%x: Failed to close handle\n", nErr);
        }
      }
    } while(retry);

    if (nErr) {
      printf("- retry attempt unsuccessful. Timing out....\n");
      printf("ERROR 0x%x: Failed to compute sum on domain %d\n", nErr, domain);
    }

    if (AEE_SUCCESS == (nErr = include_idl_2_open(uri, &handleFunc))) {
        printf("\n- call include_idl_2_func on the DSP\n");
        if (AEE_SUCCESS == (nErr = include_idl_2_func(handleFunc, test, num))) {
            printf("Compute include_idl_2_func successful on DSP\n");
        }
      }

    if (nErr) {
      printf("ERROR 0x%x: Failed to find func on domain %d\n", nErr, domain);
	  }
	if (AEE_SUCCESS == (nErr = include_idl_2_open(uri, &handleStructFunc))) {
        printf("\n- call include_idl_2_sturct_func on the DSP\n");
        if (AEE_SUCCESS == (nErr = include_idl_2_sturct_func(handleStructFunc, name))) {
            printf("Compute include_idl_2_sturct_func successful on DSP\n");
        }
      }

    if (nErr) {
      printf("ERROR 0x%x: Failed to find sturct_func on domain %d\n", nErr, domain);
	  }

    if (handleSum != -1) {
      if (AEE_SUCCESS != (nErr = include_idl_2_close(handleSum))) {
        printf("ERROR 0x%x: Failed to close handleSum\n", nErr);
      }
    }
    if (handleFunc != -1) {
      if (AEE_SUCCESS != (nErr = include_idl_2_close(handleFunc))) {
        printf("ERROR 0x%x: Failed to close handleFunc\n", nErr);
      }
    }
	if (handleStructFunc != -1) {
      if (AEE_SUCCESS != (nErr = include_idl_2_close(handleStructFunc))) {
        printf("ERROR 0x%x: Failed to close handleStructFunc\n", nErr);
      }
    }
bail:
  if (test) {
    rpcmem_free(test);
  }
  return nErr;
}
