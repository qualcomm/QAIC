//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "AEEStdErr.h"
#include "qaic_string.h"
#include "qaic_string_test.h"
#include "rpcmem.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "util.h"

#ifndef qaic_string_URI
#define qaic_string_URI "qaic_string"
#endif

typedef const _cstring1_t* STR_PTR;
int nErr = 0;

int qaic_string_test(int num, int domain_id)
{
  STR_PTR test;
  int len = 0;
  char result[7] = "result\0";
  int resLen = strlen(result);
  char *uri = NULL;
  num = 7; // Hard coding it just for testing

  len = sizeof(*test) * num;
  printf("\n---Allocate %d bytes from ION heap\n", len);

  int heapid = RPCMEM_HEAP_ID_SYSTEM;
  #if defined(SLPI) || defined(MDSP)
  heapid = RPCMEM_HEAP_ID_CONTIG;
  #endif

  if (0 == (test = (STR_PTR)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, len))) {
    printf("---Error: alloc failed\n");
	nErr = -1;
    goto bail;
  }

  printf("---Creating sequence of strings from 0 to %d\n", num - 1);
  _cstring1_t seq_ptr[] = {{"testing",7},{"sequence",8},{"of",2},{"strings",7},{"type",4},{"in",2},{"IDL",3}};
  test = seq_ptr;

  nErr = get_uri(domain_id, qaic_string_URI, strlen(qaic_string_URI), &uri);
  if (nErr) { printf("ERROR: get_uri failed (%d)\n", nErr);
    goto bail;
  }

  printf("\n---Process string on the DSP\n");
  if (0 != qaic_string_process_seq_string(test, num, result, resLen)) {
    printf("---Error: compute on DSP failed, nErr = %d\n", nErr);
    nErr = -1;
	  goto bail;
  }
  printf("result from DSP: %s\n",result);
bail:
if (uri) free(uri);
  if (test)
    rpcmem_free((void *)test);
  return nErr;
}