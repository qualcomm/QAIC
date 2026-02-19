/*==============================================================================
  Copyright (c) 2012-2014, 2020 Qualcomm Technologies, Inc.
  All rights reserved. Qualcomm Proprietary and Confidential.
==============================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "HAP_farf.h"
#include "include_idl_3.h"

int include_idl_3_open(const char*uri, remote_handle64* handle) {
   void *tptr = NULL;
  /* can be any value or ignored, rpc layer doesn't care
   * also ok
   * *handle = 0;
   * *handle = 0xdeadc0de;
   */
   tptr = (void *)malloc(1);
   *handle = (remote_handle64)tptr;
   return 0;
}

/**
 * @param handle, the value returned by open
 * @retval, 0 for success, should always succeed
 */
int include_idl_3_close(remote_handle64 handle) {
   if (handle)
      free((void*)handle);
   return 0;
}

int include_idl_3_sum1(remote_handle64 h, const int* vec, int vecLen)
{
	int sum=0;
	for(int j=0;j<vecLen;j++)
	{
		sum+=vec[j];
	}
	if(sum!=105)
	{
		FARF(ALWAYS,"Data Mismatch error sum=%d",sum);
		return -1;
	}
  	return 0;
}


int include_idl_3_diff(remote_handle64 h, const int* vec, int vecLen)
{
	int diff=0;
	for(int j=0;j<vecLen;j++)
	{
		diff+=vec[j];
	}
   diff-=100;
	if(diff!=5)
	{
		FARF(ALWAYS,"Data Mismatch error diff=%d",diff);
		return -1;
	}
  	return 0;
}