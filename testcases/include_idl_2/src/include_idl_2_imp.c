//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "HAP_farf.h"
#include "include_idl_2.h"

int include_idl_2_open(const char*uri, remote_handle64* handle) {
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
int include_idl_2_close(remote_handle64 handle) {
   if (handle)
      free((void*)handle);
   return 0;
}

int include_idl_2_sum(remote_handle64 h, const int* vec, int vecLen)
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

int include_idl_2_func(remote_handle64 h, const int* vec, int vecLen)
{
	int func=0;
	for(int j=0;j<vecLen;j++)
	{
		func+=vec[j];
	}
	if(func!=105)
	{
		FARF(ALWAYS,"Data Mismatch error func=%d",func);
		return -1;
	}
  	return 0;
}

int include_idl_2_sturct_func(remote_handle64 _h, const include_idl_2_name* n)
{
   long xvalue = n->x;
   xvalue+=50;
	if(xvalue!=150)
	{
		FARF(ALWAYS,"Data Mismatch error struct_func=%d",n->x);
		return -1;
	}
  	return 0;
}
