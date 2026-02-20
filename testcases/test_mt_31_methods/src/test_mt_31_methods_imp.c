/*==============================================================================
  Copyright (c) 2012-2014, 2020 Qualcomm Technologies, Inc.
  All rights reserved. Qualcomm Proprietary and Confidential.
==============================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "test_mt_31_methods.h"

int test_mt_31_methods_open(const char*uri, remote_handle64* handle) {
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
int test_mt_31_methods_close(remote_handle64 handle) {
   if (handle)
      free((void*)handle);
   return 0;
}

int test_mt_31_methods_sum0(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum1(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum2(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum3(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum4(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum5(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum6(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum7(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum8(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum9(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}


int test_mt_31_methods_sum10(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum11(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum12(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum13(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum14(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum15(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum16(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum17(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum18(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum19(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}


int test_mt_31_methods_sum20(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum21(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum22(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum23(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum24(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum25(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum26(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum27(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum28(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum29(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}


int test_mt_31_methods_sum30(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum31(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum32(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum33(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum34(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}

int test_mt_31_methods_sum35(remote_handle64 h, const alpha* vec, int64_t* res)
{
	// FARF(ALWAYS, "reached imp execution");
	int sum = 0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
		//FARF(ALWAYS,"(vec.number)[%d]=%d",i,(vec->number)[i]);
	}
	*res = sum;
	if(*res!=190)
	{
		// FARF(ALWAYS,"Data Mismatch Error!!!");
		return -1;
	}
	// FARF(ALWAYS, "====DSP: res=%lld====\n", *res);
  	return 0;
}