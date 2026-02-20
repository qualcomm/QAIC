/*==============================================================================
  Copyright (c) 2012-2014 Qualcomm Technologies, Inc.
  All rights reserved. Qualcomm Proprietary and Confidential.
==============================================================================*/

#include <stdio.h>
#include "HAP_farf.h"
#include "qaic_wstring.h"
#include <string.h>
#include <limits.h>
#include <wchar.h>

typedef const _wstring1_t* WSTR_PTR;

int qaic_wstring_process_seq_wstring(WSTR_PTR seq, int seqLen, _wchar_t* res, int resLen)
{
	printf("wide strings recieved in imp.c: \n");
	for(int i=0;i<seqLen;i++)
	{
		printf("dataLen=%d  data=",seq[i].dataLen);
		for(int j=0;j<7;j++)
		{
			printf("%lc", (wchar_t) (seq[i].data)[j]);
		}
		printf("\n");
	}
	int len;
	if(resLen < seq[0].dataLen){
		len=resLen;
	}
	else
	{
		len=seq[0].dataLen;
	}
	for(int i=0;i<len-1;i++)
	{
		res[i]=(seq[0].data)[i];
	}
	res[len-1]=(_wchar_t)L'\0';
	FARF(RUNTIME_HIGH, "===============DSP: ==============");
	printf("result on DSP= ");
	for(int i=0;i<len;i++)
	{
		printf("%lc",res[i]);
	}
	printf("\n");
	return 0;
}