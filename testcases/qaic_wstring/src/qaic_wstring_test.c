//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "AEEStdErr.h"
#include "qaic_wstring.h"
#include "qaic_wstring_test.h"
#include "rpcmem.h"
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dlfcn.h>
#include <unistd.h>
#include <string.h>
#include <wchar.h>

typedef const _wstring1_t* WSTR_PTR;

int nErr = 0;
int local_qaic_wstring_process_seq_wstring(WSTR_PTR seq, int seqLen, _wchar_t* res, int resLen)
{
	printf("Sequence of wide strings recieved in local method : \n");
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
	if (resLen < seq[0].dataLen){
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
	printf("result in local function= ");
	for(int i=0;i<len;i++)
	{
		printf("%lc",res[i]);
	}
	printf("\n");
	return 0;
}

int qaic_wstring_test(int runLocal, int num)
{
	_wstring1_t* test;
	int len = 0;
	int resLen=6;
	_wchar_t result[6];
	num = 7; // Hard coding it just for testing

	len = sizeof(*test) * num;
	printf("\n---Allocate %d bytes from ION heap for test\n", len);

	int heapid = RPCMEM_HEAP_ID_SYSTEM;
	#if defined(SLPI) || defined(MDSP)
	heapid = RPCMEM_HEAP_ID_CONTIG;
	#endif

	if (0 == (test = (_wstring1_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, len))) {
	    printf("---Error: alloc failed for test\n");
		nErr = -1;
	    goto bail;
	}

	printf("---Creating sequence of strings from 0 to %d\n", num - 1);
	for(int i=0;i<num;i++)
	{
		int len1=7;
		_wchar_t * seq;
		len1 = sizeof(*seq) * 7;

		if (0 == (test[i].data = (_wchar_t *)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, len1))) {
	    	printf("---Error: alloc failed for seq\n");
	    	nErr = -1;
	    	goto bail;
	    }

	    if (0 == (seq = (_wchar_t *)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, len1))) {
	       printf("---Error: alloc failed for seq\n");
	      	nErr = -1;
	     goto bail;
	    }
	    for(int i=0;i<6;i++)
	    {
	      seq[i]=(_wchar_t)(L's'+i);
	    }
	    seq[6]=(_wchar_t)L'\0';
	    test[i].data = seq;
	    test[i].dataLen = 7;
	}

	if (runLocal) {
	    printf("\n---Process string locally\n");
	    if (0 != local_qaic_wstring_process_seq_wstring(test, num, result,resLen)) {
	    	printf("Error: local compute failed\n");
		  	nErr = -1;
	      	goto bail;
	    }
	  } else {
	#ifdef __hexagon__
	    printf("\n---Process string on the DSP simulator\n");
	    if (0 != qaic_wstring_process_seq_wstring(test, num, result, resLen)) {
	      	printf("Error: compute on DSP simulator failed\n");
		  	nErr = -1;
	      	goto bail;
	    }
	#else

		void* H = 0;
		int (*func_ptr)(WSTR_PTR test, int len, _wchar_t* result, int resLen);
		H = dlopen("libqaic_wstring.so", RTLD_NOW);
		if (!H) {
		    printf("---ERROR, Failed to load libqaic_wstring.so\n");
			nErr = -1;
			goto bail;
		}

		func_ptr = (int (*)(WSTR_PTR, int, _wchar_t*, int))dlsym(H, "qaic_wstring_process_seq_wstring");
		if (!func_ptr) {
		   	printf("---ERROR, qaic_wstring_process_seq_wstring not found\n");
		  	dlclose(H);
		  	nErr = -1;
		   	goto bail;
		}

		printf("\n---Process string on the DSP with test array as: \n");
		if (0 != (nErr = (*func_ptr)(test, num, result, resLen))) {
		   	printf("---Error: compute on DSP failed, nErr = %d\n", nErr);
		   	printf("\n");
		  	dlclose(H);
		    goto bail;
		}
		printf("result received from DSP= ");
		for(int i=0;i<6;i++)
		{
			printf("%lc",result[i]);
		}
		printf("\n");
		dlclose(H);
	#endif
	}
	bail:
		if (test)
	    	rpcmem_free((void *)test);
	  	return nErr;
}