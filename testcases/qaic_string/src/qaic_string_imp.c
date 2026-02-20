/*==============================================================================
  Copyright (c) 2012-2014 Qualcomm Technologies, Inc.
  All rights reserved. Qualcomm Proprietary and Confidential.
==============================================================================*/

#include <stdio.h>
#include "HAP_farf.h"
#include "qaic_string.h"
#include <string.h>
#include <limits.h>

typedef const _cstring1_t* STR_PTR;

int qaic_string_process_seq_string(STR_PTR seq, int seqLen, char* res, int resLen)
{
	printf("From IMP file\n");
	for(int i=0;i<seqLen;i++)
	{
     	printf("seq[i].data= %s     seq[i].dataLen=%d\n",seq[i].data,seq[i].dataLen);
	}
    for(int i=0;i<resLen-1;i++)
	{
		res[i]=(seq[0].data)[i];
	}
	res[resLen-1]='\0';
    FARF(RUNTIME_HIGH, "===============     DSP: %s ==============", res);
	return 0;
}