//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "AEEStdErr.h"
#include "complex_seq.h"
#include "complex_seq_test.h"
#include "rpcmem.h"
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
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

#define LIB_SIZE 2
int nErr = 0;

int local_complex_seq_sum(const int* vec, int vecLen, int64_t* res)
{
	int ii = 0;
	*res = 0;
	for (ii = 0; ii < vecLen; ++ii) {
		*res = *res + vec[ii];
	}
	return 0;
}

int complex_seq_test(int domain, int num, bool is_signedpd_requested) {
	int nErr = AEE_SUCCESS;
	int* test = 0;
	char* charseq = 0;
	_wchar_t* widecharseq = 0;
	short* shortintseq = 0;
	unsigned char* ucharseq = 0;
	int64_t* longseq = 0;
	unsigned short* ushortseq = 0;
	uint64_t* ulongseq = 0;
	int8_t* int8tseq = 0;
	int16_t* int16tseq = 0;
	int32_t* int32tseq = 0;
	int64_t* int64tseq = 0;
	float* floatseq = 0;
	double* doubleseq = 0;
	_cstring1_t* stringseq = 0;
	int len = 0;
	int ii;
	int64_t result = 0;
	int64_t result1 = 0;

	LIBS libs;
	complex_seq_type_LIB lib;

	int retry = 10;
	remote_handle64 handleSum = -1;
	remote_handle64 handleDiff = -1;
	remote_handle64 handleMult = -1;
	remote_handle64 handleMult1 = -1;
	remote_handle64 handleDiv = -1;
	remote_handle64 handleSeq = -1;
	char *uri = NULL;
	LIBS* structseq = NULL;
	int len4 = sizeof(LIBS) * LIB_SIZE;
	complex_seq_type_LIB1* simpleseq = NULL;
	int len5 = sizeof(complex_seq_type_LIB1) * LIB_SIZE;
	int i = 0;

	len = sizeof(*test) * num;

	printf("\n---Allocate memory from ION heap\n");

	int heapid = RPCMEM_HEAP_ID_SYSTEM;
	#if defined(SLPI) || defined(MDSP)
	heapid = RPCMEM_HEAP_ID_CONTIG;
	#endif

	if (0 == ((test = (int*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, len)) &&
			(charseq = (char*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(*charseq)*num)) &&
			(widecharseq = (_wchar_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(*widecharseq)*num)) &&
			(shortintseq = (short*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(*shortintseq)*num)) &&
			(ucharseq = (unsigned char*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(*ucharseq)*num)) &&
			(longseq = (int64_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(*longseq)*num)) &&
			(ushortseq = (unsigned short*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(*ushortseq)*num)) &&
			(ulongseq = (uint64_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(*ulongseq)*num)) &&
			(int8tseq = (int8_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(*int8tseq)*num)) &&
			(int16tseq = (int16_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(*int16tseq)*num)) &&
			(int32tseq = (int32_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(*int32tseq)*num)) &&
			(int64tseq = (int64_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(*int64tseq)*num)) &&
			(floatseq = (float*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(*floatseq)*num)) &&
			(doubleseq = (double*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(*doubleseq)*num)) &&
			(stringseq = (_cstring1_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(*stringseq)*num)))) {
	printf("---Error: alloc failed for sequences\n");
	nErr = -1;
	goto bail;
	}

	for(int i=0;i<num;i++)
	{
		stringseq[i].dataLen =9;
		if (0 == (stringseq[i].data = (char*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(char)*stringseq[i].dataLen))) {
			printf("---Error: alloc failed for strings\n");
			nErr = -1;
			goto bail;
		}
	}
	libs.test3Len = 40;
	if (0 == (libs.test3 = (int*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS,sizeof(int)*libs.test3Len ))) {
		printf("---Error: alloc failed for libs.test3\n");
		nErr = -1;
		goto bail;
	}
	if (0 == (structseq = (LIBS*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, len4))) {
		printf("---Error: alloc failed for structseq\n");
		nErr = -1;
		goto bail;
	}
	for (ii = 0; ii < LIB_SIZE; ++ii) {
		structseq[ii].test3Len = 40;
		if (0 == (structseq[ii].test3 = (int*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(int)*structseq[ii].test3Len))) {
			printf("---Error: alloc failed for test3 of structseq %p\n", structseq[ii].test3);
			nErr = -1;
			goto bail;
		}
    }
	if (0 == (simpleseq = (complex_seq_type_LIB1*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, len5))) {
		printf("---Error: alloc failed for simpleseq\n");
		nErr = -1;
		goto bail;
	}
	printf("---Creating sequence of numbers from 0 to %d\n", num - 1);

	for (ii = 0; ii < num; ++ii) {
		doubleseq[ii] = int64tseq[ii] = int32tseq[ii] = int16tseq[ii] = int8tseq[ii] = ulongseq[ii] = ushortseq[ii] = longseq[ii] = shortintseq[ii] = widecharseq[ii] =  charseq[ii] = ucharseq[ii] = floatseq[ii] = test[ii] =(char)((ii%4)+1);
		strncpy(stringseq[ii].data,"Hexagon",9);
		stringseq[ii].dataLen=8;
	}

	libs.k = 2.5;
	libs.l = 3.5;

	lib.ki = 45.25;
	lib.li = 45.75;
	lib.mi = 100;
	libs.color1 =  RED;

	for (i = 0; i < libs.test3Len; ++i) {
		libs.test3[i] = (i%4)+1;
	}

	for (ii = 0; ii < LIB_SIZE; ++ii) {
		structseq[ii].k = 2;
		structseq[ii].l = 3;
		structseq[ii].color1 = RED;
		structseq[ii].test3Len = 40;
		for (i = 0; i < structseq[ii].test3Len; ++i) {
			structseq[ii].test3[i] = (i%4)+1;
		}
    }

	for (ii = 0; ii < LIB_SIZE; ++ii) {
		simpleseq[ii].ki = 10;
		simpleseq[ii].li = 20;
		simpleseq[ii].mi = 20;
	}

	printf("- compute on domain %d\n", domain);

    if (domain == ADSP_DOMAIN_ID)
      uri = complex_seq_URI ADSP_DOMAIN;
    else if (domain == CDSP_DOMAIN_ID)
      uri = complex_seq_URI CDSP_DOMAIN;
    else if (domain == MDSP_DOMAIN_ID)
      uri = complex_seq_URI MDSP_DOMAIN;
    else if (domain == SDSP_DOMAIN_ID)
      uri = complex_seq_URI SDSP_DOMAIN;
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
	  nErr = complex_seq_open(uri, &handleSum);
	  if (AEE_SUCCESS == (nErr)) {
        printf("\n---Compute sum on the DSP nErr=%d\n", nErr);
        nErr = complex_seq_sum(handleSum, test, num, floatseq, num, charseq, num, widecharseq, num, shortintseq, num, ucharseq, num, longseq, num, ushortseq, num, ulongseq, num, int8tseq, num, int16tseq, num, int32tseq, num, int64tseq, num, doubleseq, num, stringseq, num);
      }

      if (!nErr) {
        printf("- sum executed successfully\n");
        break;
      } else {
        if (nErr == AEE_ECONNRESET) {
          //AEE_ECONNRESET is returned when Sub-system restart (SSR) happens
          retry--;
          sleep(5);
        } else if (nErr == AEE_ENOSUCH || nErr == AEE_EBADSTATE) {
          // AEE_ENOSUCH is returned when Protection domain restart (PDR) happens and
          //AEE_EBADSTATE is returned when PD is exiting or crashing.
          retry -= 2;
        } else {
          break;
        }
      }
	  // Close the handle and retry handle open
      if (handleSum != -1) {
        if (AEE_SUCCESS != (nErr = complex_seq_close(handleSum))) {
          printf("ERROR 0x%x: Failed to close handle\n", nErr);
        }
      }
    } while(retry);

    if (nErr) {
      printf("- retry attempt unsuccessful. Timing out....\n");
      printf("ERROR 0x%x: Failed to compute sum on domain %d\n", nErr, domain);
	  printf("ERROR 0x%x: num beyond 239 will exceed the boundary limit of 255 for input buffers\n",nErr);
	  printf("use num<240 and num must be divisible by 4 for successful execution of this example\n");
    }
	nErr = complex_seq_open(uri, &handleDiff);
	if (AEE_SUCCESS == (nErr)) {
      printf("\n- call complex_seq_diff on the DSP\n");
	  if (AEE_SUCCESS == (nErr = complex_seq_diff(handleDiff, test, num,floatseq,num,charseq,num,widecharseq,num,shortintseq,num,ucharseq,num,longseq,num,ushortseq,num,ulongseq,num,int8tseq,num,int16tseq,num,int32tseq,num,int64tseq,num,doubleseq,num,stringseq,num))) {
        int64_t res1 = 0;
		int64_t resf1 =0;
		int64_t resc1 = 0;
		int64_t reswc1 = 0;
		int64_t resshort1 = 0;
		int64_t resoctet1 = 0;
		int64_t reslonglong1 = 0;
		int64_t resushort1 = 0;
		int64_t resulonglong1 = 0;
		int64_t resint8t1 = 0;
		int64_t resint16t1 = 0;
		int64_t resint32t1 = 0;
		int64_t resint64t1 = 0;
		int64_t resd1 = 0;

		for (int ii = 0; ii < (num); ii = ii+4) {
			res1 = test[ii] + test[ii+1] + test[ii+2] + test[ii+3];
			resf1 = floatseq[ii] + floatseq[ii+1] + floatseq[ii+2] + floatseq[ii+3];
			resc1 = charseq[ii] + charseq[ii+1] + charseq[ii+2] + charseq[ii+3];
			reswc1 = widecharseq[ii] + widecharseq[ii+1] + widecharseq[ii+2] + widecharseq[ii+3];
			resshort1 = shortintseq[ii] + shortintseq[ii+1] + shortintseq[ii+2] + shortintseq[ii+3];
			resoctet1 = ucharseq[ii] + ucharseq[ii+1] + ucharseq[ii+2] + ucharseq[ii+3];
			reslonglong1 = longseq[ii] + longseq[ii+1] + longseq[ii+2] + longseq[ii+3];
			resushort1 = ushortseq[ii] + ushortseq[ii+1] + ushortseq[ii+2] + ushortseq[ii+3];
			resulonglong1 = ulongseq[ii] + ulongseq[ii+1] + ulongseq[ii+2] + ulongseq[ii+3];
			resint8t1 = int8tseq[ii] + int8tseq[ii+1] + int8tseq[ii+2] + int8tseq[ii+3];
			resint16t1 = int16tseq[ii] + int16tseq[ii+1] + int16tseq[ii+2] + int16tseq[ii+3];
			resint32t1 = int32tseq[ii] + int32tseq[ii+1] + int32tseq[ii+2] + int32tseq[ii+3];
			resint64t1 = int64tseq[ii] + int64tseq[ii+1] + int64tseq[ii+2] + int64tseq[ii+3];
			resd1 = doubleseq[ii] + doubleseq[ii+1] + doubleseq[ii+2] + doubleseq[ii+3];

			if(strcmp(stringseq[ii].data,"Octagon") != 0){
				printf("===============String data Mismatched %s===============\n",stringseq[ii].data);
				goto bail;
			}

			if ((int)res1 != 46 || (int)resf1 != 46 || (int)resc1 != 46 || (int)reswc1 != 46 || (int)resshort1 != 46 || (int)resoctet1 != 46 || (int)reslonglong1 != 46 || (int)resushort1 != 46 || (int)resulonglong1 != 46 || (int)resint8t1 != 46 || (int)resint16t1 != 46 || (int)resint32t1 != 46 || (int)resint64t1 != 46 || (int)resd1 != 46 ) {
				printf("Failed in complex_seq_diff - out sequence :: %" PRId64 " %d ===============\n", res1,ii);
				goto bail;
			}

			res1 = resf1 = resc1 = reswc1 = resshort1 = resoctet1 = reslonglong1 = resushort1 = resulonglong1 = resint8t1 = resint16t1 = resint32t1 = resint64t1 = resd1 =  0;
		}
		printf("Compute diff successful on DSP\n");
      }
    }

    if (nErr) {
      printf("ERROR 0x%x: Failed to compute diff on domain %d\n", nErr, domain);
	  printf("ERROR 0x%x: num beyond 240 will exceed the boundary limit of 255 for rout buffers\n",nErr);
	  printf("use num<241 and num must be divisible by 4 for successful execution of this example\n");
    }

	if (AEE_SUCCESS == (nErr = complex_seq_open(uri, &handleMult))) {
      printf("\n- call complex_seq_mult on the DSP\n");
      if (AEE_SUCCESS == (nErr = complex_seq_mult(handleMult, &libs,&lib ,&result))) {
        printf("Compute complex_seq_mult successful on DSP\n");
		printf("-- result for in structures %" PRId64 "\n", result);
      }
    }

    if (nErr) {
      printf("ERROR 0x%x: Failed to find mult on domain %d\n", nErr, domain);
    }

	if (AEE_SUCCESS == (nErr = complex_seq_open(uri, &handleMult1))) {
      printf("\n- call complex_seq_mult1 on the DSP\n");
      if (AEE_SUCCESS == (nErr = complex_seq_mult1(handleMult1, &libs,&lib ,&result))) {
		    int64_t res_local=0;
			for(i=0;i<libs.test3Len;i++){
				res_local += libs.test3[i];
			}
			res_local += libs.k + libs.l + libs.color1;
			res_local += lib.li + lib.ki + lib.mi;
			if(result!=229 && res_local!=229)
			{
				printf("Data Mismatch error, complex_seq_mult1 failed!!\n");
				nErr = -1;
				goto bail;
			}
			printf("Compute complex_seq_mult1 successful on DSP\n");
			printf("-- result for out structures %" PRId64 "\n", result);
      }
    }

    if (nErr) {
      printf("ERROR 0x%x: Failed to find mult1 on domain %d\n", nErr, domain);
	}

	if (AEE_SUCCESS == (nErr = complex_seq_open(uri, &handleDiv))) {
      printf("\n- call complex_seq_div on the DSP\n");
      if (AEE_SUCCESS == (nErr = complex_seq_div(handleDiv, structseq,LIB_SIZE,simpleseq,LIB_SIZE,&result))) {
        printf("Compute complex_seq_div successful on DSP\n");
      }
    }

    if (nErr) {
      printf("ERROR 0x%x: Failed to find div on domain %d\n", nErr, domain);
    }

	if (AEE_SUCCESS == (nErr = complex_seq_open(uri, &handleSeq))) {
      printf("\n- call complex_seq_seq on the DSP\n");
      if (AEE_SUCCESS == (nErr = complex_seq_seq(handleSeq, structseq,LIB_SIZE,simpleseq,LIB_SIZE,&result1))) {
			int ii = 0 ;
			int i = 0;
			int64_t res1 = 0;
			int64_t res2 = 0;
			int64_t res = 0;
			int64_t res_local = 0;
			int t = 0;
			for (ii=0 ; ii < LIB_SIZE ; ++ii ){
				t = structseq[ii].test3Len;
				for(i=0;i<t;i++){
					res = res + structseq[ii].test3[i];
				}
				res1 = structseq[ii].k + structseq[ii].l + structseq[ii].color1;
			}
			for (ii=0 ; ii < LIB_SIZE; ++ii ){
				res2 = simpleseq[ii].ki + simpleseq[ii].li + simpleseq[ii].mi;
			}
			res_local = res1 + res + res2 ;
			if((res1 != 50) || (res != 80) || (res2 != 50))
				goto bail;
			if(result1!=180 && res_local!=180)
			{
				printf("Data Mismatch Error, complex_seq_seq failed!!!\n");
				nErr=-1;
				goto bail;
			}
			printf("Compute complex_seq_seq successful on DSP\n");
			printf("result for output complex sequences %" PRId64 "\n",result1);
      }
    }

    if (nErr) {
      printf("ERROR 0x%x: Failed to find seq on domain %d\n", nErr, domain);
    }
    if (handleSum != -1) {
      if (AEE_SUCCESS != (nErr = complex_seq_close(handleSum))) {
        printf("ERROR 0x%x: Failed to close handleSum\n", nErr);
      }
    }

    if (handleDiff != -1) {
      if (AEE_SUCCESS != (nErr = complex_seq_close(handleDiff))) {
        printf("ERROR 0x%x: Failed to close handleDiff\n", nErr);
      }
    }
	if (handleMult != -1) {
      if (AEE_SUCCESS != (nErr = complex_seq_close(handleMult))) {
        printf("ERROR 0x%x: Failed to close handleMult\n", nErr);
      }
    }
	if (handleMult1 != -1) {
      if (AEE_SUCCESS != (nErr = complex_seq_close(handleMult1))) {
        printf("ERROR 0x%x: Failed to close handleMult1\n", nErr);
      }
    }
	if (handleDiv != -1) {
      if (AEE_SUCCESS != (nErr = complex_seq_close(handleDiv))) {
        printf("ERROR 0x%x: Failed to close handleDiv\n", nErr);
      }
    }
	if (handleSeq != -1) {
      if (AEE_SUCCESS != (nErr = complex_seq_close(handleSeq))) {
        printf("ERROR 0x%x: Failed to close handleSeq\n", nErr);
      }
    }

bail:
	if (test)
	    rpcmem_free(test);
	if(charseq)
	    rpcmem_free(charseq);
	if(widecharseq)
	    rpcmem_free(widecharseq);
	if(shortintseq)
	    rpcmem_free(shortintseq);
	if(ucharseq)
	    rpcmem_free(ucharseq);
	if(longseq)
	    rpcmem_free(longseq);
	if(ushortseq)
	    rpcmem_free(ushortseq);
	if(ulongseq)
	    rpcmem_free(ulongseq);
	if(int8tseq)
	    rpcmem_free(int8tseq);
	if(int16tseq)
	    rpcmem_free(int16tseq);
	if(int32tseq)
	    rpcmem_free(int32tseq);
	if(int64tseq)
	    rpcmem_free(int64tseq);
	if(floatseq)
	    rpcmem_free(floatseq);
	if(doubleseq)
	    rpcmem_free(doubleseq);
	for(int i=0;i<num;i++)
	{
		if (stringseq[i].data) {
			rpcmem_free(stringseq[i].data);
		}
	}
	if(stringseq)
	    rpcmem_free(stringseq);
	if(libs.test3)
		rpcmem_free(libs.test3);
	for (ii = 0; ii < LIB_SIZE; ++ii) {
		if (structseq[ii].test3) {
			rpcmem_free(structseq[ii].test3);
		}
	}
	if(structseq)
		rpcmem_free(structseq);
	if(simpleseq)
		rpcmem_free(simpleseq);

  return nErr;
}