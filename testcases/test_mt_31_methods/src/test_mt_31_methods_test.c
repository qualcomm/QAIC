//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "AEEStdErr.h"
#include "test_mt_31_methods.h"
#include "test_mt_31_methods_test.h"
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

int local_test_mt_31_methods_sum(alpha* vec, int64_t* res)
{
    printf("reached local execution\n");
	int sum=0;
	for(int i=0;i<20;i++)
	{
		sum+=(vec->number)[i];
	}

	*res = sum;
  	return 0;
}

int test_mt_31_methods_test(int domain, int num, bool is_signedpd_requested) {
  int nErr = AEE_SUCCESS;
  alpha* test = NULL;
  int  len = 0;
  int64_t resultSum0 = 0;  int64_t resultSum1 = 0;  int64_t resultSum2 = 0;  int64_t resultSum3 = 0;  int64_t resultSum4 = 0;  int64_t resultSum5 = 0;  int64_t resultSum6 = 0;  int64_t resultSum7 = 0;  int64_t resultSum8 = 0;  int64_t resultSum9 = 0;  int64_t resultSum10 = 0;  int64_t resultSum11 = 0;  int64_t resultSum12 = 0;  int64_t resultSum13 = 0;  int64_t resultSum14 = 0;  int64_t resultSum15 = 0;  int64_t resultSum16 = 0;  int64_t resultSum17 = 0;  int64_t resultSum18 = 0;  int64_t resultSum19 = 0;  int64_t resultSum20 = 0;  int64_t resultSum21 = 0;  int64_t resultSum22 = 0;  int64_t resultSum23 = 0;  int64_t resultSum24 = 0;  int64_t resultSum25 = 0;  int64_t resultSum26 = 0;  int64_t resultSum27 = 0;  int64_t resultSum28 = 0;  int64_t resultSum29 = 0;  int64_t resultSum30 = 0;  int64_t resultSum31 = 0;  int64_t resultSum32 = 0;  int64_t resultSum33 = 0;  int64_t resultSum34 = 0;  int64_t resultSum35 = 0;
  remote_handle64 handle = -1;

  char *uri = NULL;
  num = 15;

  len = sizeof(*test) * 1;
  printf("\n- allocate %d bytes from ION heap\n", len);

  int heapid = RPCMEM_HEAP_ID_SYSTEM;
#if defined(SLPI) || defined(MDSP)
  heapid = RPCMEM_HEAP_ID_CONTIG;
#endif

  if (0 == (test = (alpha *)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, len))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }

  printf("- putting number in test \n");
	for(int i=0;i<20;i++)
	{
		(test->number)[i] = i;
	}

    printf("- compute sum on domain %d\n", domain);

    if (domain == ADSP_DOMAIN_ID)
      uri = test_mt_31_methods_URI ADSP_DOMAIN;
    else if (domain == CDSP_DOMAIN_ID)
      uri = test_mt_31_methods_URI CDSP_DOMAIN;
    else if (domain == MDSP_DOMAIN_ID)
      uri = test_mt_31_methods_URI MDSP_DOMAIN;
    else if (domain == SDSP_DOMAIN_ID)
      uri = test_mt_31_methods_URI SDSP_DOMAIN;
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

    if (AEE_SUCCESS == (nErr = test_mt_31_methods_open(uri, &handle))) {
      printf("\n- call test_mt_31_methods_sum0 on the DSP\n");
      nErr = test_mt_31_methods_sum0(handle, test, &resultSum0);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum1 on the DSP\n");
      nErr = test_mt_31_methods_sum1(handle, test, &resultSum1);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum1 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum2 on the DSP\n");
      nErr = test_mt_31_methods_sum2(handle, test, &resultSum2);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum2 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum3 on the DSP\n");
      nErr = test_mt_31_methods_sum3(handle, test, &resultSum3);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum3 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum4 on the DSP\n");
      nErr = test_mt_31_methods_sum4(handle, test, &resultSum4);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum4 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum5 on the DSP\n");
      nErr = test_mt_31_methods_sum5(handle, test, &resultSum5);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum5 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum6 on the DSP\n");
      nErr = test_mt_31_methods_sum6(handle, test, &resultSum6);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum6 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum7 on the DSP\n");
      nErr = test_mt_31_methods_sum7(handle, test, &resultSum7);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum7 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum8 on the DSP\n");
      nErr = test_mt_31_methods_sum8(handle, test, &resultSum8);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum8 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum9 on the DSP\n");
      nErr = test_mt_31_methods_sum9(handle, test, &resultSum9);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum9 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum10 on the DSP\n");
      nErr = test_mt_31_methods_sum10(handle, test, &resultSum10);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum10 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum11 on the DSP\n");
      nErr = test_mt_31_methods_sum11(handle, test, &resultSum11);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum11 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum12 on the DSP\n");
      nErr = test_mt_31_methods_sum12(handle, test, &resultSum12);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum12 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum13 on the DSP\n");
      nErr = test_mt_31_methods_sum13(handle, test, &resultSum13);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum13 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum14 on the DSP\n");
      nErr = test_mt_31_methods_sum14(handle, test, &resultSum14);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum14 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum15 on the DSP\n");
      nErr = test_mt_31_methods_sum15(handle, test, &resultSum15);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum15 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum16 on the DSP\n");
      nErr = test_mt_31_methods_sum16(handle, test, &resultSum16);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum16 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum17 on the DSP\n");
      nErr = test_mt_31_methods_sum17(handle, test, &resultSum17);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum17 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum18 on the DSP\n");
      nErr = test_mt_31_methods_sum18(handle, test, &resultSum18);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum18 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum19 on the DSP\n");
      nErr = test_mt_31_methods_sum19(handle, test, &resultSum19);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum19 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum20 on the DSP\n");
      nErr = test_mt_31_methods_sum20(handle, test, &resultSum20);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum20 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum21 on the DSP\n");
      nErr = test_mt_31_methods_sum21(handle, test, &resultSum21);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum21 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum22 on the DSP\n");
      nErr = test_mt_31_methods_sum22(handle, test, &resultSum22);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum22 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum23 on the DSP\n");
      nErr = test_mt_31_methods_sum23(handle, test, &resultSum23);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum23 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum24 on the DSP\n");
      nErr = test_mt_31_methods_sum24(handle, test, &resultSum24);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum24 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum25 on the DSP\n");
      nErr = test_mt_31_methods_sum25(handle, test, &resultSum25);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum25 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum26 on the DSP\n");
      nErr = test_mt_31_methods_sum26(handle, test, &resultSum26);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum26 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum27 on the DSP\n");
      nErr = test_mt_31_methods_sum27(handle, test, &resultSum27);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum27 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum28 on the DSP\n");
      nErr = test_mt_31_methods_sum28(handle, test, &resultSum28);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum28 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum29 on the DSP\n");
      nErr = test_mt_31_methods_sum29(handle, test, &resultSum29);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum29 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum30 on the DSP\n");
      nErr = test_mt_31_methods_sum30(handle, test, &resultSum30);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum30 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum31 on the DSP\n");
      nErr = test_mt_31_methods_sum31(handle, test, &resultSum31);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum31 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum32 on the DSP\n");
      nErr = test_mt_31_methods_sum32(handle, test, &resultSum32);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum32 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum33 on the DSP\n");
      nErr = test_mt_31_methods_sum33(handle, test, &resultSum33);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum33 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum34 on the DSP\n");
      nErr = test_mt_31_methods_sum34(handle, test, &resultSum34);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum34 on domain %d\n", nErr, domain);
        goto bail;
      }
      printf("\n- call test_mt_31_methods_sum35 on the DSP\n");
      nErr = test_mt_31_methods_sum35(handle, test, &resultSum35);
      if (nErr) {
        printf("ERROR 0x%x: Failed to find sum35 on domain %d\n", nErr, domain);
        goto bail;
      }
    }
    if ((AEE_SUCCESS == nErr) && (((int)resultSum0 ==190) && ((int)resultSum1 ==190) && ((int)resultSum2 ==190) && ((int)resultSum3 ==190) && ((int)resultSum4 ==190) && ((int)resultSum5 ==190) && ((int)resultSum6 ==190) && ((int)resultSum7 ==190) && ((int)resultSum8 ==190) && ((int)resultSum9 ==190) && ((int)resultSum10 ==190) && ((int)resultSum11 ==190) && ((int)resultSum12 ==190) && ((int)resultSum13 ==190) && ((int)resultSum14 ==190) && ((int)resultSum15 ==190) && ((int)resultSum16 ==190) && ((int)resultSum17 ==190) && ((int)resultSum18 ==190) && ((int)resultSum19 ==190) && ((int)resultSum20 ==190) && ((int)resultSum21 ==190) && ((int)resultSum22 ==190) && ((int)resultSum23 ==190) && ((int)resultSum24 ==190) && ((int)resultSum25 ==190) && ((int)resultSum26 ==190) && ((int)resultSum27 ==190) && ((int)resultSum28 ==190) && ((int)resultSum29 ==190) && ((int)resultSum30 ==190) && ((int)resultSum31 ==190) && ((int)resultSum32 ==190) && ((int)resultSum33 ==190) && ((int)resultSum34 ==190) && ((int)resultSum35 ==190))) {
        printf("Testcase executed successfully!!\n");
    }
    else{
      printf("Testcase failed!!!\n");
    }
    if (handle != -1) {
      if (AEE_SUCCESS != (nErr = test_mt_31_methods_close(handle))) {
        printf("ERROR 0x%x: Failed to close handle\n", nErr);
      }
    }

bail:
  if (test) {
    rpcmem_free(test);
  }
  return nErr;
}
