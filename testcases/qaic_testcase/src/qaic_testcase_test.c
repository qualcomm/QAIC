//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "AEEStdErr.h"
#include "qaic_testcase.h"
#include "qaic_testcase_test.h"
#include "rpcmem.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "util.h"

int nErr = 0;

int qaic_testcase_test(int domain_id) {

  int retry=10;
  _wchar_t arg1 = (wchar_t)65;
  unsigned char arg2= (char) 97;
  char arg3= (char) 100;
  short arg4= -1;
  unsigned short arg5 = 1;
  int arg6 = -10;
  unsigned int arg7 = 10;
  int64_t arg8 = -100;
  uint64_t arg9 = 100;
  int8_t var = 2;
  uint8_t var1 = 3;
  int16_t var2 = 4;
  uint16_t var3 = 5;
  int32_t var4 = 6;
  uint32_t var5 = 7;
  int64_t var6 = 8;
  uint64_t var7 = 9;
  int8_t var8 = 10;
  uint8_t var9 = 11;
  int16_t num1 = 12;
  uint16_t num2 = 13;
  int32_t num3 = 14;
  uint32_t num4 = 15;
  int64_t num5 = 16;
  uint64_t num6 = 17;
  float num7 = 18;
  double num8 = 19;
  bool num9 = 0;
  traffic enum1 = GREEN;
  alpha *struc1 = NULL, *struc2 = NULL, *seq1 = NULL, *seq2 = NULL;
  int seqLen = 2;
  remote_handle64 handleSum = -1;
  char *uri = NULL;
  int heapid = RPCMEM_HEAP_ID_SYSTEM;
  #if defined(SLPI) || defined(MDSP)
    heapid = RPCMEM_HEAP_ID_CONTIG;
  #endif

  if (0 == (struc1 = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  struc1->enum1 = RED;
  struc1->enum2Len = 3;
  if (0 == (struc1->enum2 = (traffic*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(traffic)*(struc1->enum2Len)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  struc1->enum2[0] = RED;
  struc1->enum2[1] = YELLOW;
  struc1->enum2[2] = GREEN;
  if (0 == (struc2 = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  struc2->enum2Len = 3;
  if (0 == (struc2->enum2 = (traffic*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(traffic)*(struc2->enum2Len)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }

  if (0 == (seq1 = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)* seqLen))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }

  for(int i=0;i<seqLen;i++){
    seq1[i].enum2Len = 3;
    seq1[i].enum1 = RED;
    if (0 == (seq1[i].enum2 = (traffic*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(traffic)*(seq1[i].enum2Len)))) {
      nErr = AEE_ENORPCMEMORY;
      printf("ERROR 0x%x: memory alloc failed\n", nErr);
      goto bail;
    }
    for(int j=0;j<seq1[i].enum2Len;j++){
      seq1[i].enum2[j] = YELLOW;
    }
  }

  if (0 == (seq2 = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)* seqLen))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }

  for(int i=0;i<seqLen;i++){
    seq2[i].enum2Len = 3;
    if (0 == (seq2[i].enum2 = (traffic*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(traffic)*(seq2[i].enum2Len)))) {
      nErr = AEE_ENORPCMEMORY;
      printf("ERROR 0x%x: memory alloc failed\n", nErr);
      goto bail;
    }
  }

  printf("Compute sum on domain %d\n", domain_id);

  nErr = get_uri(domain_id, qaic_testcase_URI, (int)strlen(qaic_testcase_URI), &uri);
  if (nErr) { 
    printf("ERROR: get_uri failed (%d)\n", nErr);
      goto bail;
  }

  do {
    if (AEE_SUCCESS == (nErr = qaic_testcase_open(uri, &handleSum))) {
      printf("\nCall m1 on the DSP\n");
      nErr = qaic_testcase_m1(handleSum, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, var, var1, var2, var3, var4, var5, var6, var7, var8, var9, num1, num2, num3,num4, num5, num6, num7, num8, num9, enum1);
    }
    if (!nErr) {
      printf("Completion of m1 on the DSP\n");
      break;
    } else {
      if (nErr == AEE_ECONNRESET) {
        /* In case of a Sub-system restart (SSR), AEE_ECONNRESET is returned by FastRPC
        and errno is set to ECONNRESET by the kernel.*/
        retry--;
      } else if (nErr == AEE_ENOSUCH || nErr == AEE_EBADSTATE) {
        /* AEE_ENOSUCH is returned when Protection domain restart (PDR) happens and
        AEE_EBADSTATE is returned from DSP when PD is exiting or crashing.*/
        /* Refer to AEEStdErr.h for more info on error codes*/
        retry -= 2;
      } else {
        break;
      }
    }

    /* Close the handle and retry handle open */
    if (handleSum != -1) {
      if (AEE_SUCCESS != (nErr = qaic_testcase_close(handleSum))) {
        printf("ERROR 0x%x: Failed to close handle\n", nErr);
      }
    }
  } while(retry);

  if (nErr) {
    printf("ERROR 0x%x: Failed to run m1 on domain %d\n", nErr, domain_id);
    goto bail;
  }

  printf("\nCall m2 on the DSP\n");
  if (AEE_SUCCESS == (qaic_testcase_m2(handleSum, &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9, &var, &var1, &var2, &var3, &var4, &var5, &var6, &var7, &var8, &var9, &num1, &num2, &num3, &num4, &num5, &num6, &num7, &num8, &num9, &enum1))) {
      if(arg1!=L'B' || (char)arg2!= 'b'||arg3!= 'e' || arg4 != -2 || arg5!= 2 || arg6!= -9 || arg7!= 9 || arg8!= -99 ||arg9 != 99 || var != 3 || var1!=4 || var2 != 5 || var3!=6 || var4 != 7 ||var5!= 8 || var6!= 9 ||var7!= 10 || var8!= 11 || var9!= 12 || num1 != 13 ||num2 !=14 || num3!= 15 || num4!= 16 ||num5!= 17 ||num6!= 18 || num7!= 19 || num8!= 20 || !(num9) || enum1)
      printf("=== Data Mismatch ===\n");
    else
      printf("Completion of m2 on the DSP\n");
  }
  if (nErr) {
    printf("ERROR 0x%x: Failed to run m2 on domain %d\n", nErr, domain_id);
  }

  printf("\nCall m3 on the DSP\n");
  if (AEE_SUCCESS == (qaic_testcase_m3(handleSum, &arg1, &arg2, &arg3, &arg4, &arg5, &arg6, &arg7, &arg8, &arg9, &var, &var1, &var2, &var3, &var4, &var5, &var6, &var7, &var8, &var9, &num1, &num2, &num3, &num4, &num5, &num6, &num7, &num8, &num9, &enum1))) {
      if(arg1!=L'C' || (char)arg2!= 'c'||arg3!= 'f' || arg4 != -1 || arg5!= 3 || arg6!= -8 || arg7!= 10 || arg8!= -98 ||arg9 != 100 || var != 4 || var1!=5 || var2 != 6 || var3!= 7 || var4 != 8 ||var5!= 9 || var6!= 10 ||var7!= 11 || var8!= 12 || var9!= 13 || num1 != 14 ||num2 !=15 || num3!= 16 || num4!= 17 ||num5!= 18 ||num6!= 19 || num7!= 20 || num8!= 21 || num9 || !(enum1))
      printf("=== Data Mismatch ===\n");
    else
      printf("Completion of m3 on the DSP\n");
  }
  if (nErr) {
    printf("ERROR 0x%x: Failed to run m3 on domain %d\n", nErr, domain_id);
  }

  printf("\nCall m4 on the DSP\n");
  if (AEE_SUCCESS == (qaic_testcase_m4(handleSum, struc1, struc2))) {
    if(struc2->enum1 != 1 || struc2->enum2[0] != GREEN || struc2->enum2[1] != YELLOW || struc2->enum2[2] != RED)
      printf("=== Data Mismatch ===\n");
    else
      printf("Completion of m4 on the DSP\n");
  }
  if (nErr) {
    printf("ERROR 0x%x: Failed to run m4 on domain %d\n", nErr, domain_id);
  }

  printf("\nCall m5 on the DSP\n");
  if (AEE_SUCCESS == (qaic_testcase_m5(handleSum, seq1, seqLen, seq2, seqLen))) {
    int f=0;
    for(int i=0;i<seqLen;i++){
      if(seq2[i].enum1 != i){
        f++;
        break;
      }
      for(int j=0;j<seq2[i].enum2Len;j++){
          if(seq2[i].enum2[j] != j){
            f++;
            break;
        }
      }
    }
    if(f)
      printf("=== Data Mismatch ===\n");
    else
      printf("Completion of m5 on the DSP\n");
  }
  if (nErr) {
    printf("ERROR 0x%x: Failed to run m5 on domain %d\n", nErr, domain_id);
  }
  if (handleSum != -1) {
    if (AEE_SUCCESS != (nErr = qaic_testcase_close(handleSum))) {
      printf("ERROR 0x%x: Failed to close handle\n", nErr);
    }
  }
  bail:
    if (uri) free(uri);
    if (struc1 && struc1->enum2) 
      rpcmem_free(struc1->enum2);
    if(struc1)
      rpcmem_free(struc1);
    if (struc2 && struc2->enum2) 
      rpcmem_free(struc2->enum2);
    if(struc2)
      rpcmem_free(struc2);
    for(int i=0;i<seqLen;i++){
      if(seq1[i].enum2)
        rpcmem_free(seq1[i].enum2);
    }
    if(seq1)
      rpcmem_free(seq1);
    for(int i=0;i<seqLen;i++){
      if(seq2[i].enum2)
        rpcmem_free(seq2[i].enum2);
    }
    if(seq2)
      rpcmem_free(seq2);
    return nErr;
  }
