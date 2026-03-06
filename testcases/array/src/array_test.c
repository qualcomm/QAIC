//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "AEEStdErr.h"
#include "array.h"
#include "array_test.h"
#include "rpcmem.h"
#include "unistd.h"
#include "util.h"

int array_test(int domain, int num) {

  int nErr = AEE_SUCCESS;
  alpha* test = NULL;
  alpha *vec1 = NULL;
  int64_t* res = NULL;
  int  len = 0, resLen = 29;
  int retry = 10, seqLen = 2, sum1 = 0, sum2 = 0;
  remote_handle64 handleSum = -1;
  char *uri = NULL;
  num = 15;
  beta *struc1 = NULL, *struc2 = NULL, *seq3 = NULL, *seq4 = NULL;
  alpha *seq1 = NULL, *seq2 = NULL;
  len = sizeof(*test) * 1;

  int heapid = RPCMEM_HEAP_ID_SYSTEM;
#if defined(SLPI) || defined(MDSP)
  heapid = RPCMEM_HEAP_ID_CONTIG;
#endif

  if (0 == (test = (alpha *)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, len))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  if (0 == (res = (int64_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(int64_t)*resLen))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  //Allocating memory to sum2 rout param
  if (0 == (vec1 = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  //Allocating memory for method1 params
  if (0 == (struc1 = (beta*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(beta)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  if (0 == (struc2 = (beta*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(beta)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  // Allocating memory for method2 params
  if (0 == (seq1 = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)*seqLen))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  if (0 == (seq2 = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)*seqLen))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  //Allocating memory for method3 params
  if (0 == (seq3 = (beta*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(beta)*seqLen))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  if (0 == (seq4 = (beta*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(beta)*seqLen))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  printf("- Initializing in params \n");
  //Initializing sum method in params
  for(int i=0;i<10;i++)
  {
    (test->num2)[i] =(wchar_t) i;
    (test->num3)[i] =(unsigned char) i;
    (test->num4)[i] = (char) i;
    (test->number)[i] = (test->num5)[i] = (test->num6)[i] = (test->num7)[i]= (test->num8)[i] = (test->num9)[i] = (test->arr)[i] = (test->arr1)[i] = (test->arr2)[i] = (test->arr3)[i] = (test->arr4)[i] = (test->arr5)[i] = (test->arr6)[i] = (test->arr7)[i] = (test->arr8)[i] = (test->arr9)[i] = (test->var)[i] = (test->var1)[i] = (test->var2)[i] = (test->var3)[i] = (test->var4)[i] = (test->var5)[i] = (test->var6)[i] = (test->var7)[i] = i;
    (test->var8)[i] = i%2;
  }
  test->enum1[0] = test->enum1[3] = test->enum1[6] = test->enum1[9] = DARK;
  test->enum1[1] = test->enum1[4] = test->enum1[7] = GREY;
  test->enum1[2] = test->enum1[5] = test->enum1[8] = WHITE;

  //Initializing method1 in params
  for(int i=0;i<5;i++){
    struc1->a.num[i] = i;
  }
  //Initializing method2 in params
  for(int j=0;j<seqLen;j++){
    for(int i=0;i<10;i++){
    (seq1[j].num2)[i] =(wchar_t) (i+j);
    (seq1[j].num3)[i] =(unsigned char) (i+j);
    (seq1[j].num4)[i] = (char) (i+j);
    (seq1[j].number)[i] = (seq1[j].num5)[i] = (seq1[j].num6)[i] = (seq1[j].num7)[i]= (seq1[j].num8)[i] = (seq1[j].num9)[i] = (seq1[j].arr)[i] = (seq1[j].arr1)[i] = (seq1[j].arr2)[i] = (seq1[j].arr3)[i] = (seq1[j].arr4)[i] = (seq1[j].arr5)[i] = (seq1[j].arr6)[i] = (seq1[j].arr7)[i] = (seq1[j].arr8)[i] = (seq1[j].arr9)[i] = (seq1[j].var)[i] = (seq1[j].var1)[i] = (seq1[j].var2)[i] = (seq1[j].var3)[i] = (seq1[j].var4)[i] = (seq1[j].var5)[i] = (seq1[j].var6)[i] = (seq1[j].var7)[i] = (i+j);
    (seq1[j].var8)[i] = (i+j)%2;
    seq1[j].enum1[i] = WHITE;
    }
  }
  //Initializing method3 in params
  for(int i=0;i<seqLen;i++){
    for(int j=0;j<5;j++){
      seq3[i].a.num[j] = i+j;
    }
  }

    printf("- compute sum on domain %d\n", domain);
      nErr = get_uri(domain, array_URI, strlen(array_URI), &uri);
      if (nErr) {
      printf("ERROR 0x%x: get_uri failed\n", nErr);
        goto bail;
      }

    do {
      if (AEE_SUCCESS == (nErr = array_open(uri, &handleSum))) {
        printf("\nCall array_sum on the DSP\n");
        nErr = array_sum(handleSum, test, res, resLen);
      }
    int k=0;
      if (!nErr) {
        for(int i=0;i<resLen-2;i++){
          if(res[i]!=45){
            k++;
            break;
          }
        }
        if(res[resLen-2] != 5 || res[resLen-1] != 9)
          k++;
        if(!k)
          printf("Completion of array_sum on the DSP\n");
        else
          printf("=== Data Mismatch ===\n");
        break;
      } else {
        if (nErr == AEE_ECONNRESET) {
          /* AEE_ECONNRESET is returned when Sub-system restart (SSR) happens */
          retry--;
          //sleep(5);
        } else if (nErr == AEE_ENOSUCH || nErr == AEE_EBADSTATE) {
          /* AEE_ENOSUCH is returned when Protection domain restart (PDR) happens and
          AEE_EBADSTATE is returned when PD is exiting or crashing.*/
          retry -= 2;
        } else {
          break;
        }
      }

      /* Close the handle and retry handle open */
      if (handleSum != -1) {
        if (AEE_SUCCESS != (nErr = array_close(handleSum))) {
          printf("ERROR 0x%x: Failed to close handle\n", nErr);
        }
      }
    } while(retry);

    if (nErr) {
      printf("- retry attempt unsuccessful. Timing out....\n");
      printf("ERROR 0x%x: Failed to compute sum on domain %d\n", nErr, domain);
      goto bail;
    }

    printf("\nCall array_sum2 on the DSP\n");
    if (AEE_SUCCESS == (nErr = array_sum2(handleSum, vec1, test))) {
      sum1 = 0, sum2 = 0;
      for(int i=0;i<10;i++){
        sum1 = sum1 + (vec1->number)[i] + (int)(vec1->num2)[i] + (int)(vec1->num3)[i] + (int)(vec1->num4)[i] + (vec1->num5)[i] + (vec1->num6)[i] + (vec1->num7)[i] + (vec1->num8)[i] + (vec1->num9)[i] + (vec1->arr)[i] + (vec1->arr1)[i] + (vec1->arr2)[i] + (vec1->arr3)[i] + (vec1->arr4)[i] + (vec1->arr5)[i] + (vec1->arr6)[i] + (vec1->arr7)[i] + (vec1->arr8)[i] + (vec1->arr9)[i] + (vec1->var)[i] + (vec1->var1)[i] + (vec1->var2)[i] + (vec1->var3)[i] + (vec1->var4)[i] + (vec1->var5)[i] + (vec1->var6)[i] + (vec1->var7)[i] + (vec1->var8)[i] + (vec1->enum1)[i];

        sum2 = sum2 + (test->number)[i] + (int)(test->num2)[i] + (int)(test->num3)[i] + (int)(test->num4)[i] + (test->num5)[i] + (test->num6)[i] + (test->num7)[i] + (test->num8)[i] + (test->num9)[i] + (test->arr)[i] + (test->arr1)[i] + (test->arr2)[i] + (test->arr3)[i] + (test->arr4)[i] + (test->arr5)[i] + (test->arr6)[i] + (test->arr7)[i] + (test->arr8)[i] + (test->arr9)[i] + (test->var)[i] + (test->var1)[i] + (test->var2)[i] + (test->var3)[i] + (test->var4)[i] + (test->var5)[i] + (test->var6)[i] + (test->var7)[i] + (test->var8)[i] + (test->enum1)[i];
      }
      if(sum1 != 463 || sum2 != 502)
        printf("=== Data Mismatch ===\n");
      else
        printf("Completion of array_sum2 on DSP\n");
    }
    if (nErr) {
      printf("ERROR 0x%x: Failed to run array_method1 on domain %d\n", nErr, domain);
    }
    printf("\nCall array_method1 on the DSP\n");
    if (AEE_SUCCESS == (nErr = array_method1(handleSum, struc1, struc2, struc1))) {
      int sum1 = 0;
      for(int j=0;j<5;j++)
          sum1 = sum1 + struc1->a.num[j];
      if(struc2->a.num[0] != 1 || struc2->a.num[1] != 1 || struc2->a.num[2] != 2 || struc2->a.num[3] != 6 || struc2->a.num[4] != 24 || sum1 != 30)
        printf("=== Data Mismatch ===\n");
      else
        printf("Completion of array_method1 on DSP\n");
    }
    if (nErr) {
      printf("ERROR 0x%x: Failed to run array_method1 on domain %d\n", nErr, domain);
    }
    printf("\nCall array_method2 on the DSP\n");
    if (AEE_SUCCESS == (nErr = array_method2(handleSum, seq1, seqLen, seq2, seqLen, seq1, seqLen))) {
      sum1 = 0, sum2 =0;
      for(int i=0;i<seqLen;i++){
        for(int j=0;j<10;j++){
          sum1 = sum1 +seq1[i].number[j] + (int)seq1[i].num2[j] + (int)seq1[i].num3[j] + (int)seq1[i].num4[j] + seq1[i].num5[j] + seq1[i].num6[j] + seq1[i].num7[j] + seq1[i].num8[j] + seq1[i].num9[j] + seq1[i].arr[j] + seq1[i].arr1[j] + seq1[i].arr2[j] + seq1[i].arr3[j] + seq1[i].arr4[j]+ seq1[i].arr5[j]+ seq1[i].arr6[j]+ seq1[i].arr7[j]+ seq1[i].arr8[j]+ seq1[i].arr9[j] + seq1[i].var[j]+ seq1[i].var1[j]+ seq1[i].var2[j]+ seq1[i].var3[j]+ seq1[i].var4[j]+ seq1[i].var5[j]+ seq1[i].var6[j]+ seq1[i].var7[j]+ seq1[i].var8[j] + seq1[i].enum1[j];
          sum2 = sum2 +seq2[i].number[j] + (int)seq2[i].num2[j] + (int)seq2[i].num3[j] + (int)seq2[i].num4[j] + seq2[i].num5[j] + seq2[i].num6[j] + seq2[i].num7[j] + seq2[i].num8[j] + seq2[i].num9[j] + seq2[i].arr[j] + seq2[i].arr1[j] + seq2[i].arr2[j] + seq2[i].arr3[j] + seq2[i].arr4[j]+ seq2[i].arr5[j]+ seq2[i].arr6[j]+ seq2[i].arr7[j]+ seq2[i].arr8[j]+ seq2[i].arr9[j] + seq2[i].var[j]+ seq2[i].var1[j]+ seq2[i].var2[j]+ seq2[i].var3[j]+ seq2[i].var4[j]+ seq2[i].var5[j]+ seq2[i].var6[j]+ seq2[i].var7[j]+ seq2[i].var8[j] + seq2[i].enum1[j];
        }
      }
      if(sum2 != 954 || sum1!= 934)
        printf("=== Data Mismatch %d, %d===\n",sum1,sum2);
      else
        printf("Completion of array_method2 on DSP\n");
    }
    if (nErr) {
      printf("ERROR 0x%x: Failed to run array_method2 on domain %d\n", nErr, domain);
    }
    printf("\nCall array_method3 on the DSP\n");
    if (AEE_SUCCESS == (nErr = array_method3(handleSum, seq3, seqLen, seq4, seqLen, seq3, seqLen))) {
      sum1 = 0, sum2 = 0;
      for(int i=0;i<seqLen;i++){
        for(int j=0;j<5;j++){
          sum1 = sum1 + seq3[i].a.num[j];
          sum2 = sum2 + seq4[i].a.num[j];
        }
      }
      if(sum1 != 187|| sum2 != 85)
        printf("=== Data Mismatch %d, %d===\n", sum1, sum2);
      else
        printf("Completion of array_method3 on DSP\n");
    }
    if (nErr) {
      printf("ERROR 0x%x: Failed to run array_method3 on domain %d\n", nErr, domain);
    }
    if (handleSum != -1) {
      if (AEE_SUCCESS != (nErr = array_close(handleSum))) {
        printf("ERROR 0x%x: Failed to close handleSum\n", nErr);
      }
    }

bail:
  if (test) {
    rpcmem_free(test);
  }
  if (res) {
    rpcmem_free(res);
  }
  if (struc1) {
    rpcmem_free(struc1);
  }
  if (struc2) {
    rpcmem_free(struc1);
  }
  if (seq1) {
    rpcmem_free(seq1);
  }
  if (seq2) {
    rpcmem_free(seq2);
  }
  if (seq3) {
    rpcmem_free(seq3);
  }
  if (seq4) {
    rpcmem_free(seq4);
  }
  if (vec1) {
    rpcmem_free(vec1);
  }
  return nErr;
}
