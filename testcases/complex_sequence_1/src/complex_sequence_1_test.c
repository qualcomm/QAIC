//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "AEEStdErr.h"
#include "complex_sequence_1.h"
#include "complex_sequence_1_test.h"
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

int local_complex_sequence_1_sum(beta* vec, int vecLen)
{
  printf("reached local execution %d\n",vecLen);
  int sum=0;
  for(int j=0;j<vecLen;j++)
  {
      for(int i=0;i<vec[j].struct_ptrLen;i++)
      {
          sum+=((vec[j].struct_ptr)[i].length+(vec[j].struct_ptr)[i].width+(vec[j].struct_ptr)[i].height);
      }
  }
  if(sum!=4050)
      return -1;
  return 0;
}

int complex_sequence_1_test(int domain, int num, bool is_signedpd_requested) {
  int nErr = AEE_SUCCESS;
  beta* test = NULL;
  beta *vec = NULL, *vec1 = NULL;
  alpha1 *seq_str = NULL, *seq_str1 = NULL;
  beta1 *cseq = NULL, *cseq1 = NULL;
  alpha2 *wseq = NULL, *wseq1 = NULL;
  beta2 *cwseq = NULL, *cwseq1 = NULL;
  int  len = 0, len1 = 2 ;
  int retry = 10;
  remote_handle64 handleSum = -1;
  char *uri = NULL;
  num = 5;

  len = sizeof(*test) * num;
  printf("\n- allocate %d bytes from ION heap\n", len);

  int heapid = RPCMEM_HEAP_ID_SYSTEM;
  #if defined(SLPI) || defined(MDSP)
    heapid = RPCMEM_HEAP_ID_CONTIG;
  #endif

  if (0 == (test = (beta *)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, len))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }

  printf("- creating sequence of numbers from 0 to %d\n", num - 1);

  for(int i=0;i<num;i++){
    alpha* a_;
    int len1 = sizeof(*a_) * num;

    if( 0 == (a_ = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, len1))){
        printf("---Error: alloc failed\n");
        nErr = -1;
        goto bail;
    }

    a_[0].length = 10;
    a_[0].width = 9;
    a_[0].height = 123;

    a_[1].length = 20;
    a_[1].width = 8;
    a_[1].height = 124;

    a_[2].length = 30;
    a_[2].width = 7;
    a_[2].height = 125 ;

    a_[3].length = 40;
    a_[3].width = 6;
    a_[3].height = 126;

    a_[4].length = 50;
    a_[4].width = 5;
    a_[4].height = 127;

    test[i].struct_ptr = a_;
    test[i].struct_ptrLen = 5;
  }
  //Allocating memory and initializing the parameters for sum2 method
  if (0 == (vec = (beta*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(beta)*len1))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  if (0 == (vec1 = (beta*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(beta)*len1))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  for(int i=0;i<len1;i++){
    vec[i].struct_ptrLen = 5;
    if (0 == (vec[i].struct_ptr = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)*(vec[i].struct_ptrLen)))) {
      nErr = AEE_ENORPCMEMORY;
      printf("ERROR 0x%x: memory alloc failed\n", nErr);
      goto bail;
    }
    for(int j =0;j<vec[i].struct_ptrLen;j++){
      vec[i].struct_ptr[j].length = i+j;
      vec[i].struct_ptr[j].width = 2*(i+j);
      vec[i].struct_ptr[j].height = 3*(i+j);
    }
    vec1[i].struct_ptrLen = 5;
    if (0 == (vec1[i].struct_ptr = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)*(vec1[i].struct_ptrLen)))) {
      nErr = AEE_ENORPCMEMORY;
      printf("ERROR 0x%x: memory alloc failed\n", nErr);
      goto bail;
    }
  }
  //Allocating memory and initializing the parameters for sm1 method
  if (0 == (seq_str = (alpha1*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha1)*len1))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  if (0 == (seq_str1 = (alpha1*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha1)*len1))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  for(int i=0;i<len1;i++){
    seq_str[i].strLen = 5;
    if (0 == (seq_str[i].str = (char*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(char)*(seq_str[i].strLen)))) {
      nErr = AEE_ENORPCMEMORY;
      printf("ERROR 0x%x: memory alloc failed\n", nErr);
      goto bail;
    }
    for(int j=0;j<(seq_str[i].strLen)-1;j++)
      seq_str[i].str[j] = (char)(65+i+j);
    seq_str[i].str[(seq_str[i].strLen)-1] = '\0';
    seq_str1[i].strLen = 5;
    if (0 == (seq_str1[i].str = (char*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(char)*(seq_str1[i].strLen)))) {
      nErr = AEE_ENORPCMEMORY;
      printf("ERROR 0x%x: memory alloc failed\n", nErr);
      goto bail;
    }
  }
  //Allocating memory and initializing the parameters for sm2 method
  if (0 == (cseq = (beta1*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(beta1)*len1))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  if (0 == (cseq1 = (beta1*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(beta1)*len1))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  for(int i=0;i<len1;i++){
    cseq[i].a.strLen = 5;
    if (0 == (cseq[i].a.str = (char*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(char)*(cseq[i].a.strLen)))) {
      nErr = AEE_ENORPCMEMORY;
      printf("ERROR 0x%x: memory alloc failed\n", nErr);
      goto bail;
    }
    for(int j=0;j<(cseq[i].a.strLen)-1;j++){
      cseq[i].a.str[j] = (char)(97+i+j);
    }
    cseq[i].a.str[(cseq[i].a.strLen)-1] = '\0';
    cseq1[i].a.strLen = 5;
    if (0 == (cseq1[i].a.str = (char*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(char)*(cseq1[i].a.strLen)))) {
      nErr = AEE_ENORPCMEMORY;
      printf("ERROR 0x%x: memory alloc failed\n", nErr);
      goto bail;
    }
  }
  //Allocating memory and initializing the parameters for wsm1 method
  if (0 == (wseq = (alpha2*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha2)*len1))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  if (0 == (wseq1 = (alpha2*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha2)*len1))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  for(int i=0;i<len1;i++){
    wseq[i].wstrLen = 5;
    if (0 == (wseq[i].wstr = (_wchar_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(_wchar_t)*(wseq[i].wstrLen)))) {
      nErr = AEE_ENORPCMEMORY;
      printf("ERROR 0x%x: memory alloc failed\n", nErr);
      goto bail;
    }
    for(int j=0;j<(wseq[i].wstrLen)-1;j++)
      wseq[i].wstr[j] = (wchar_t) (100+i+j);
    wseq[i].wstr[(wseq[i].wstrLen)-1] = L'\0';
    wseq1[i].wstrLen = 5;
    if (0 == (wseq1[i].wstr = (_wchar_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(_wchar_t)*(wseq1[i].wstrLen)))) {
      nErr = AEE_ENORPCMEMORY;
      printf("ERROR 0x%x: memory alloc failed\n", nErr);
      goto bail;
    }
  }
  //Allocating memory and initializing the parameters for wsm2 method
  if (0 == (cwseq = (beta2*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(beta2)*len1))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  if (0 == (cwseq1 = (beta2*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(beta2)*len1))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  for(int i=0;i<len1;i++){
    cwseq[i].a.wstrLen = 5;
    if (0 == (cwseq[i].a.wstr = (_wchar_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(_wchar_t)*(cwseq[i].a.wstrLen)))) {
      nErr = AEE_ENORPCMEMORY;
      printf("ERROR 0x%x: memory alloc failed\n", nErr);
      goto bail;
    }
    for(int j=0;j<(cwseq[i].a.wstrLen)-1;j++){
      cwseq[i].a.wstr[j] = (wchar_t)(65+i+j);
    }
    cwseq[i].a.wstr[(cwseq[i].a.wstrLen)-1] = L'\0';
    cwseq1[i].a.wstrLen = 5;
    if (0 == (cwseq1[i].a.wstr = (_wchar_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(_wchar_t)*(cwseq1[i].a.wstrLen)))) {
      nErr = AEE_ENORPCMEMORY;
      printf("ERROR 0x%x: memory alloc failed\n", nErr);
      goto bail;
    }
  }

    printf("- compute sum on domain %d\n", domain);

    if (domain == ADSP_DOMAIN_ID)
      uri = complex_sequence_1_URI ADSP_DOMAIN;
    else if (domain == CDSP_DOMAIN_ID)
      uri = complex_sequence_1_URI CDSP_DOMAIN;
    else if (domain == MDSP_DOMAIN_ID)
      uri = complex_sequence_1_URI MDSP_DOMAIN;
    else if (domain == SDSP_DOMAIN_ID)
      uri = complex_sequence_1_URI SDSP_DOMAIN;
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
      if (AEE_SUCCESS == (nErr = complex_sequence_1_open(uri, &handleSum))) {
        printf("\n- call complex_sequence_1_sum on the DSP\n");
        nErr = complex_sequence_1_sum(handleSum, test, num);
      }

      if (!nErr) {
        printf("- complex_sequence_1 ran successfully!! \n");
        break;
      } else {
        if (nErr == AEE_ECONNRESET) {
          /* AEE_ECONNRESET is returned when Sub-system restart (SSR) happens */
          retry--;
          sleep(5);
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
        if (AEE_SUCCESS != (nErr = complex_sequence_1_close(handleSum))) {
          printf("ERROR 0x%x: Failed to close handle\n", nErr);
        }
      }
    } while(retry);

    if (nErr) {
      printf("- retry attempt unsuccessful. Timing out....\n");
      printf("ERROR 0x%x: Failed to compute sum on domain %d\n", nErr, domain);
    }

    //Calling implementation function of sum2 method
    printf("\nCall sum2 method on the DSP\n");
    if (AEE_SUCCESS == (nErr = complex_sequence_1_sum2(handleSum, vec, len1, vec1, len1))) {
      int sum =0;
      for(int i=0;i<len1;i++){
        for(int j=0;j<vec1[i].struct_ptrLen;j++){
          sum = sum +vec1[i].struct_ptr[j].length+ vec1[i].struct_ptr[j].width + vec1[i].struct_ptr[j].height;
        }
      }
      if(sum != 300)
        printf(" === Data Mismatch === \n");
      else
        printf("Completion of sum2 method on DSP \n");
    }
    if (nErr)
      printf("ERROR 0x%x: Failed to run sum2 method on domain %d\n", nErr, domain);

    //Calling implementation function of string method1
    printf("\nCall string method1 on the DSP\n");
    if (AEE_SUCCESS == (nErr = complex_sequence_1_sm1(handleSum, seq_str, len1, seq_str1, len1))) {
      int k =0;
      for(int i=0;i<len1;i++){
        for(int j=0;j<(seq_str1[i].strLen)-1;j++){
          if(seq_str1[i].str[j]!= (char)(65+i+j+1)){
            k++;
            break;
          }
        }
      }
      if(k)
        printf(" === Data Mismatch === \n");
      else
        printf("Completion of string method1 on DSP \n");
    }
    if (nErr)
      printf("ERROR 0x%x: Failed to run string method1 on domain %d\n", nErr, domain);

    //Calling implementation function of string method2
    printf("\nCall string method2 on the DSP\n");
    if (AEE_SUCCESS == (nErr = complex_sequence_1_sm2(handleSum, cseq, len1, cseq1, len1))) {
      int k=0;
      for(int i=0;i<len1;i++){
        if(strncmp(cseq[i].a.str, cseq1[i].a.str,5)!=0){
          k++;
          break;
        }
      }
      if(k)
        printf(" === Data Mismatch === \n");
      else
        printf("Completion of string method2 on DSP \n");
    }
    if (nErr)
      printf("ERROR 0x%x: Failed to run string method2 on domain %d\n", nErr, domain);

    //Calling implementation function of wstring method1
    printf("\nCall wstring method1 on the DSP\n");
    if (AEE_SUCCESS == (nErr = complex_sequence_1_wsm1(handleSum, wseq, len1, wseq1, len1))) {
      int k=0;
      for(int i=0;i<len1;i++){
        for(int j=0;j<(wseq1[i].wstrLen)-1;j++){
          if(wseq1[i].wstr[j] != (wchar_t) (65+i+j)){
            k++;
            break;
          }
        }
      }
      if(k)
        printf(" === Data Mismatch === \n");
      else
        printf("Completion of wstring method1 on DSP \n");
    }
    if (nErr)
      printf("ERROR 0x%x: Failed to run wstring method1 on domain %d\n", nErr, domain);

    //Calling implementation function of wstring method2
    printf("\nCall wstring method2 on the DSP\n");
    if (AEE_SUCCESS == (nErr = complex_sequence_1_wsm2(handleSum, cwseq, len1, cwseq1, len1))) {
      int k=0;
      for(int i=0;i<len1;i++){
        for(int j=0;j<(cwseq1[i].a.wstrLen)-1;j++){
          if(cwseq1[i].a.wstr[j] != (wchar_t) (97+i+j)){
            k++;
            break;
          }
        }
      }
      if(k)
        printf(" === Data Mismatch === \n");
      else
        printf("Completion of wstring method2 on DSP \n");
    }
    if (nErr)
      printf("ERROR 0x%x: Failed to run wstring method2 on domain %d\n", nErr, domain);

    if (handleSum != -1) {
      if (AEE_SUCCESS != (nErr = complex_sequence_1_close(handleSum))) {
        printf("ERROR 0x%x: Failed to close handleSum\n", nErr);
      }
    }

bail:
  if (test) {
    rpcmem_free(test);
  }
  for(int i=0;i<len1;i++){
    if (vec[i].struct_ptr) {
      rpcmem_free(vec[i].struct_ptr);
    }
    if (vec1[i].struct_ptr) {
      rpcmem_free(vec[i].struct_ptr);
    }
    if (seq_str[i].str) {
      rpcmem_free(seq_str[i].str);
    }
    if (seq_str1[i].str) {
      rpcmem_free(seq_str1[i].str);
    }
    if (cseq[i].a.str) {
      rpcmem_free(cseq[i].a.str);
    }
    if (cseq1[i].a.str) {
      rpcmem_free(cseq1[i].a.str);
    }
    if (wseq[i].wstr) {
      rpcmem_free(wseq[i].wstr);
    }
    if (wseq1[i].wstr) {
      rpcmem_free(wseq1[i].wstr);
    }
    if (cwseq[i].a.wstr) {
      rpcmem_free(cwseq[i].a.wstr);
    }
    if (cwseq1[i].a.wstr) {
      rpcmem_free(cwseq1[i].a.wstr);
    }
  }
  if (vec) {
    rpcmem_free(vec);
  }
  if (vec1) {
    rpcmem_free(vec1);
  }
  if (seq_str) {
    rpcmem_free(seq_str);
  }
  if (seq_str1) {
    rpcmem_free(seq_str1);
  }
  if (cseq) {
    rpcmem_free(cseq);
  }
  if (cseq1) {
    rpcmem_free(cseq1);
  }
  if (wseq) {
    rpcmem_free(wseq);
  }
  if (wseq1) {
    rpcmem_free(wseq1);
  }
  if (cwseq) {
    rpcmem_free(cwseq);
  }
  if (cwseq1) {
    rpcmem_free(cwseq1);
  }
  return nErr;
}
