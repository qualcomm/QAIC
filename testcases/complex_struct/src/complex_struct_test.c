//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "AEEStdErr.h"
#include "complex_struct.h"
#include "complex_struct_test.h"
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

int local_complex_struct_sum(beta* vec, int64_t* res)
{
  *res =0;
  for(int i=0;i<vec->struct_ptrLen;i++)
  {
     *res+=((vec->struct_ptr)[i].length+(vec->struct_ptr)[i].width+(vec->struct_ptr)[i].height);
  }
  if(*res!=810)
      return -1;
  return 0;
}

int complex_struct_test(int domain, int num, bool is_signedpd_requested) {
  int nErr = AEE_SUCCESS;
  beta* test = NULL;
  beta *vec1 = NULL, *vec2 = NULL;
  beta1 *s1 = NULL, *s2 = NULL;
  alpha2 *wstruc1 = NULL, *wstruc2 = NULL;
  beta2 *wstruc3 = NULL, *wstruc4 = NULL;
  int  len = 0;
  int retry = 10;
  remote_handle64 handleSum = -1;
  int64_t result=0;
  char *uri = NULL;
  num = 5;
  alpha1 *struc1 = NULL, *struc2 = NULL;

  len = sizeof(*test) * 1;

  int heapid = RPCMEM_HEAP_ID_SYSTEM;
#if defined(SLPI) || defined(MDSP)
  heapid = RPCMEM_HEAP_ID_CONTIG;
#endif

  if (0 == (test = (beta*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, len))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  if (0 == (vec1 = (beta*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, len))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  vec1->struct_ptrLen = 3;
  if (0 == (vec1->struct_ptr = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)*(vec1->struct_ptrLen)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  if (0 == (vec2 = (beta*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, len))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  vec2->struct_ptrLen = 3;
  if (0 == (vec2->struct_ptr = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)*(vec2->struct_ptrLen)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }

  printf("---Creating sequence of alpha from 0 to %d\n", num - 1);


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

  test->struct_ptr = a_;
  test->struct_ptrLen = 5;

  vec1->struct_ptr[0].length = 2;
  vec1->struct_ptr[0].width = 4;
  vec1->struct_ptr[0].height = 6;

  vec1->struct_ptr[1].length = 3;
  vec1->struct_ptr[1].width = 6;
  vec1->struct_ptr[1].height = 9;

  vec1->struct_ptr[2].length = 4;
  vec1->struct_ptr[2].width = 8;
  vec1->struct_ptr[2].height = 12;

  if (0 == (struc1 = (alpha1*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha1)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  if (0 == (struc2 = (alpha1*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha1)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  struc1->strLen = 6;
  if (0 == (struc1->str = (char*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(char)*(struc1->strLen)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  struc2->strLen = 6;
  if (0 == (struc2->str = (char*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(char)*(struc2->strLen)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  for(int i=0;i<(struc1->strLen)-1; i++){
    struc1->str[i] = (char)(65+i);
  }
  struc1->str[(struc1->strLen)-1] = '\0';

  if (0 == (s1 = (beta1*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(beta1)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  if (0 == (s2 = (beta1*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(beta1)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  s1->a.strLen = 5;
  if (0 == (s1->a.str = (char*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(char)*(s1->a.strLen)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  for(int i=0;i<(s1->a.strLen)-1;i++)
    s1->a.str[i] = (char) (97+i);
  s1->a.str[(s1->a.strLen)-1] = '\0';
  s2->a.strLen = 3;
  if (0 == (s2->a.str = (char*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(char)*(s2->a.strLen)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
    if (0 == (wstruc1 = (alpha2*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha2)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  if (0 == (wstruc2 = (alpha2*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha2)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  wstruc1->wstrLen = 6;
  if (0 == (wstruc1->wstr = (_wchar_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(_wchar_t)*(wstruc1->wstrLen)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  for(int i=0;i<(wstruc1->wstrLen)-1;i++)
    wstruc1->wstr[i] = (wchar_t) (100+i);
  wstruc1->wstr[(wstruc1->wstrLen)-1] = L'\0';
  wstruc2->wstrLen = 6;
  if (0 == (wstruc2->wstr = (_wchar_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(_wchar_t)*(wstruc2->wstrLen)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  if (0 == (wstruc3 = (beta2*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha2)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  if (0 == (wstruc4 = (beta2*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha2)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  wstruc3->a.wstrLen = 6;
  if (0 == (wstruc3->a.wstr = (_wchar_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(_wchar_t)*(wstruc3->a.wstrLen)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }
  for(int i=0;i<(wstruc3->a.wstrLen)-1;i++)
    wstruc3->a.wstr[i] = (wchar_t) (100+i);
  wstruc3->a.wstr[(wstruc3->a.wstrLen)-1] = L'\0';
  wstruc4->a.wstrLen = 6;
  if (0 == (wstruc4->a.wstr = (_wchar_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(_wchar_t)*(wstruc4->a.wstrLen)))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }

    printf("- compute sum on domain %d\n", domain);

    if (domain == ADSP_DOMAIN_ID)
      uri = complex_struct_URI ADSP_DOMAIN;
    else if (domain == CDSP_DOMAIN_ID)
      uri = complex_struct_URI CDSP_DOMAIN;
    else if (domain == MDSP_DOMAIN_ID)
      uri = complex_struct_URI MDSP_DOMAIN;
    else if (domain == SDSP_DOMAIN_ID)
      uri = complex_struct_URI SDSP_DOMAIN;
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
      if (AEE_SUCCESS == (nErr = complex_struct_open(uri, &handleSum))) {
        printf("\n- call complex_struct_sum on the DSP\n");
        nErr = complex_struct_sum(handleSum, test, &result);
      }

      if (!nErr) {
        printf("- Completion of complex_struct_sum on DSP\n");
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
        if (AEE_SUCCESS != (nErr = complex_struct_close(handleSum))) {
          printf("ERROR 0x%x: Failed to close handle\n", nErr);
        }
      }
    } while(retry);

    if (nErr) {
      printf("- retry attempt unsuccessful. Timing out....\n");
      printf("ERROR 0x%x: Failed to compute sum on domain %d\n", nErr, domain);
    }

    printf("\nCall sum2 method on the DSP\n");
    if (AEE_SUCCESS == (nErr = complex_struct_sum2(handleSum, vec1, vec2))) {
        int res[vec2->struct_ptrLen];
      for(int i=0;i<vec2->struct_ptrLen;i++)
        res[i] = vec2->struct_ptr[i].length + vec2->struct_ptr[i].width + vec2->struct_ptr[i].height;
      if(res[0] != 24 || res[1] != 36 || res[2] != 48){
        printf(" === Data Mismatch === \n");
      }
      else
        printf("Completion of sum2 method on DSP \n");
    }
    if (nErr)
      printf("ERROR 0x%x: Failed to run string method1 on domain %d\n", nErr, domain);

    printf("\nCall  string method1 on the DSP\n");
    if (AEE_SUCCESS == (nErr = complex_struct_sm1(handleSum, struc1, struc2))) {
      if(strncmp(struc2->str, "ABCDE", 5)==0)
        printf("Completion of string method1 on the DSP\n");
      else
        printf("=== Data Mismatch ===\n");
    }
    if (nErr)
      printf("ERROR 0x%x: Failed to run string method1 on domain %d\n", nErr, domain);

    printf("\nCall  string method2 on the DSP\n");
    if (AEE_SUCCESS == (nErr = complex_struct_sm2(handleSum, s1, s2))) {
      if(strncmp(s2->a.str, "ab", 2)==0)
        printf("Completion of string method2 on the DSP\n");
      else
        printf("=== Data Mismatch ===\n");
    }
    if (nErr)
      printf("ERROR 0x%x: Failed to run string method2 on domain %d\n", nErr, domain);

    printf("\nCall  wstring method1 on the DSP\n");
    if (AEE_SUCCESS == (nErr = complex_struct_wsm1(handleSum, wstruc1, wstruc2))) {
      int k=0;
      for(int i=0;i<(wstruc2->wstrLen)-1;i++){
        if(wstruc2->wstr[i] != (wchar_t) (65+i)){
          printf("=== Data Mismatch ===\n");
          k++;
          break;
        }
      }
      if(!k)
        printf("Completion of wstring method1 on the DSP\n");
    }
    if (nErr)
      printf("ERROR 0x%x: Failed to run string method1 on domain %d\n", nErr, domain);

    printf("\nCall  wstring method2 on the DSP\n");
    if (AEE_SUCCESS == (nErr = complex_struct_wsm2(handleSum, wstruc3, wstruc4))) {
      int k=0;
      for(int i=0;i<(wstruc4->a.wstrLen)-1;i++){
        if(wstruc4->a.wstr[i] != (wchar_t) (65+i)){
          printf("=== Data Mismatch ===\n");
        k++;
          break;
        }
      }
      if(!k)
        printf("Completion of wstring method2 on the DSP\n");
    }
    if (nErr)
      printf("ERROR 0x%x: Failed to run string method1 on domain %d\n", nErr, domain);

    if (handleSum != -1) {
      if (AEE_SUCCESS != (nErr = complex_struct_close(handleSum))) {
        printf("ERROR 0x%x: Failed to close handleSum\n", nErr);
      }
    }

bail:
  if (test) {
    rpcmem_free(test);
  }
  if(vec1->struct_ptr){
    rpcmem_free(vec1->struct_ptr);
  }
  if(vec1){
    rpcmem_free(vec1);
  }
  if(vec2->struct_ptr){
    rpcmem_free(vec2->struct_ptr);
  }
  if(vec2){
    rpcmem_free(vec2);
  }
  if(s1->a.str){
    rpcmem_free(s1->a.str);
  }
  if(s1){
    rpcmem_free(s1);
  }
  if(s2->a.str){
    rpcmem_free(s2->a.str);
  }
  if(s2){
    rpcmem_free(s2);
  }
  if(struc1->str){
    rpcmem_free(struc1->str);
  }
  if(struc1){
    rpcmem_free(struc1);
  }
  if(struc2->str){
    rpcmem_free(struc2->str);
  }
  if(struc2){
    rpcmem_free(struc2);
  }
  if(wstruc1->wstr){
    rpcmem_free(wstruc1->wstr);
  }
  if(wstruc1){
  rpcmem_free(wstruc1);
  }
  if(wstruc2->wstr){
    rpcmem_free(wstruc2->wstr);
  }
  if(wstruc3->a.wstr){
    rpcmem_free(wstruc3->a.wstr);
  }
  if(wstruc3){
    rpcmem_free(wstruc3);
  }
  if(wstruc4->a.wstr){
    rpcmem_free(wstruc4->a.wstr);
  }
  if(wstruc4){
    rpcmem_free(wstruc4);
  }
  return nErr;
}
