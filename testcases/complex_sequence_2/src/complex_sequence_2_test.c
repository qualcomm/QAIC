/*==============================================================================
  Copyright (c) 2023 Qualcomm Technologies, Inc.
  All rights reserved. Qualcomm Proprietary and Confidential.
==============================================================================*/

#include "AEEStdErr.h"
#include "complex_sequence_2.h"
#include "complex_sequence_2_test.h"
#include "rpcmem.h"
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include "remote.h"
#include <unistd.h>
#include <string.h>
#include "util.h"

int complex_sequence_2_test( int domain_id, bool is_signedpd_requested) {

  int heapid = RPCMEM_HEAP_ID_SYSTEM;
  if (domain_id == 3) {
    if (getenv("FASTRPC_DMA_HEAP_NAME") == NULL) setenv("FASTRPC_DMA_HEAP_NAME", "qcom,system", 0);
    setenv("FASTRPC_DISABLE_ION", "1", 1);
  }

  int retry = 10;
  remote_handle64 handle = -1;
  char *uri = NULL;

  beta *b = NULL, *b1 = NULL;
  beta1 *B = NULL, *B1 = NULL;
  int nErr = AEE_SUCCESS;
  int seqLen =2, a=0;
  // Allocating memory to in sequence<beta> and members of beta struct
  if (0 == (b = (beta*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(beta)*seqLen))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }

  for(int i=0;i<seqLen;i++){
    b[i].alpLen = 2;
    if (0 == (b[i].alp = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)*(b[i].alpLen)))) {
      nErr = AEE_ENORPCMEMORY;
      printf("ERROR 0x%x: memory alloc failed\n", nErr);
      goto bail;
    }
    for(int k=0;k<2;k++){
      b[i].alp[k].lLen = 2;
      if (0 == (b[i].alp[k].l = (int*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(int)*(b[i].alp[k].lLen)))) {
        nErr = AEE_ENORPCMEMORY;
        printf("ERROR 0x%x: memory alloc failed\n", nErr);
        goto bail;
      }
    }
  }
  //Initializing in sequence<beta>
  for(int i=0;i<seqLen;i++){
    b[i].z = i +1;
    for(int j=0;j<b[i].alpLen;j++){
      b[i].alp[j].x = 2*(j+1);
      for(int k=0;k<b[i].alp[j].lLen;k++){
        b[i].alp[j].l[k] = i+j+k;
      }
    }
  }
  //Allocating memory to rout sequence<beta> and members of beta struct
  if (0 == (b1 = (beta*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(beta)*seqLen))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }

  for(int i=0;i<seqLen;i++){
    b1[i].alpLen = 2;
    if (0 == (b1[i].alp = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)*(b1[i].alpLen)))) {
      nErr = AEE_ENORPCMEMORY;
      printf("ERROR 0x%x: memory alloc failed\n", nErr);
      goto bail;
    }
    for(int k=0;k<2;k++){
      b1[i].alp[k].lLen = 2;
      if (0 == (b1[i].alp[k].l = (int*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(int)*(b1[i].alp[k].lLen)))) {
        nErr = AEE_ENORPCMEMORY;
        printf("ERROR 0x%x: memory alloc failed\n", nErr);
        goto bail;
      }
    }
  }
  //Allocating memory to in sequence<beta1> and members of beta1 struct
  if (0 == (B = (beta1*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(beta1)*seqLen))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }

  for(int i=0; i<seqLen;i++){
    B[i].ssLen = 2;
    if (0 == (B[i].ss = (alpha1*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha1)*(B[i].ssLen)))) {
      nErr = AEE_ENORPCMEMORY;
      printf("ERROR 0x%x: memory alloc failed\n", nErr);
      goto bail;
    }
    for(int j=0;j<B[i].ssLen;j++){
      B[i].ss[j].sLen = 2;
      if (0 == (B[i].ss[j].s = (_cstring1_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(_cstring1_t)*(B[i].ss[j].sLen)))) {
        nErr = AEE_ENORPCMEMORY;
        printf("ERROR 0x%x: memory alloc failed\n", nErr);
        goto bail;
      }
      for(int k=0;k<B[i].ss[j].sLen;k++){
        B[i].ss[j].s[k].dataLen = 5;
        if (0 == (B[i].ss[j].s[k].data = (char*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(char)*(B[i].ss[j].s[k].dataLen)))) {
          nErr = AEE_ENORPCMEMORY;
          printf("ERROR 0x%x: memory alloc failed\n", nErr);
          goto bail;
        }
      }
    }
  }
  //Initializing in sequence<beta1>
  for(int i=0;i<seqLen;i++){
    for(int j=0;j< B[i].ssLen;j++){
      B[i].ss[j].y = i+j;
      for(int k=0;k< B[i].ss[j].sLen;k++){
        for(int a=0; a<(B[i].ss[j].s[k].dataLen)-1; a++){
          B[i].ss[j].s[k].data[a] = (char)(65+i+j+k+a);
        }
        B[i].ss[j].s[k].data[(B[i].ss[j].s[k].dataLen)-1] = '\0';
      }
    }
  }
  //Allocating memory to rout sequence<beta1> and members of beta struct
  if (0 == (B1 = (beta1*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(beta1)*seqLen))) {
    nErr = AEE_ENORPCMEMORY;
    printf("ERROR 0x%x: memory alloc failed\n", nErr);
    goto bail;
  }

  for(int i=0; i<seqLen;i++){
    B1[i].ssLen = 2;
    if (0 == (B1[i].ss = (alpha1*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha1)*(B1[i].ssLen)))) {
      nErr = AEE_ENORPCMEMORY;
      printf("ERROR 0x%x: memory alloc failed\n", nErr);
      goto bail;
    }
    for(int j=0;j<B1[i].ssLen;j++){
      B1[i].ss[j].sLen = 2;
      if (0 == (B1[i].ss[j].s = (_cstring1_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(_cstring1_t)*(B1[i].ss[j].sLen)))) {
        nErr = AEE_ENORPCMEMORY;
        printf("ERROR 0x%x: memory alloc failed\n", nErr);
        goto bail;
      }
      for(int k=0;k<B[i].ss[j].sLen;k++){
        B1[i].ss[j].s[k].dataLen = 5;
        if (0 == (B1[i].ss[j].s[k].data = (char*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(char)*(B1[i].ss[j].s[k].dataLen)))) {
          nErr = AEE_ENORPCMEMORY;
          printf("ERROR 0x%x: memory alloc failed\n", nErr);
          goto bail;
        }
      }
    }
  }

  printf("Run testcase on domain %d\n", domain_id);
  /* Build URI using util helper*/
  nErr = get_uri(domain_id, complex_sequence_2_URI, (int)strlen(complex_sequence_2_URI), &uri);
  if (nErr) { printf("ERROR: get_uri failed (%d)\n", nErr); goto bail; }
  printf("URI is %s\n", uri);

  /* Configure Signed/Unsigned PD via util helper */
  nErr = set_unsigned_module_loading(domain_id, is_signedpd_requested);
  if (nErr) {
      printf("ERROR %d: configuring unsigned module loading failed for domain %d\n", nErr, domain_id);
      goto bail;
  }
  printf("\nCall method1 on the DSP\n");

  do {
    if (AEE_SUCCESS == (nErr = complex_sequence_2_open(uri, &handle))) {
      nErr =  complex_sequence_2_method1(handle, b, seqLen, b1, seqLen);
    //Checking the rout sequence on APPS side
    if (!nErr) {
      a=0;
      for(int i=0;i<seqLen;i++){
        for(int j=0;j<b1[i].alpLen;j++){
          for(int k=0; k< b1[i].alp[j].lLen; k++){
            if ((b1[i].alp[j].x != 2*(j+1)) || b1[i].z != i+1 || (b1[i].alp[j].l[k] != i+j+k)){
              a++;
            break;
            }
          }
        }
      }
    if(!a)
        printf("Completion of method1 on the DSP\n");
    else
        printf("==== Data Mismatch in method1====\n");
      break;
    } else {
      if (nErr == AEE_ECONNRESET) {
        /* AEE_ECONNRESET is returned when Sub-system restart (SSR) happens */
        retry--;
      } else if (nErr == AEE_ENOSUCH || nErr == AEE_EBADSTATE) {
        /* AEE_ENOSUCH is returned when Protection domain restart (PDR) happens and
        AEE_EBADSTATE is returned when PD is exiting or crashing.*/
        retry -= 2;
      } else {
        break;
      }
    }


    /* Close the handle and retry handle open */
    if (handle != -1) {
      if (AEE_SUCCESS != complex_sequence_2_close(handle)) {
        printf("ERROR 0x%x: Failed to close handle\n", nErr);
      }
    }
    }
  } while(retry);

  if (nErr) {
    printf("- retry attempt unsuccessful. Timing out....\n");
    printf("ERROR 0x%x: Failed to run method1 on domain %d\n", nErr, domain_id);
  }
  printf("\nCall method2 on the DSP\n");
  if(handle != -1){
    nErr = complex_sequence_2_method2(handle, B, seqLen, B1, seqLen);
    //Checking the rout sequence on APPS side
    if(!nErr){
      a=0;
      for(int i=0;i<seqLen;i++){
        for(int j=0;j< B1[i].ssLen;j++){
          for(int k=0;k< B1[i].ss[j].sLen;k++){
            for(int c=0;c< (B1[i].ss[j].s[k].dataLen)-1; c++){
              if(B1[i].ss[j].y != i+j || B1[i].ss[j].s[k].data[c] != (char) (65+i+j+k+c)){
                  a++;
                  break;
              }
            }
          }
        }
      }
      if(!a)
        printf("Completion of method2 on the DSP\n");
      else
        printf("==== Data Mismatch in method2 ====\n");
    }
    else{
        printf("ERROR 0x%x: Failed to run method2 on domain %d\n", nErr, domain_id);
    }
  }
  else
    printf("Failed to open the handle\n");

  if (handle != -1) {
    if (AEE_SUCCESS != (nErr = complex_sequence_2_close(handle))) {
      printf("ERROR 0x%x: Failed to close handle\n", nErr);
    }
  }

bail:
  if (uri) {
    free(uri);
  }
  for(int i=0;i<seqLen;i++){
    for(int j=0;j<b[i].alpLen;j++){
      if(b[i].alp[j].l){
        rpcmem_free(b[i].alp[j].l);
      }
    }
    if(b[i].alp){
      rpcmem_free(b[i].alp);
    }
  }
  if(b){
    rpcmem_free(b);
  }
  for(int i=0;i<seqLen;i++){
    for(int j=0;j<b1[i].alpLen;j++){
      if(b1[i].alp[j].l){
        rpcmem_free(b1[i].alp[j].l);
      }
    }
    if(b1[i].alp){
      rpcmem_free(b1[i].alp);
    }
  }
  if(b1){
    rpcmem_free(b1);
  }
  for(int i=0; i< seqLen; i++){
    for(int j=0; j< B[i].ssLen; j++){
      for(int k=0; k < B[i].ss[j].sLen; k++){
        if(B[i].ss[j].s[k].data){
          rpcmem_free(B[i].ss[j].s[k].data);
        }
      }
      if(B[i].ss[j].s){
        rpcmem_free(B[i].ss[j].s);
      }
    }
    if(B[i].ss){
      rpcmem_free(B[i].ss);
    }
  }
  if(B){
    rpcmem_free(B);
  }
  for(int i=0; i< seqLen; i++){
    for(int j=0; j< B1[i].ssLen; j++){
      for(int k=0; k < B1[i].ss[j].sLen; k++){
        if(B1[i].ss[j].s[k].data){
          rpcmem_free(B1[i].ss[j].s[k].data);
        }
      }
      if(B1[i].ss[j].s){
        rpcmem_free(B1[i].ss[j].s);
      }
    }
    if(B1[i].ss){
      rpcmem_free(B1[i].ss);
    }
  }
  if(B1){
    rpcmem_free(B1);
  }
 return nErr;
}

