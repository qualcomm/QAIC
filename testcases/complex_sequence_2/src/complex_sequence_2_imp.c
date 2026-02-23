//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include "HAP_farf.h"
#include "complex_sequence_2.h"

int complex_sequence_2_open(const char*uri, remote_handle64* handle) {
   void *tptr = NULL;
  /* can be any value or ignored, rpc layer doesn't care
   * also ok
   * *handle = 0;
   * *handle = 0xdeadc0de;
   */
   tptr = (void *)malloc(1);
   *handle = (remote_handle64)tptr;
   return 0;
}

/**
 * @param handle, the value returned by open
 * @retval, 0 for success, should always succeed
 */
int complex_sequence_2_close(remote_handle64 handle) {
   if (handle)
      free((void*)handle);
   return 0;
}

int complex_sequence_2_method1(remote_handle64 _h, const beta*b, int bLen, beta* b1, int b1Len){
  //Checking the sequence length of in sequence received on the DSP
  if(bLen!=2){
    FARF(ERROR, " === Data Mismatch in sequence length === \n");
    return -1;
  }

  for(int i=0;i<bLen;i++){
    if(b[i].alpLen != 2){
      FARF(ERROR, " === Data Mismatch === \n");
      return -1;
    }
    for(int k=0;k<2;k++){
      if(b[i].alp[k].lLen != 2){
        FARF(ERROR, " === Data Mismatch === \n");
        return -1;
      }
    }
  }
  //Checking the data of the in sequence received on DSP
  int a=0;
  for(int i=0;i<bLen;i++){
    for(int j=0;j<b[i].alpLen;j++){
      for(int k=0; k< b[i].alp[j].lLen; k++){
        if ((b[i].alp[j].x != 2*(j+1)) || b[i].z != i+1 || (b[i].alp[j].l[k] != i+j+k)){
          a++;
        break;
        }
      }
    }
  }
  if(a){
    FARF(ERROR, " === Data Mismatch === \n");
    return -1;
  }
  //Checking the sequence length of rout sequence received on the DSP
  if(b1Len!=2){
    FARF(ERROR, " === Data Mismatch in sequence length === \n");
    return -1;
  }

  for(int i=0;i<b1Len;i++){
    if(b1[i].alpLen != 2){
      FARF(ERROR, " === Data Mismatch === \n");
      return -1;
    }
    for(int k=0;k<2;k++){
      if(b1[i].alp[k].lLen != 2){
        FARF(ERROR, " === Data Mismatch === \n");
        return -1;
      }
    }
  }
  //Initializing the rout sequence
  for(int i=0;i<b1Len;i++){
    b1[i].z = i +1;
    for(int j=0;j<b1[i].alpLen;j++){
      b1[i].alp[j].x = 2*(j+1);
      for(int k=0;k<b1[i].alp[j].lLen;k++){
        b1[i].alp[j].l[k] = i+j+k;
      }
    }
  }
  return 0;
}

int complex_sequence_2_method2(remote_handle64 _h, const beta1* B, int BLen, beta1* B1, int B1Len){
  //Checking the sequence length of in sequence received on the DSP
  if(BLen!=2){
    FARF(ERROR, " === Data Mismatch in sequence length === \n");
    return -1;
  }

  for(int i=0;i< BLen; i++){
    if(B[i].ssLen !=2 ){
      FARF(ERROR, " === Data Mismatch === \n");
      return -1;
    }
    for(int j=0;j<B[i].ssLen; j++){
      if(B[i].ss[j].sLen != 2){
        FARF(ERROR, " === Data Mismatch === \n");
        return -1;
      }
      for(int k=0;k<B[i].ss[j].sLen;k++){
        if(B[i].ss[j].s[k].dataLen != 5){
          FARF(ERROR, " === Data Mismatch in string length=== \n");
          return -1;
        }
      }
    }
  }
  //Checking the data of the in sequence received on DSP
  int a1=0;
  for(int i=0;i<BLen;i++){
    for(int j=0;j< B[i].ssLen;j++){
      for(int k=0;k< B[i].ss[j].sLen;k++){
        for(int c=0;c< (B[i].ss[j].s[k].dataLen)-1; c++){
          if(B[i].ss[j].y != i+j || B[i].ss[j].s[k].data[c] != (char) (65+i+j+k+c)){
              a1++;
              break;
          }
        }
      }
    }
  }
  if(a1){
    FARF(ERROR, " === Data Mismatch === \n");
    return -1;
  }
  //Checking the sequence length of rout sequence received on the DSP
  if(BLen!=2){
    FARF(ERROR, " === Data Mismatch in sequence length === \n");
    return -1;
  }

  for(int i=0;i< BLen; i++){
    if(B[i].ssLen !=2 ){
      FARF(ERROR, " === Data Mismatch === \n");
      return -1;
    }
    for(int j=0;j<B[i].ssLen; j++){
      if(B[i].ss[j].sLen != 2){
        FARF(ERROR, " === Data Mismatch === \n");
        return -1;
      }
      for(int k=0;k<B[i].ss[j].sLen;k++){
        if(B[i].ss[j].s[k].dataLen != 5){
          FARF(ERROR, " === Data Mismatch in string length=== \n");
          return -1;
        }
      }
    }
  }
  //Initializing the rout sequence
  for(int i=0;i<B1Len;i++){
    for(int j=0;j< B1[i].ssLen;j++){
      B1[i].ss[j].y = i+j;
      for(int k=0;k< B1[i].ss[j].sLen;k++){
        for(int a=0; a<(B1[i].ss[j].s[k].dataLen)-1; a++){
          B1[i].ss[j].s[k].data[a] = (char)(65+i+j+k+a);
        }
        B1[i].ss[j].s[k].data[(B1[i].ss[j].s[k].dataLen)-1] = '\0';
      }
    }
  }
  return 0;
}