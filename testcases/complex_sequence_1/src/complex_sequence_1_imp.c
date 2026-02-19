/*==============================================================================
  Copyright (c) 2012-2014, 2020, 2023 Qualcomm Technologies, Inc.
  All rights reserved. Qualcomm Proprietary and Confidential.
==============================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "HAP_farf.h"
#include "complex_sequence_1.h"
#include <string.h>

int complex_sequence_1_open(const char*uri, remote_handle64* handle) {
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
int complex_sequence_1_close(remote_handle64 handle) {
   if (handle)
      free((void*)handle);
   return 0;
}

int complex_sequence_1_sum(remote_handle64 h, const beta* vec, int vecLen)
{
  FARF(ALWAYS, "reached imp execution %d",vecLen);
  int sum=0;
  for(int j=0;j<vecLen;j++)
  {
      for(int i=0;i<vec[j].struct_ptrLen;i++)
      {
          sum+=((vec[j].struct_ptr)[i].length+(vec[j].struct_ptr)[i].width+(vec[j].struct_ptr)[i].height);
      }
  }
  if(sum!=4050)
  {
      FARF(ALWAYS,"Data Mismatch error!!");
      return -1;
  }
  return 0;
}

int complex_sequence_1_sum2(remote_handle64 h, const beta* vec, int vecLen, beta* vec1, int vec1Len){

  if(vecLen != vec1Len){
    FARF(ERROR, " === Data Mismatch in sequence length === \n");
    return -1;
  }
  for(int i=0;i<vecLen;i++){
    if(vec[i].struct_ptrLen != vec1[i].struct_ptrLen){
      FARF(ERROR, " === Data Mismatch in sequence length === \n");
      return -1;
    }
  }
  int sum = 0;
  for(int i=0;i<vecLen;i++){
    for(int j=0;j<vec[i].struct_ptrLen;j++){
      sum = sum +vec[i].struct_ptr[j].length+ vec[i].struct_ptr[j].width + vec[i].struct_ptr[j].height;
    }
  }
  if(sum != 150){
    FARF(ERROR, " === Data Mismatch === \n");
    return -1;
  }
  for(int i =0;i<vec1Len;i++){
    for(int j=0;j<vec1[i].struct_ptrLen;j++){
      vec1[i].struct_ptr[j].length = 2* vec[i].struct_ptr[j].length;
      vec1[i].struct_ptr[j].width = 2* vec[i].struct_ptr[j].width;
      vec1[i].struct_ptr[j].height = 2* vec[i].struct_ptr[j].height;
    }
  }
  return 0;
}

int complex_sequence_1_sm1(remote_handle64 _h, const alpha1* seq, int seqLen, alpha1* seq1, int seq1Len){

  if(seqLen != seq1Len){
    FARF(ERROR, " === Data Mismatch in sequence length === \n");
    return -1;
  }
  for(int i=0;i<seqLen;i++){
    if(seq[i].strLen != seq1[i].strLen){
      FARF(ERROR, " === Data Mismatch in string length === \n");
      return -1;
    }
  }
  for(int i=0;i<seqLen;i++){
    for(int j=0;j<(seq[i].strLen)-1;j++){
      if(seq[i].str[j]!= (char)(65+i+j)){
        FARF(ERROR, " === Data Mismatch === \n");
        return -1;
      }
    }
  }
  for(int i =0;i<seq1Len;i++){
    for(int j=0;j<(seq1[i].strLen)-1;j++){
      seq1[i].str[j] = (char) (((int)seq[i].str[j])+1);
    }
    seq1[i].str[(seq1[i].strLen)-1] = '\0';
  }
  return 0;
}

int complex_sequence_1_sm2(remote_handle64 _h, const beta1* cseq, int cseqLen, beta1* cseq1, int cseq1Len){

  if(cseqLen != cseq1Len){
    FARF(ERROR, " === Data Mismatch in sequence length === \n");
    return -1;
  }
  for(int i=0;i<cseqLen;i++){
    if(cseq[i].a.strLen != cseq1[i].a.strLen){
      FARF(ERROR, " === Data Mismatch in string length === \n");
      return -1;
    }
  }
  for(int i=0;i<cseqLen;i++){
    for(int j=0;j<(cseq[i].a.strLen)-1;j++){
      if(cseq[i].a.str[j]!= (char)(97+i+j)){
        FARF(ERROR, " === Data Mismatch === \n");
        return -1;
      }
    }
  }
  for(int i=0;i<cseq1Len;i++){
    strncpy(cseq1[i].a.str, cseq[i].a.str, 4);
    cseq1[i].a.str[4] = '\0';
  }
  return 0;
}

int complex_sequence_1_wsm1(remote_handle64 _h, const alpha2* seq, int seqLen, alpha2* seq1, int seq1Len){

  if(seqLen != seq1Len){
    FARF(ERROR, " === Data Mismatch in sequence length === \n");
    return -1;
  }
  for(int i=0;i<seqLen;i++){
    if(seq[i].wstrLen != seq1[i].wstrLen){
      FARF(ERROR, " === Data Mismatch in wstring length === \n");
      return -1;
    }
  }
  for(int i=0;i<seqLen;i++){
    for(int j=0;j<(seq[i].wstrLen)-1;j++){
      if(seq[i].wstr[j] != (wchar_t) (100+i+j)){
        FARF(ERROR, " === Data Mismatch === \n");
        return -1;
      }
    }
  }
  for(int i=0;i<seq1Len;i++){
    for(int j=0;j<(seq1[i].wstrLen)-1;j++){
      seq1[i].wstr[j] = (wchar_t) (65+i+j);
    }
    seq1[i].wstr[(seq1[i].wstrLen)-1] = L'\0';
  }
  return 0;
}

int complex_sequence_1_wsm2(remote_handle64 _h, const beta2* seq, int seqLen, beta2* seq1, int seq1Len){

  if(seqLen != seq1Len){
    FARF(ERROR, " === Data Mismatch in sequence length === \n");
    return -1;
  }
  for(int i=0;i<seqLen;i++){
    if(seq[i].a.wstrLen != seq1[i].a.wstrLen){
      FARF(ERROR, " === Data Mismatch in wstring length === \n");
      return -1;
    }
  }
  for(int i=0;i<seqLen;i++){
    for(int j=0;j<(seq[i].a.wstrLen)-1;j++){
      if(seq[i].a.wstr[j] != (wchar_t) (65+i+j)){
        FARF(ERROR, " === Data Mismatch === \n");
        return -1;
      }
    }
  }
  for(int i=0;i<seq1Len;i++){
    for(int j=0;j<(seq1[i].a.wstrLen)-1;j++){
      seq1[i].a.wstr[j] = (wchar_t) (97+i+j);
    }
    seq1[i].a.wstr[(seq1[i].a.wstrLen)-1] = L'\0';
  }
  return 0;
}