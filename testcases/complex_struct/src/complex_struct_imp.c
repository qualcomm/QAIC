//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "HAP_farf.h"
#include "complex_struct.h"

int complex_struct_open(const char*uri, remote_handle64* handle) {
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
int complex_struct_close(remote_handle64 handle) {
   if (handle)
      free((void*)handle);
   return 0;
}

int complex_struct_sum(remote_handle64 h, const beta* vec, int64_t* res)
{
  *res = 0;
  for(int i=0;i<vec->struct_ptrLen;i++)
  {
      *res+=((vec->struct_ptr)[i].length+(vec->struct_ptr)[i].width+(vec->struct_ptr)[i].height);
  }
  if(*res!=810)
  {
      FARF(ALWAYS,"===============Data Mismatch Error on DSP: %lld ==============", *res);
      return -1;
  }
  FARF(ALWAYS,"===============     DSP: %lld ==============", *res);
  return 0;
}

int complex_struct_sum2(remote_handle64 _h, const beta* vec1, beta* vec2){

  int res[vec1->struct_ptrLen];
  for(int i=0;i<vec1->struct_ptrLen;i++)
    res[i] = vec1->struct_ptr[i].length + vec1->struct_ptr[i].width + vec1->struct_ptr[i].height;
  if(res[0] != 12 || res[1] != 18 || res[2] != 24 || vec1->struct_ptrLen != vec2->struct_ptrLen){
    FARF(ERROR, " === Data Mismatch === \n");
    return -1;
  }
  for(int i=0;i<vec2->struct_ptrLen;i++){
    vec2->struct_ptr[i].length = 2 *(vec1->struct_ptr[i].length);
    vec2->struct_ptr[i].width = 2*(vec1->struct_ptr[i].width);
    vec2->struct_ptr[i].height = 2*(vec1->struct_ptr[i].height);
  }
  return 0;
}


int complex_struct_sm1(remote_handle64 _h, const alpha1* struc1, alpha1* struc2){

  if(struc1->strLen!=6 || struc2->strLen!= 6){
    FARF(ERROR, " === Data Mismatch in string length === \n");
    return -1;
  }
  if(strncmp(struc1->str, "ABCDE",5)!=0){
    FARF(ERROR, " === Data Mismatch === \n");
    return -1;
  }
  if (struc2->strLen > 0) {
        size_t n = (struc2->strLen - 1);
        size_t src_payload = (struc1->strLen > 0) ? (struc1->strLen - 1) : 0;
        if (src_payload < n) n = src_payload;
        memcpy(struc2->str, struc1->str, n);
        struc2->str[n] = '\0';
    }
  return 0;
}

int complex_struct_sm2(remote_handle64 _h, const beta1* struc1, beta1* struc2){

  if(struc1->a.strLen!= 5 || struc2->a.strLen!= 3){
    FARF(ERROR, " === Data Mismatch in string length === \n");
    return -1;
  }
  if(strncmp(struc1->a.str, "abcd",4)!=0){
    FARF(ERROR, " === Data Mismatch === \n");
    return -1;
  }
  if (struc2->a.strLen > 0) {
        size_t n = (struc2->a.strLen - 1);
        size_t src_payload = (struc1->a.strLen > 0) ? (struc1->a.strLen - 1) : 0;
        if (src_payload < n) n = src_payload;
        memcpy(struc2->a.str, struc1->a.str, n);
        struc2->a.str[n] = '\0';
    }
  struc1->a.str[2] = '\0';
  return 0;
}

int complex_struct_wsm1(remote_handle64 _h, const alpha2* struc1, alpha2* struc2){

  if(struc1->wstrLen!=struc2->wstrLen){
    FARF(ERROR, " === Data Mismatch in wstring length === \n");
    return -1;
  }
  for(int i=0;i<(struc1->wstrLen)-1;i++){
    if(struc1->wstr[i] != (wchar_t) (100+i)){
      FARF(ERROR, " === Data Mismatch === \n");
      return -1;
    }
  }
  for(int i=0;i<(struc2->wstrLen)-1;i++){
    struc2->wstr[i] = (wchar_t) (65+i);
  }
  struc2->wstr[(struc2->wstrLen)-1] = L'\0';
  return 0;
}

int complex_struct_wsm2(remote_handle64 _h, const beta2* struc3, beta2* struc4){

  if(struc3->a.wstrLen!=struc4->a.wstrLen){
    FARF(ERROR, " === Data Mismatch in wstring length === \n");
    return -1;
  }
  for(int i=0;i<(struc3->a.wstrLen)-1;i++){
    if(struc3->a.wstr[i] != (wchar_t) (100+i)){
      FARF(ERROR, " === Data Mismatch === \n");
      return -1;
    }
  }
  for(int i=0;i<(struc4->a.wstrLen)-1;i++){
    struc4->a.wstr[i] = (wchar_t) (65+i);
  }
  struc4->a.wstr[(struc4->a.wstrLen)-1] = L'\0';
  return 0;
}
