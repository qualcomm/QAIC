//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "qaic_testcase.h"

int qaic_testcase_open(const char*uri, remote_handle64* handle) {
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
int qaic_testcase_close(remote_handle64 handle) {
   if (handle)
      free((void*)handle);
   return 0;
}

int qaic_testcase_m1(remote_handle64 _h, _wchar_t arg1, unsigned char arg2, char arg3, short arg4, unsigned short arg5, int arg6, unsigned int arg7, int64_t arg8, uint64_t arg9, signed char var, unsigned char var1, signed short var2, unsigned short var3, int32_t var4, uint32_t var5, int64_t var6, uint64_t var7, int8_t var8, uint8_t var9, int16_t num1, uint16_t num2, int32_t num3, uint32_t num4, int64_t num5, uint64_t num6, float num7, double num8, bool num9, traffic enum1){

  if(arg1!=L'A' || (char)arg2!= 'a'||arg3!= 'd' || arg4 != -1 || arg5!=1 || arg6!=-10 || arg7!= 10 || arg8!=-100 ||arg9!=100 || var!=2 || var1!=3 || var2 != 4 || var3!=5 || var4 !=6 ||var5!= 7 || var6!= 8 ||var7!=9 || var8!= 10 || var9!= 11 || num1 != 12 ||num2 !=13 || num3!= 14 || num4!= 15 ||num5!= 16 ||num6!= 17 || num7!= 18 || num8!= 19 || num9 || enum1 !=2){
    // FARF(ERROR, "=== Data Mismatch ===\n");
    return -1;
  }
  return 0;
}

int qaic_testcase_m2(remote_handle64 _h, _wchar_t* arg1, unsigned char* arg2, char* arg3, short* arg4, unsigned short* arg5, int* arg6, unsigned int* arg7, int64_t* arg8, uint64_t* arg9, signed char* var, unsigned char* var1, signed short* var2, unsigned short* var3, int32_t* var4, uint32_t* var5, int64_t* var6, uint64_t* var7, int8_t* var8, uint8_t* var9, int16_t* num1, uint16_t* num2, int32_t* num3, uint32_t* num4, int64_t* num5, uint64_t* num6, float* num7, double* num8, bool* num9, traffic* enum1){

  *arg1 = (wchar_t) 66;
  *arg2 = (char) 98;
  *arg3 = (char) 101;
  *arg4 = -2;
  *arg5 = 2;
  *arg6 = -9;
  *arg7 = 9;
  *arg8 = -99;
  *arg9 = 99;
  *var = 3;
  *var1 = 4;
  *var2 = 5;
  *var3 = 6;
  *var4 = 7;
  *var5 = 8;
  *var6 = 9;
  *var7 = 10;
  *var8 = 11;
  *var9 = 12;
  *num1 = 13;
  *num2 = 14;
  *num3 = 15;
  *num4 = 16;
  *num5 = 17;
  *num6 = 18;
  *num7 = 19;
  *num8 = 20;
  *num9 = 1;
  *enum1 = RED;
  return 0;
}

int qaic_testcase_m3(remote_handle64 _h, _wchar_t* arg1, unsigned char* arg2, char* arg3, short* arg4, unsigned short* arg5, int* arg6, unsigned int* arg7, int64_t* arg8, uint64_t* arg9, signed char* var, unsigned char* var1, signed short* var2, unsigned short* var3, int32_t* var4, uint32_t* var5, int64_t* var6, uint64_t* var7, int8_t* var8, uint8_t* var9, int16_t* num1, uint16_t* num2, int32_t* num3, uint32_t* num4, int64_t* num5, uint64_t* num6, float* num7, double* num8, bool* num9, traffic* enum1){

  if(*arg1!=L'B' || (char)*arg2!= 'b'||*arg3!= 'e' || *arg4 != -2 || *arg5!= 2 || *arg6!= -9 || *arg7!= 9 || *arg8!= -99 || *arg9 != 99 || *var != 3 || *var1!=4 || *var2 != 5 || *var3!=6 || *var4 != 7 || *var5!= 8 || *var6!= 9 || *var7!= 10 || *var8!= 11 || *var9!= 12 || *num1 != 13 || *num2 !=14 || *num3!= 15 || *num4!= 16 || *num5!= 17 || *num6!= 18 || *num7!= 19 || *num8!= 20 || *num9!=1 || *enum1 != 0){
    // FARF(ERROR, "=== Data Mismatch ===\n");
    return -1;
  }
  *arg1 = (wchar_t) ((int)(*arg1)+1);
  *arg2 = (char) ((int)(*arg2)+1);
  *arg3 = (char) ((int)(*arg3)+1);
  *arg4 = *arg4 +1;
  *arg5 = *arg5 +1;
  *arg6 = *arg6 +1;
  *arg7 = *arg7 +1;
  *arg8 = *arg8 +1;
  *arg9 = *arg9 +1;
  *var = *var +1;
  *var1 = *var1 +1;
  *var2 = *var2 +1;
  *var3 = *var3 +1;
  *var4 = *var4 +1;
  *var5 = *var5 +1;
  *var6 = *var6 +1;
  *var7 = *var7 +1;
  *var8 = *var8 +1;
  *var9 = *var9 +1;
  *num1 = *num1 +1;
  *num2 = *num2 +1;
  *num3 = *num3 +1;
  *num4 = *num4 +1;
  *num5 = *num5 +1;
  *num6 = *num6 +1;
  *num7 = *num7 +1;
  *num8 = *num8 +1;
  *num9 = 0;
  *enum1 = YELLOW;
  return 0;
}

int qaic_testcase_m4(remote_handle64 _h, const alpha* struc1, alpha* struc2){

  if(struc1->enum1 != 0 || struc1->enum2Len != struc2->enum2Len){
    // FARF(ERROR, "=== Data Mismatch ===\n");
    return -1;
  }
  for(int i=0;i<struc1->enum2Len; i++){
    if(struc1->enum2[i] != i){
      // FARF(ERROR, "=== Data Mismatch ===\n");
      return -1;
    }
  }
  struc2->enum1 = YELLOW;
  for(int i=0;i<struc2->enum2Len;i++){
    struc2->enum2[i] = struc2->enum2Len-i-1;
  }
  return 0;
}

int qaic_testcase_m5(remote_handle64 _h, const alpha* seq1, int seq1Len, alpha* seq2, int seq2Len){

  if(seq1Len != seq2Len){
    // FARF(ERROR, "=== Data Mismatch ===\n");
    return -1;
  }
  for(int i=0;i<seq1Len;i++){
    if(seq1[i].enum1 != 0){
      // FARF(ERROR, "=== Data Mismatch ===\n");
      return -1;
    }
    for(int j=0;j<seq1[i].enum2Len;j++){
      if(seq1[i].enum2[j] != 1){
        // FARF(ERROR, "=== Data Mismatch ===\n");
        return -1;
      }
    }
  }
  seq2[0].enum1 = RED;
  seq2[1].enum1 = YELLOW;
  for(int i=0;i<seq2Len;i++){
    seq2[i].enum2[0] = RED;
    seq2[i].enum2[1] = YELLOW;
    seq2[i].enum2[2] = GREEN;
  }
  return 0;
}