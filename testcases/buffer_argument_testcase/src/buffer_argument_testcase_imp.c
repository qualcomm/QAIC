//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "HAP_farf.h"
#include "buffer_argument_testcase.h"

int buffer_argument_testcase_open(const char*uri, remote_handle64* handle) {
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
int buffer_argument_testcase_close(remote_handle64 handle) {
   if (handle)
      free((void*)handle);
   return 0;
}

int fact(int a){

  if(a<0)
   return -1;
  else if(a==0 || a==1)
    return 1;
  else
    return a*fact(a-1);
}
int buffer_argument_testcase_method1(remote_handle64 _h, const alpha* a, alpha* b, alpha* c){

  //validating in and inrout params
  if(a->arg1!='a'||(int)a->arg2!= 1|| a->arg3!='d'||a->arg4!= 4||a->arg5!= 5||a->arg6!= 6||a->arg7!= -7|| a->arg8!= 8||a->arg9!= 9||a->num1!= -1||a->num2!= 2||a->num3!= -3||a->num4!= 4||a->num5!= -5||a->num6!= 6||a->num7!= -7||a->num8!= 8||a->num9!= 4||a->var1!= 11||a->var2!= 22||a->var3!= 33||a->var4!= 44||a->var5!= 55||a->var6!=66||a->var7!= 77||a->var8!= 88.788f||a->var9!= 99.9999||a->b1){
    FARF(ERROR, "==== ERROR: Data Mismatch for in param ====\n");
    return -1;
  }

  if(c->arg1!='a'||(int)c->arg2!= 1|| c->arg3!='d'||c->arg4!= 4||c->arg5!= 5||c->arg6!= 6||c->arg7!= -7|| c->arg8!= 8||c->arg9!= 9||c->num1!= -1||c->num2!= 2||c->num3!= -3||c->num4!= 4||c->num5!= -5||c->num6!= 6||c->num7!= -7||c->num8!= 8||c->num9!= 4||c->var1!= 11||c->var2!= 22||c->var3!= 33||c->var4!= 44||c->var5!= 55||c->var6!=66||c->var7!= 77||c->var8!= 88.788f||c->var9!= 99.9999||c->b1){
    FARF(ERROR, "==== ERROR: Data Mismatch for inrout param ====\n");
    return -1;
  }

  c->arg1 = b->arg1 = (wchar_t)(((int) (a->arg1))+1);
  c->arg2 = b->arg2 = a->arg2;
  c->arg3 = b->arg3 = (char)(((int) (a->arg3))+1);
  c->arg4 = b->arg4 = fact(a->arg4);
  c->arg5 = b->arg5 = fact(a->arg5);
  c->arg6 = b->arg6 = fact(a->arg6);
  c->arg7 = b->arg7 = (a->arg7)*10;
  c->arg8 = b->arg8 = fact(a->arg8);
  c->arg9 = b->arg9 = fact(a->arg9);
  c->num2 = b->num2 = fact(a->num2);
  c->num1 = b->num1 = (a->num1)*10;
  c->num4 = b->num4 = fact(a->num4);
  c->num3 = b->num3 = (a->num3)*10;
  c->num6 = b->num6 = fact(a->num6);
  c->num5 = b->num5 = (a->num5)*10;
  c->num7 = b->num7 = fact(a->num7);
  c->num8 = b->num8 = (a->num8)*10;
  c->num9 = b->num9 = fact(a->num9);
  c->var1 = b->var1 = 10* (a->var1);
  c->var2 = b->var2 = 10* (a->var2);
  c->var3 = b->var3 = 10* (a->var3);
  c->var4 = b->var4 = 10* (a->var4);
  c->var5 = b->var5 = 10* (a->var5);
  c->var6 = b->var6 = 10* (a->var6);
  c->var7 = b->var7 = 10* (a->var7);
  c->var8 = b->var8 = 10* (a->var8);
  c->var9 = b->var9 = 10* (a->var9);
  b->b1 = !(!(a->b1));
  c->b1 = !(a->b1)||(b->b1);
  return 0;
}

int buffer_argument_testcase_method2(remote_handle64 _h, const alpha* vec, int vecLen, alpha* vec1, int vec1Len, alpha* vec2, int vec2Len){

  if(vecLen != vec1Len ||vecLen!= vec2Len){
    FARF(ERROR, "=== Data Mismatch ===\n");
    return -1;
  }
  int k=0;
  //Validating in and inrout sequence contents
  if(vec[k].arg1!= 'a'||(int)vec[k].arg2!= 65|| vec[k].arg3!='d'||vec[k].arg4!= 4||vec[k].arg5!= 5||vec[k].arg6!= 6||vec[k].arg7!= -7|| vec[k].arg8!= 1||vec[k].arg9!= 9||vec[k].num1!= -1||vec[k].num2!= 2||vec[k].num3!= -3||vec[k].num4!= 4||vec[k].num5!= -5||vec[k].num6!= 6||vec[k].num7!= -7||vec[k].num8!= 8||vec[k].num9!= 4||vec[k].var1!= 11||vec[k].var2!= 22||vec[k].var3!= 33||vec[k].var4!= 44||vec[k].var5!= 55||vec[k].var6!=66||vec[k].var7!= 77||vec[k].var8!= 88.788f||vec[k].var9!= 99.9999||vec[k].b1){
    FARF(ERROR, "==== ERROR: Data Mismatch for in sequence member %d ====\n",k);
    return -1;
  }
    if(vec2[k].arg1!='a'||(char)vec2[k].arg2!= 'A'|| vec2[k].arg3!='d'||vec2[k].arg4!= 4||vec2[k].arg5!= 5||vec2[k].arg6!= 6||vec2[k].arg7!= -7|| vec2[k].arg8!= 1||vec2[k].arg9!= 9||vec2[k].num1!= -1||vec2[k].num2!= 2||vec2[k].num3!= -3||vec2[k].num4!= 4||vec2[k].num5!= -5||vec2[k].num6!= 6||vec2[k].num7!= -7||vec2[k].num8!= 8||vec2[k].num9!= 4||vec2[k].var1!= 11||vec2[k].var2!= 22||vec2[k].var3!= 33||vec2[k].var4!= 44||vec2[k].var5!= 55||vec2[k].var6!=66||vec2[k].var7!= 77||vec2[k].var8!= 88.788f||vec2[k].var9!= 99.9999||vec2[k].b1){
    FARF(ERROR, "==== ERROR: Data Mismatch for inrout sequence member %d ====\n",k);
    return -1;
  }
  k++;
  if(vec[k].arg1!='b'||(char)vec[k].arg2!= 'B'|| vec[k].arg3!='e'||vec[k].arg4!= 5||vec[k].arg5!= 6||vec[k].arg6!= 7||vec[k].arg7!= -6|| vec[k].arg8!= 2||vec[k].arg9!= 10||vec[k].num1!= 0||vec[k].num2!= 3||vec[k].num3!= -2||vec[k].num4!= 5||vec[k].num5!= -4||vec[k].num6!= 7||vec[k].num7!= -6||vec[k].num8!= 9||vec[k].num9!= 5||vec[k].var1!= 12||vec[k].var2!= 23||vec[k].var3!= 34||vec[k].var4!= 45||vec[k].var5!= 56||vec[k].var6!=67||vec[k].var7!= 78||vec[k].var8!= 89.788f||vec[k].var9!= 100.9999||vec[k].b1){
    FARF(ERROR, "==== ERROR: Data Mismatch for in sequence member %d ====\n",k);
    return -1;
  }
  if(vec2[k].arg1!='b'||(char)vec2[k].arg2!= 'B'|| vec2[k].arg3!='e'||vec2[k].arg4!= 5||vec2[k].arg5!= 6||vec2[k].arg6!= 7||vec2[k].arg7!= -6|| vec2[k].arg8!= 2||vec2[k].arg9!= 10||vec2[k].num1!= 0||vec2[k].num2!= 3||vec2[k].num3!= -2||vec2[k].num4!= 5||vec2[k].num5!= -4||vec2[k].num6!= 7||vec2[k].num7!= -6||vec2[k].num8!= 9||vec2[k].num9!= 5||vec2[k].var1!= 12||vec2[k].var2!= 23||vec2[k].var3!= 34||vec2[k].var4!= 45||vec2[k].var5!= 56||vec2[k].var6!=67||vec2[k].var7!= 78||vec2[k].var8!= 89.788f||vec2[k].var9!= 100.9999||vec2[k].b1){
    FARF(ERROR, "==== ERROR: Data Mismatch for inrout sequence member %d ====\n",k);
    return -1;
  }
  for(int i=0;i<vec1Len;i++){
    vec1[i].arg1 = (wchar_t)(((int) vec[i].arg1)+1);
    vec1[i].arg2 = vec[i].arg2;
    vec1[i].arg3 = vec[i].arg3;
    vec1[i].arg4 = fact(vec[i].arg4);
    vec1[i].arg5 = fact(vec[i].arg5);
    vec1[i].arg6 = fact(vec[i].arg6);
    vec1[i].arg7 = vec[i].arg7;
    vec1[i].arg8 = fact(vec[i].arg8);
    vec1[i].arg9 = fact(vec[i].arg9);
    vec1[i].num2 = vec[i].num2;
    vec1[i].num1 = vec[i].num1;
    vec1[i].num4 = vec[i].num4;
    vec1[i].num3 = vec[i].num3;
    vec1[i].num6 = vec[i].num6;
    vec1[i].num5 = vec[i].num5;
    vec1[i].num7 = vec[i].num7;
    vec1[i].num8 = vec[i].num8;
    vec1[i].num9 = vec[i].num9;
    vec1[i].var1 = vec[i].var1;
    vec1[i].var2 = vec[i].var2;
    vec1[i].var3 = vec[i].var3;
    vec1[i].var4 = vec[i].var4;
    vec1[i].var5 = vec[i].var5;
    vec1[i].var6 = vec[i].var6;
    vec1[i].var7 = vec[i].var7;
    vec1[i].var8 = vec[i].var8;
    vec1[i].var9 = vec[i].var9;
    vec1[i].b1 = !(vec[i].b1);
  }
  for(int i=0;i<vec2Len;i++){
    vec2[i].arg1 = (wchar_t)(((int) vec2[i].arg1)+1);
    vec2[i].arg2 = (char) 68;
    vec2[i].arg3 = (char)(((int)vec2[i].arg3)+1) ;
    vec2[i].arg4 = fact(vec2[i].arg4);
    vec2[i].arg5 = fact(vec2[i].arg5);
    vec2[i].arg6 = fact(vec2[i].arg6);
    vec2[i].arg7 = vec2[i].arg7+1;
    vec2[i].arg8 = vec2[i].arg8+1;
    vec2[i].arg9 = vec2[i].arg9+1;
    vec2[i].num2 = vec2[i].num2+1;
    vec2[i].num1 = vec2[i].num1+1;
    vec2[i].num4 = vec2[i].num4+1;
    vec2[i].num3 = vec2[i].num3+1;
    vec2[i].num6 = vec2[i].num6+1;
    vec2[i].num5 = vec2[i].num5+1;
    vec2[i].num7 = vec2[i].num7+1;
    vec2[i].num8 = vec2[i].num8+1;
    vec2[i].num9 = vec2[i].num9+1;
    vec2[i].var1 = vec2[i].var1+1;
    vec2[i].var2 = vec2[i].var2+1;
    vec2[i].var3 = vec2[i].var3+1;
    vec2[i].var4 = vec2[i].var4+1;
    vec2[i].var5 = vec2[i].var5+1;
    vec2[i].var6 = vec2[i].var6+1;
    vec2[i].var7 = vec2[i].var7+1;
    vec2[i].var8 = vec2[i].var8+1;
    vec2[i].var9 = vec2[i].var9+1;
    vec2[i].b1 = !(vec2[i].b1);
  }
  return 0;
}

int buffer_argument_testcase_method3(remote_handle64 _h, const unsigned short* ushort, int ushortLen, const int64_t* llong, int llongLen, const unsigned int* ulong, int ulongLen, const uint64_t* ullong, int ullongLen, const signed char* nt, int ntLen, const unsigned char* unt, int untLen, const signed short* nt2, int nt2Len, const int32_t* nt3, int nt3Len, const uint32_t* unt3, int unt3Len, const uint8_t* t1, int t1Len, const uint16_t* t2, int t2Len, const uint32_t* t3, int t3Len, const uint64_t* t4, int t4Len, const bool* b1, int b1Len){
  if(ushortLen!=llongLen||ushortLen!=ulongLen||ushortLen!=ullongLen||ushortLen!=ntLen||ushortLen!=untLen||ushortLen!=nt2Len||ushortLen!=nt3Len||ushortLen!=unt3Len||ushortLen!=t1Len||ushortLen!=t2Len||ushortLen!=t3Len||ushortLen!=t4Len||ushortLen!=b1Len){
    FARF(ERROR, "=== Data Mismatch of sequence length ====\n");
    return -1;
  }
  //Validating in sequence contents
  int res1 = 0, res2 =0, res3 = 0, res4 = 0,res5 =0, res6 =0,res7=0,res8=0,res9 =0,resu1=0,resu2=0,resu3=0,resu4=0,res=0;
  for(int i =0;i<ushortLen;i++){
    res1 = res1+ushort[i];
    res2 = res2+llong[i];
    res3 = res3+ulong[i];
    res4 = res4+ullong[i];
    res5 = res5+nt[i];
    res6 = res6+unt[i];
    res7 = res7+nt2[i];
    res8 = res8+nt3[i];
    res9 = res9+unt3[i];
    resu1 = resu1+t1[i];
    resu2 = resu2+t2[i];
    resu3 = resu3+t3[i];
    resu4 = resu4+t4[i];
    res = res+b1[i];
  }
    if(res1!= 36|| res2!=36||res3!= 36||res4!=36||res5!= 36||res6!= 36||res7!= 36||res8!= 36||res9!= 36||resu1!= 36||resu2!= 36||resu3!= 36||resu4!=36 || res!=7){
      FARF(ERROR, "=== Data Mismatch ====\n");
      return -1;
    }
  return 0;
}

int buffer_argument_testcase_method4(remote_handle64 _h, unsigned short* ushort, int ushortLen, int64_t* llong, int llongLen, unsigned int* ulong, int ulongLen, uint64_t* ullong, int ullongLen, signed char* nt, int ntLen, unsigned char* unt, int untLen, signed short* nt2, int nt2Len, int32_t* nt3, int nt3Len, uint32_t* unt3, int unt3Len, uint8_t* t1, int t1Len, uint16_t* t2, int t2Len, uint32_t* t3, int t3Len, uint64_t* t4, int t4Len, bool* b1, int b1Len){
  if(ushortLen!=llongLen||ushortLen!=ulongLen||ushortLen!=ullongLen||ushortLen!=ntLen||ushortLen!=untLen||ushortLen!=nt2Len||ushortLen!=nt3Len||ushortLen!=unt3Len||ushortLen!=t1Len||ushortLen!=t2Len||ushortLen!=t3Len||ushortLen!=t4Len||ushortLen!=b1Len){
    FARF(ERROR, "=== Data Mismatch of sequence length ===\n");
    return -1;
  }

  for(int i=0;i<ushortLen;i++){
    ushort[i] = llong[i] = ulong[i] = ullong[i] = nt[i] = unt[i] = nt2[i] = nt3[i] = unt3[i] = t1[i] = t2[i] = t3[i] = t4[i] = 2*(i+1);
    b1[i] = (bool)(i+1);
  }
  return 0;
}

int buffer_argument_testcase_method5(remote_handle64 _h, unsigned short* ushort, int ushortLen, int64_t* llong, int llongLen, unsigned int* ulong, int ulongLen, uint64_t* ullong, int ullongLen, signed char* nt, int ntLen, unsigned char* unt, int untLen, signed short* nt2, int nt2Len, int32_t* nt3, int nt3Len, uint32_t* unt3, int unt3Len, uint8_t* t1, int t1Len, uint16_t* t2, int t2Len, uint32_t* t3, int t3Len, uint64_t* t4, int t4Len, bool* b1, int b1Len){

  if(ushortLen!=llongLen||ushortLen!=ulongLen||ushortLen!=ullongLen||ushortLen!=ntLen||ushortLen!=untLen||ushortLen!=nt2Len||ushortLen!=nt3Len||ushortLen!=unt3Len||ushortLen!=t1Len||ushortLen!=t2Len||ushortLen!=t3Len||ushortLen!=t4Len||ushortLen!=b1Len){
    FARF(ERROR, "=== Data Mismatch of sequence length===\n");
    return -1;
  }
  //validating inrout sequence contents
  int res1 = 0, res2 =0, res3 = 0, res4 = 0,res5 =0, res6 =0,res7=0,res8=0,res9 =0,resu1=0,resu2=0,resu3=0,resu4=0,res=0;
  for(int i =0;i<ushortLen;i++){
    res1 = res1+ushort[i];
    res2 = res2+llong[i];
    res3 = res3+ulong[i];
    res4 = res4+ullong[i];
    res5 = res5+nt[i];
    res6 = res6+unt[i];
    res7 = res7+nt2[i];
    res8 = res8+nt3[i];
    res9 = res9+unt3[i];
    resu1 = resu1+t1[i];
    resu2 = resu2+t2[i];
    resu3 = resu3+t3[i];
    resu4 = resu4+t4[i];
    res = res+b1[i];
  }
  if(res1!= 72|| res2!=72||res3!= 72||res4!=72||res5!= 72||res6!= 72||res7!= 72||res8!= 72||res9!= 72||resu1!= 72||resu2!= 72||resu3!= 72||resu4!=72 || res!=8){
    FARF(ERROR, "=== Data Mismatch ====\n");
    return -1;
  }
  //changing inrout sequence contents
  for(int i=0;i<ushortLen;i++){
    ushort[i] = llong[i] = ulong[i] = ullong[i] = nt[i] = unt[i] = nt2[i] = nt3[i] = unt3[i] = t1[i] = t2[i] = t3[i] = t4[i] = 10*(i);
    b1[i] = (bool)i;
  }
  return 0;
}

int buffer_argument_testcase_method6(remote_handle64 _h, const grade* g1, int g1Len, grade* g2, int g2Len, grade* g3, int g3Len){

  if(g1Len != g2Len|| g1Len!=g3Len){
    FARF(ERROR, "=== Data Mismatch of sequence length===\n");
    return -1;
  }
  //Validating in sequence contents
  for(int i=0;i<g1Len;i++){
    if(g1[i]!=i){
      FARF(ERROR, "=== Data Mismatch ===\n");
      return -1;
    }
  }
  for(int i=0;i<g2Len;i++){
    g2[i] = GOOD;
  }
  for(int i=0;i<g3Len;i++){
    g3[i] = g1[i];
  }
  return 0;
}