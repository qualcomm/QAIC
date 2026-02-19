/*==============================================================================
  Copyright (c) 2012-2014,2017,2020, 2022 Qualcomm Technologies, Inc.
  All rights reserved. Qualcomm Proprietary and Confidential.
==============================================================================*/
#include <stdio.h>
#include <stdbool.h>
#include "buffer_argument_testcase_macros.h"
#include "buffer_argument_testcase.h"
#include "remote.h"
#include "rpcmem.h"

int heapid = RPCMEM_HEAP_ID_SYSTEM;
#if defined(SLPI) || defined(MDSP)
  heapid = RPCMEM_HEAP_ID_CONTIG;
#endif

int testcase1(remote_handle64 handle){
  int nErr = AEE_SUCCESS;
  alpha *a =NULL, *b = NULL, *c = NULL;
  if (0 == (a = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)))) {
    MEMALLOC_FAILED(nErr);
  }
  if (0 == (b = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)))) {
   MEMALLOC_FAILED(nErr);
  }
  if (0 == (c = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)))) {
    MEMALLOC_FAILED(nErr);
  }
  c->arg1 = a->arg1= (wchar_t)97;
  c->arg2 = a->arg2= (char) 1;
  c->arg3 = a->arg3= (char) 100;
  c->arg4 = a->arg4= 4;
  c->arg5 = a->arg5= 5;
  c->arg6 = a->arg6= 6;
  c->arg7 = a->arg7= -7;
  c->arg8 = a->arg8= 8;
  c->arg9 = a->arg9= 9;
  c->num1 = a->num1= -1;
  c->num2 = a->num2= 2;
  c->num3 = a->num3= -3;
  c->num4 = a->num4= 4;
  c->num5 = a->num5= -5;
  c->num6 = a->num6= 6;
  c->num7 = a->num7= -7;
  c->num8 = a->num8= 8;
  c->num9 = a->num9= 4;
  c->var1 = a->var1= 11;
  c->var2 = a->var2= 22;
  c->var3 = a->var3= 33;
  c->var4 = a->var4= 44;
  c->var5 = a->var5= 55;
  c->var6 = a->var6= 66;
  c->var7 = a->var7= 77;
  c->var8 = a->var8= 88.788;
  c->var9 = a->var9= 99.9999;
  c->b1 = a->b1 = false;

  nErr = buffer_argument_testcase_method1(handle, a, b, c);
  if(nErr)
    goto bail;
  else{
    if(b->arg1!='b'||(int)b->arg2!= 1|| b->arg3!='e'||b->arg4!= 24||b->arg5!= 120||b->arg6!= 720||b->arg7!= -70|| b->arg8!= 40320||b->arg9!= 362880||b->num1!= -10||b->num2!= 2||b->num3!= -30||b->num4!= 24||b->num5!= -50||b->num6!= 720||b->num7!= -1||b->num8!= 80||b->num9!= 24||b->var1!= 110||b->var2!= 220||b->var3!= 330||b->var4!= 440||b->var5!= 550||b->var6!=660||b->var7!= 770||b->var8!= 887.880005f||b->var9!= 999.999000||b->b1)
      printf("==== ERROR: Data Mismatch for rout param ====\n");

    if(c->arg1!='b'||(int)c->arg2!= 1|| c->arg3!='e'||c->arg4!= 24||c->arg5!= 120||c->arg6!= 720||c->arg7!= -70|| c->arg8!= 40320||c->arg9!= 362880||c->num1!= -10||c->num2!= 2||c->num3!= -30||c->num4!= 24||c->num5!= -50||c->num6!= 720||c->num7!= -1||c->num8!= 80||c->num9!= 24||c->var1!= 110||c->var2!= 220||c->var3!= 330||c->var4!= 440||c->var5!= 550||c->var6!=660||c->var7!= 770||c->var8!= 887.880005f||c->var9!= 999.999000||!(c->b1))
      printf("==== ERROR: Data Mismatch for inrout param ====\n");
  }

  bail:
    MEM_FREE(a);
    MEM_FREE(b);
    MEM_FREE(c);
  return nErr;
}

int testcase2(remote_handle64 handle){

  int nErr = AEE_SUCCESS, seqLen = 2, seq1Len = 2, seq2Len = 2;
  alpha *seq =NULL, *seq1 = NULL, *seq2 = NULL;
  if (0 == (seq = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)*seqLen))) {
    MEMALLOC_FAILED(nErr);
  }
  if (0 == (seq1 = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)*seq1Len))) {
   MEMALLOC_FAILED(nErr);
  }
  if (0 == (seq2 = (alpha*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(alpha)*seq2Len))) {
    MEMALLOC_FAILED(nErr);
  }
  for(int i =0; i<seqLen;i++){
    seq[i].arg1 = seq2[i].arg1 = (wchar_t)97+i;
    seq[i].arg2 = seq2[i].arg2 = (char) 65+i;
    seq[i].arg3 = seq2[i].arg3 = (char) 100+i;
    seq[i].arg4 = seq2[i].arg4 = 4+i;
    seq[i].arg5 = seq2[i].arg5 = 5+i;
    seq[i].arg6 = seq2[i].arg6 = 6+i;
    seq[i].arg7 = seq2[i].arg7 = -7+i;
    seq[i].arg8 = seq2[i].arg8 = 1+i;
    seq[i].arg9 = seq2[i].arg9 = 9+i;
    seq[i].num1 = seq2[i].num1 = -1+i;
    seq[i].num2 = seq2[i].num2 = 2+i;
    seq[i].num3 = seq2[i].num3 = -3+i;
    seq[i].num4 = seq2[i].num4 = 4+i;
    seq[i].num5 = seq2[i].num5 = -5+i;
    seq[i].num6 = seq2[i].num6 = 6+i;
    seq[i].num7 = seq2[i].num7 = -7+i;
    seq[i].num8 = seq2[i].num8 = 8+i;
    seq[i].num9 = seq2[i].num9 = 4+i;
    seq[i].var1 = seq2[i].var1 = 11+i;
    seq[i].var2 = seq2[i].var2 = 22+i;
    seq[i].var3 = seq2[i].var3 = 33+i;
    seq[i].var4 = seq2[i].var4 = 44+i;
    seq[i].var5 = seq2[i].var5 = 55+i;
    seq[i].var6 = seq2[i].var6 = 66+i;
    seq[i].var7 = seq2[i].var7 = 77+i;
    seq[i].var8 = seq2[i].var8 = 88.788+i;
    seq[i].var9 = seq2[i].var9 = 99.9999+i;
    seq[i].b1 = seq2[i].b1 = false;
  }
  nErr = buffer_argument_testcase_method2(handle, seq, seqLen, seq1, seq1Len, seq2, seq2Len);
  if(nErr)
    goto bail;
  else{
    int j=0;
    if(seq1[j].arg1!='b'||(char)seq1[j].arg2!= 'A'|| seq1[j].arg3!='d'||seq1[j].arg4!= 24||seq1[j].arg5!= 120||seq1[j].arg6!= 720||seq1[j].arg7!= -7|| seq1[j].arg8!= 1||seq1[j].arg9!= 362880||seq1[j].num1!= -1||seq1[j].num2!= 2||seq1[j].num3!= -3||seq1[j].num4!= 4||seq1[j].num5!= -5||seq1[j].num6!= 6||seq1[j].num7!= -7||seq1[j].num8!= 8||seq1[j].num9!= 4||seq1[j].var1!= 11||seq1[j].var2!= 22||seq1[j].var3!= 33||seq1[j].var4!= 44||seq1[j].var5!= 55||seq1[j].var6!=66||seq1[j].var7!= 77||seq1[j].var8!= 88.7880005f||seq1[j].var9!= 99.9999000||!(seq1[j].b1))
        printf("==== ERROR: Data Mismatch for rout param for %d member of sequence ====\n", j+1);

    if(seq2[j].arg1!='b'||(char)seq2[j].arg2!= 'D'|| seq2[j].arg3!='e'||seq2[j].arg4!= 24||seq2[j].arg5!= 120||seq2[j].arg6!= 720||seq2[j].arg7!= -6|| seq2[j].arg8!= 2||seq2[j].arg9!= 10||seq2[j].num1!= 0||seq2[j].num2!= 3||seq2[j].num3!= -2||seq2[j].num4!= 5||seq2[j].num5!= -4||seq2[j].num6!= 7||seq2[j].num7!= -6||seq2[j].num8!= 9||seq2[j].num9!= 5||seq2[j].var1!= 12||seq2[j].var2!= 23||seq2[j].var3!= 34||seq2[j].var4!= 45||seq2[j].var5!= 56||seq2[j].var6!=67||seq2[j].var7!= 78||seq2[j].var8!= 89.7880005f||seq2[j].var9!= 100.9999000||!(seq2[j].b1))
        printf("==== ERROR: Data Mismatch for inrout param for %d member of sequence ====\n", j+1);

    j++;
    if(seq1[j].arg1!='c'||(char)seq1[j].arg2!= 'B'|| seq1[j].arg3!='e'||seq1[j].arg4!= 120||seq1[j].arg5!= 720||seq1[j].arg6!= 5040||seq1[j].arg7!= -6|| seq1[j].arg8!= 2||seq1[j].arg9!= 3628800||seq1[j].num1!= 0||seq1[j].num2!= 3||seq1[j].num3!= -2||seq1[j].num4!= 5||seq1[j].num5!= -4||seq1[j].num6!= 7||seq1[j].num7!= -6||seq1[j].num8!= 9||seq1[j].num9!= 5||seq1[j].var1!= 12||seq1[j].var2!= 23||seq1[j].var3!= 34||seq1[j].var4!= 45||seq1[j].var5!= 56||seq1[j].var6!=67||seq1[j].var7!= 78||seq1[j].var8!= 89.7880005f||seq1[j].var9!= 100.9999000||!(seq1[j].b1))
        printf("==== ERROR: Data Mismatch for rout param for %d member of sequence ====\n", j+1);

    if(seq2[j].arg1!='c'||(char)seq2[j].arg2!= 'D'|| seq2[j].arg3!='f'||seq2[j].arg4!= 120||seq2[j].arg5!= 720||seq2[j].arg6!= 5040||seq2[j].arg7!= -5|| seq2[j].arg8!= 3||seq2[j].arg9!= 11||seq2[j].num1!= 1||seq2[j].num2!= 4||seq2[j].num3!= -1||seq2[j].num4!= 6||seq2[j].num5!= -3||seq2[j].num6!= 8||seq2[j].num7!= -5||seq2[j].num8!= 10||seq2[j].num9!= 6||seq2[j].var1!= 13||seq2[j].var2!= 24||seq2[j].var3!= 35||seq2[j].var4!= 46||seq2[j].var5!= 57||seq2[j].var6!=68||seq2[j].var7!= 79||seq2[j].var8!= 90.7880005f||seq2[j].var9!= 101.9999000||!(seq2[j].b1))
        printf("==== ERROR: Data Mismatch for inrout param for %d member of sequence ====\n", j+1);
  }

  bail:
    MEM_FREE(seq1);
    MEM_FREE(seq2);
    MEM_FREE(seq);
  return nErr;
}

int testcase3(remote_handle64 handle){

  int nErr = AEE_SUCCESS, seqLen = 8;
  unsigned short *ushort = NULL;
  int64_t *llong = NULL;
  unsigned int *ulong = NULL;
  uint64_t *ullong = NULL;
  signed char *nt = NULL;
  unsigned char *unt = NULL;
  signed short *nt2 = NULL;
  int32_t *nt3 = NULL;
  uint32_t *unt2 = NULL;
  uint8_t *ut1 = NULL;
  uint16_t *ut2 = NULL;
  uint32_t *ut3 = NULL;
  uint64_t *ut4 = NULL;
  bool *loob = NULL;
  if (0 == ((ushort = (unsigned short*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(unsigned short)*seqLen))&&
            (llong = (int64_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(int64_t)*seqLen)) &&
            (ulong = (unsigned int*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(unsigned int)*seqLen))&&
            (ullong = (uint64_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(uint64_t)*seqLen))&&
            (nt = (signed char*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(signed char)*seqLen))&&
            (unt = (unsigned char*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(unsigned char)*seqLen))&&
            (nt2 = (signed short*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(signed short)*seqLen))&&
            (nt3 = (int32_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(int32_t)*seqLen))&&
            (unt2 = (uint32_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(uint32_t)*seqLen))&&
            (ut1 = (uint8_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(uint8_t)*seqLen))&&
            (ut2 = (uint16_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(uint16_t)*seqLen))&&
            (ut3 = (uint32_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(uint32_t)*seqLen))&&
            (ut4 = (uint64_t*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(uint64_t)*seqLen))&&
            (loob = (bool*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(bool)*seqLen)))){
    MEMALLOC_FAILED(nErr);
  }
  for(int i=0;i<seqLen;i++){
    ushort[i] = llong[i] = ulong[i] = ullong[i] = nt[i] = unt[i] = nt2[i] = nt3[i] = unt2[i] = ut1[i] = ut2[i] = ut3[i] = ut4[i] = i+1;
    loob[i] = (bool) i;
  }
  printf("Call method3 on the DSP\n");
  nErr = buffer_argument_testcase_method3(handle, ushort,seqLen, llong,seqLen, ulong,seqLen, ullong,seqLen, nt,seqLen, unt,seqLen, nt2,seqLen, nt3,seqLen, unt2,seqLen, ut1,seqLen, ut2,seqLen, ut3,seqLen, ut4,seqLen, loob,seqLen);
  if(nErr){
    goto bail;
  }
  else{
    printf("Completion of method3 on the DSP\n");
  }
  printf("Call method4 on the DSP\n");
  nErr = buffer_argument_testcase_method4(handle, ushort,seqLen, llong,seqLen, ulong,seqLen, ullong,seqLen, nt,seqLen, unt,seqLen, nt2,seqLen, nt3,seqLen, unt2,seqLen, ut1,seqLen, ut2,seqLen, ut3,seqLen, ut4,seqLen, loob,seqLen);
  if(nErr){
    goto bail;
  }
  else{
    int res1 = 0, res2 =0, res3 = 0, res4 = 0,res5 =0, res6 =0,res7=0,res8=0,res9 =0,resu1=0,resu2=0,resu3=0,resu4=0,res=0;
    for(int i =0;i<seqLen;i++){
      res1 = res1+ushort[i];
      res2 = res2+llong[i];
      res3 = res3+ulong[i];
      res4 = res4+ullong[i];
      res5 = res5+nt[i];
      res6 = res6+unt[i];
      res7 = res7+nt2[i];
      res8 = res8+nt3[i];
      res9 = res9+unt2[i];
      resu1 = resu1+ut1[i];
      resu2 = resu2+ut2[i];
      resu3 = resu3+ut3[i];
      resu4 = resu4+ut4[i];
      res = res+loob[i];
    }
    if(res1!= 72|| res2!=72||res3!= 72||res4!=72||res5!= 72||res6!= 72||res7!= 72||res8!= 72||res9!= 72||resu1!= 72||resu2!= 72||resu3!= 72||resu4!=72 || res!=8)
      printf("=== Data Mismatch ====\n");
    else
      printf("Completion of method4 on the DSP\n");
  }
  printf("Call method5 on the DSP\n");
  nErr = buffer_argument_testcase_method5(handle, ushort,seqLen, llong,seqLen, ulong,seqLen, ullong,seqLen, nt,seqLen, unt,seqLen, nt2,seqLen, nt3,seqLen, unt2,seqLen, ut1,seqLen, ut2,seqLen, ut3,seqLen, ut4,seqLen, loob,seqLen);
  if(nErr){
    goto bail;
  }
  else{
    int res1 = 0, res2 =0, res3 = 0, res4 = 0,res5 =0, res6 =0,res7=0,res8=0,res9 =0,resu1=0,resu2=0,resu3=0,resu4=0,res=0;
    for(int i =0;i<seqLen;i++){
      res1 = res1+ushort[i];
      res2 = res2+llong[i];
      res3 = res3+ulong[i];
      res4 = res4+ullong[i];
      res5 = res5+nt[i];
      res6 = res6+unt[i];
      res7 = res7+nt2[i];
      res8 = res8+nt3[i];
      res9 = res9+unt2[i];
      resu1 = resu1+ut1[i];
      resu2 = resu2+ut2[i];
      resu3 = resu3+ut3[i];
      resu4 = resu4+ut4[i];
      res = res+loob[i];
    }
    if(res1!= 280|| res2!=280||res3!= 280||res4!=280||res5!= 280||res6!= 280||res7!= 280||res8!= 280||res9!= 280||resu1!= 280||resu2!= 280||resu3!= 280||resu4!=280 || res!=7)
      printf("=== Data Mismatch ====\n");
    else
      printf("Completion of method5 on the DSP\n");
  }
  bail:
    MEM_FREE(ushort);
    MEM_FREE(llong);
    MEM_FREE(ulong);
    MEM_FREE(ullong);
    MEM_FREE(nt);
    MEM_FREE(unt);
    MEM_FREE(nt2);
    MEM_FREE(nt3);
    MEM_FREE(unt2);
    MEM_FREE(ut1);
    MEM_FREE(ut2);
    MEM_FREE(ut3);
    MEM_FREE(ut4);
    MEM_FREE(loob);
  return nErr;
}

int testcase4(remote_handle64 handle){

  grade *g1 = NULL, *g2 = NULL, *g3 = NULL;
  int g1Len = 3, g2Len = 3, g3Len = 3, nErr = AEE_SUCCESS;
  if (0 == (g1 = (grade*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(grade)*g1Len))) {
    MEMALLOC_FAILED(nErr);
  }
  if (0 == (g2 = (grade*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(grade)*g2Len))) {
    MEMALLOC_FAILED(nErr);
  }
  if (0 == (g3 = (grade*)rpcmem_alloc(heapid, RPCMEM_DEFAULT_FLAGS, sizeof(grade)*g3Len))) {
    MEMALLOC_FAILED(nErr);
  }
  for(int i=0;i<g1Len;i++)
    g1[i] = i;
  printf("Call method6 on the DSP\n");
  nErr = buffer_argument_testcase_method6(handle, g1, g1Len, g2, g2Len, g3, g3Len);
  if(nErr){
    goto bail;
  }
  else{
    int k=0;
    for(int i=0;i<g2Len;i++){
      if(g2[i]!=2 || g3[i]!=g1[i])
        k=1;
    }
    if(k)
      printf("=== Data Mismatch ===\n");
    else
      printf("Completion of method6 on the DSP\n");
  }
  bail:
    MEM_FREE(g1);
    MEM_FREE(g2);
    MEM_FREE(g3);
  return nErr;
}