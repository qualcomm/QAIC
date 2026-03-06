//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "HAP_farf.h"
#include "array.h"

int array_open(const char*uri, remote_handle64* handle) {
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
int array_close(remote_handle64 handle) {
   if (handle)
      free((void*)handle);
   return 0;
}

int fact(int a){

  if(a == 0||a == 1)
    return 1;
  else
    return a*fact(a-1);
}

int array_sum(remote_handle64 h, const alpha* vec, int64_t* res, int resLen)
{
  FARF(ALWAYS, "reached imp execution");
  if(resLen!=29){
    FARF(ERROR, "=== Data Mismatch ===\n");
    return -1;
  }
  for(int i=0;i<10;i++){

    if((vec->number)[i]!=i || (int)(vec->num2)[i]!=i || (int)(vec->num3)[i]!=i ||(int)(vec->num4)[i]!=i ||(vec->num5)[i]!=i ||(vec->num6)[i]!=i ||(vec->num7)[i]!=i ||(vec->num8)[i]!=i ||(vec->num9)[i]!=i ||(vec->arr)[i]!=i ||(vec->arr1)[i]!=i ||(vec->arr2)[i]!=i ||(vec->arr3)[i]!=i ||(vec->arr4)[i]!=i ||(vec->arr5)[i]!=i ||(vec->arr6)[i]!=i ||(vec->arr7)[i]!=i ||(vec->arr8)[i]!=i ||(vec->arr9)[i]!=i ||(vec->var)[i]!=i ||(vec->var1)[i]!=i ||(vec->var2)[i]!=i ||(vec->var3)[i]!=i ||(vec->var4)[i]!=i ||(vec->var5)[i]!=i ||(vec->var6)[i]!=i ||(vec->var7)[i]!=i ||(vec->var8)[i]!=i%2 || (vec->enum1)[i]!=i%3){
      FARF(ERROR, "=== Data Mismatch ===\n");
      return -1;
    }
  }
  for(int i=0;i<resLen;i++){
    res[i] = 0;
  }
  for(int i=0;i<10;i++){
    res[0] = res[0] + (vec->number)[i];
    res[1] = res[1] + (int)(vec->num2)[i];
    res[2] = res[2] + (int)(vec->num3)[i];
    res[3] = res[3] + (int)(vec->num4)[i];
    res[4] = res[4] + (vec->num5)[i];
    res[5] = res[5] + (vec->num6)[i];
    res[6] = res[6] + (vec->num7)[i];
    res[7] = res[7] + (vec->num8)[i];
    res[8] = res[8] + (vec->num9)[i];
    res[9] = res[9] + (vec->arr)[i];
    res[10] = res[10] + (vec->arr1)[i];
    res[11] = res[11] + (vec->arr2)[i];
    res[12] = res[12] + (vec->arr3)[i];
    res[13] = res[13] + (vec->arr4)[i];
    res[14] = res[14] + (vec->arr5)[i];
    res[15] = res[15] + (vec->arr6)[i];
    res[16] = res[16] + (vec->arr7)[i];
    res[17] = res[17] + (vec->arr8)[i];
    res[18] = res[18] + (vec->arr9)[i];
    res[19] = res[19] + (vec->var)[i];
    res[20] = res[20] + (vec->var1)[i];
    res[21] = res[21] + (vec->var2)[i];
    res[22] = res[22] + (vec->var3)[i];
    res[23] = res[23] + (vec->var4)[i];
    res[24] = res[24] + (vec->var5)[i];
    res[25] = res[25] + (vec->var6)[i];
    res[26] = res[26] + (vec->var7)[i];
    res[27] = res[27] + (vec->var8)[i];
    res[28] = res[28] + (vec->enum1)[i];
  }
  return 0;
}

int array_method1(remote_handle64 _h, const beta* struc1, beta* struc2, beta* struc3){

  for(int i=0;i<5;i++){
    if(struc1->a.num[i]!=i || struc3->a.num[i] != i){
      FARF(ERROR, "=== Data Mismatch ===\n");
      return -1;
    }
  }
  for(int i=0;i<5;i++){
    struc2->a.num[i] = fact(struc1->a.num[i]);
    struc3->a.num[i] = struc3->a.num[i] * struc3->a.num[i];
  }
  return 0;
}

int array_method3(remote_handle64 _h, const beta* seq1, int seq1Len, beta* seq2, int seq2Len, beta* seq3, int seq3Len){

  if(seq1Len != seq2Len || seq1Len != seq3Len){
    FARF(ERROR, "=== Data Mismatch ===\n");
    return -1;
  }
  for(int i =0 ;i<seq1Len;i++){
    for(int j=0;j<5;j++){
      if(seq1[i].a.num[j] != i+j || seq3[i].a.num[j] != i+j){
        FARF(ERROR, "=== Data Mismatch ===\n");
        return -1;
      }
    }
  }
  for(int i =0;i<seq2Len;i++){
    for(int j=0;j<5;j++){
      seq2[i].a.num[j] = seq1[i].a.num[j] * seq1[i].a.num[j];
      seq3[i].a.num[j] = fact(seq3[i].a.num[j]);
    }
  }
  return 0;
}

int array_method2(remote_handle64 _h, const alpha* seq1, int seq1Len, alpha* seq2, int seq2Len, alpha* seq3, int seq3Len){

  if(seq1Len != seq2Len || seq3Len != seq1Len){
    FARF(ERROR, "=== Data Mismatch ===\n");
    return -1;
  }
  int sum1 = 0, sum2 = 0;
  for(int i =0 ;i<seq1Len;i++){
    for(int j=0;j<10;j++){
      sum1 = sum1 +seq1[i].number[j] + (int)seq1[i].num2[j] + (int)seq1[i].num3[j] + (int)seq1[i].num4[j] + seq1[i].num5[j] + seq1[i].num6[j] + seq1[i].num7[j] + seq1[i].num8[j] + seq1[i].num9[j] + seq1[i].arr[j] + seq1[i].arr1[j] + seq1[i].arr2[j] + seq1[i].arr3[j] + seq1[i].arr4[j]+ seq1[i].arr5[j]+ seq1[i].arr6[j]+ seq1[i].arr7[j]+ seq1[i].arr8[j]+ seq1[i].arr9[j] + seq1[i].var[j]+ seq1[i].var1[j]+ seq1[i].var2[j]+ seq1[i].var3[j]+ seq1[i].var4[j]+ seq1[i].var5[j]+ seq1[i].var6[j]+ seq1[i].var7[j]+ seq1[i].var8[j] + seq1[i].enum1[j];
      sum2 = sum2 +seq3[i].number[j] + (int)seq3[i].num2[j] + (int)seq3[i].num3[j] + (int)seq3[i].num4[j] + seq3[i].num5[j] + seq3[i].num6[j] + seq3[i].num7[j] + seq3[i].num8[j] + seq3[i].num9[j] + seq3[i].arr[j] + seq3[i].arr1[j] + seq3[i].arr2[j] + seq3[i].arr3[j] + seq3[i].arr4[j]+ seq3[i].arr5[j]+ seq3[i].arr6[j]+ seq3[i].arr7[j]+ seq3[i].arr8[j]+ seq3[i].arr9[j] + seq3[i].var[j]+ seq3[i].var1[j]+ seq3[i].var2[j]+ seq3[i].var3[j]+ seq3[i].var4[j]+ seq3[i].var5[j]+ seq3[i].var6[j]+ seq3[i].var7[j]+ seq3[i].var8[j] + seq3[i].enum1[j];
    }
  }
  if(sum1 != 2750 || sum2 != 2750){
    FARF(ERROR, "=== Data Mismatch ===\n");
    return -1;
  }

  for(int j =0;j<seq2Len;j++){
    for(int i=0;i<10;i++){
      (seq2[j].num2)[i] =(wchar_t) (i+j);
      (seq2[j].num3)[i] =(unsigned char) (i+j);
      (seq2[j].num4)[i] = (char) (i+j);
      (seq2[j].number)[i] = (seq2[j].num5)[i] = (seq2[j].num6)[i] = (seq2[j].num7)[i]= (seq2[j].num8)[i] = (seq2[j].num9)[i] = (seq2[j].arr)[i] = (seq2[j].arr1)[i] = (seq2[j].arr2)[i] = (seq2[j].arr3)[i] = (seq2[j].arr4)[i] = (seq2[j].arr5)[i] = (seq2[j].arr6)[i] = (seq2[j].arr7)[i] = (seq2[j].arr8)[i] = (seq2[j].arr9)[i] = (seq2[j].var)[i] = (seq2[j].var1)[i] = (seq2[j].var2)[i] = (seq2[j].var3)[i] = (seq2[j].var4)[i] = (seq2[j].var5)[i] = (seq2[j].var6)[i] = (seq2[j].var7)[i] = fact((i+j)%3);
      (seq2[j].var8)[i] = (i+j)%2;
      seq2[j].enum1[i] = GREY;
      (seq3[j].num2)[i] =(wchar_t) (seq3[j].num2[i]);
      (seq3[j].num3)[i] =(unsigned char) (seq3[j].num3[i]);
      (seq3[j].num4)[i] = (char) (seq3[j].num4[i]);
      (seq3[j].number)[i] = (seq3[j].num5)[i] = (seq3[j].num6)[i] = (seq3[j].num7)[i]= (seq3[j].num8)[i] = (seq3[j].num9)[i] = (seq3[j].arr)[i] = (seq3[j].arr1)[i] = (seq3[j].arr2)[i] = (seq3[j].arr3)[i] = (seq3[j].arr4)[i] = (seq3[j].arr5)[i] = (seq3[j].arr6)[i] = (seq3[j].arr7)[i] = (seq3[j].arr8)[i] = (seq3[j].arr9)[i] = (seq3[j].var)[i] = (seq3[j].var1)[i] = (seq3[j].var2)[i] = (seq3[j].var3)[i] = (seq3[j].var4)[i] = (seq3[j].var5)[i] = (seq3[j].var6)[i] = (seq3[j].var7)[i] = fact((seq3[j].number[i])%3);
      (seq3[j].var8)[i] = (seq3[j].var8[i])%2;
      seq3[j].enum1[i] = DARK;
    }
  }
  return 0;
}

int array_sum2(remote_handle64 _h, alpha* vec1, alpha* vec2){

  for(int i=0;i<10;i++){

    if((vec2->number)[i]!=i || (int)(vec2->num2)[i]!=i || (int)(vec2->num3)[i]!=i ||(int)(vec2->num4)[i]!=i ||(vec2->num5)[i]!=i ||(vec2->num6)[i]!=i ||(vec2->num7)[i]!=i ||(vec2->num8)[i]!=i ||(vec2->num9)[i]!=i ||(vec2->arr)[i]!=i ||(vec2->arr1)[i]!=i ||(vec2->arr2)[i]!=i ||(vec2->arr3)[i]!=i ||(vec2->arr4)[i]!=i ||(vec2->arr5)[i]!=i ||(vec2->arr6)[i]!=i ||(vec2->arr7)[i]!=i ||(vec2->arr8)[i]!=i ||(vec2->arr9)[i]!=i ||(vec2->var)[i]!=i ||(vec2->var1)[i]!=i ||(vec2->var2)[i]!=i ||(vec2->var3)[i]!=i ||(vec2->var4)[i]!=i ||(vec2->var5)[i]!=i ||(vec2->var6)[i]!=i ||(vec2->var7)[i]!=i ||(vec2->var8)[i]!=i%2 || (vec2->enum1)[i]!=i%3){
      FARF(ERROR, "=== Data Mismatch ===\n");
      return -1;
    }
  }

  for(int i=0;i<10;i++){
    (vec1->num2)[i] =(wchar_t) i;
    (vec1->num3)[i] =(unsigned char) i;
    (vec1->num4)[i] = (char) i;
    (vec1->number)[i] = (vec1->num5)[i] = (vec1->num6)[i] = (vec1->num7)[i]= (vec1->num8)[i] = (vec1->num9)[i] = (vec1->arr)[i] = (vec1->arr1)[i] = (vec1->arr2)[i] = (vec1->arr3)[i] = (vec1->arr4)[i] = (vec1->arr5)[i] = (vec1->arr6)[i] = (vec1->arr7)[i] = (vec1->arr8)[i] = (vec1->arr9)[i] = (vec1->var)[i] = (vec1->var1)[i] = (vec1->var2)[i] = (vec1->var3)[i] = (vec1->var4)[i] = (vec1->var5)[i] = (vec1->var6)[i] = (vec1->var7)[i] = fact(i%3);
    (vec1->var8)[i] = i%2;
  }
  vec1->enum1[0] = vec1->enum1[3] = vec1->enum1[6] = vec1->enum1[9] = WHITE;
  vec1->enum1[1] = vec1->enum1[4] = vec1->enum1[7] = GREY;
  vec1->enum1[2] = vec1->enum1[5] = vec1->enum1[8] = DARK;
  for(int i=0;i<10;i++){
    (vec2->num2)[i] =(wchar_t) ((int) (vec2->num2)[i]+1);
    (vec2->num3)[i] =(unsigned char) ((int)((vec2->num3)[i]+1));
    (vec2->num4)[i] = (char) ((int) (vec2->num2)[i]+1);;
    (vec2->number)[i] = (vec2->num5)[i] = (vec2->num6)[i] = (vec2->num7)[i]= (vec2->num8)[i] = (vec2->num9)[i] = (vec2->arr)[i] = (vec2->arr1)[i] = (vec2->arr2)[i] = (vec2->arr3)[i] = (vec2->arr4)[i] = (vec2->arr5)[i] = (vec2->arr6)[i] = (vec2->arr7)[i] = (vec2->arr8)[i] = (vec2->arr9)[i] = (vec2->var)[i] = (vec2->var1)[i] = (vec2->var2)[i] = (vec2->var3)[i] = (vec2->var4)[i] = (vec2->var5)[i] = (vec2->var6)[i] = (vec2->var7)[i] = fact((vec2->number)[i]%3);
    (vec2->var8)[i] = i%2;
  }
  vec2->enum1[0] = vec2->enum1[3] = vec2->enum1[6] = vec2->enum1[9] = GREY;
  vec2->enum1[1] = vec2->enum1[4] = vec2->enum1[7] = DARK;
  vec2->enum1[2] = vec2->enum1[5] = vec2->enum1[8] = WHITE;
  return 0;
}