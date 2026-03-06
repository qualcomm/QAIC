//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "HAP_farf.h"
#include "complex_seq.h"

int complex_seq_open(const char*uri, remote_handle64* handle) {
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
int complex_seq_close(remote_handle64 handle) {
   if (handle)
      free((void*)handle);
   return 0;
}


int complex_seq_sum(remote_handle64 _h, const int* vec, int vecLen, const float* vfloat, int vfloatLen, const char* vchar, int vcharLen, const _wchar_t* vwchar, int vwcharLen, const short* vshort, int vshortLen, const unsigned char* voctet, int voctetLen, const int64_t* vlonglong, int vlonglongLen, const unsigned short* vushort, int vushortLen, const uint64_t* vulonglong, int vulonglongLen, const int8_t* vint8t, int vint8tLen, const int16_t* vint16t, int vint16tLen, const int32_t* vint32t, int vint32tLen, const int64_t* vint64t, int vint64tLen, const double* vdouble, int vdoubleLen, const _cstring1_t* vstring, int vstringLen)
{
	int ii=0;
	int64_t res1 = 0;
	int64_t resf1 =0;
	int64_t resc1 = 0;
	int64_t reswc1 = 0;
	int64_t resshort1 = 0;
	int64_t resoctet1 = 0;
	int64_t reslonglong1 = 0;
	int64_t resushort1 = 0;
	int64_t resulonglong1 = 0;
	int64_t resint8t1 = 0;
	int64_t resint16t1 = 0;
	int64_t resint32t1 = 0;
	int64_t resint64t1 = 0;
	int64_t resd1 = 0;


	for (ii = 0; ii < (vecLen); ii = ii+4) {
		res1 = vec[ii] + vec[ii+1] + vec[ii+2] + vec[ii+3];
		resf1 = vfloat[ii] + vfloat[ii+1] + vfloat[ii+2] + vfloat[ii+3];
		resc1 = vchar[ii] + vchar[ii+1] + vchar[ii+2] + vchar[ii+3];
		reswc1 = vwchar[ii] + vwchar[ii+1] + vwchar[ii+2] + vwchar[ii+3];
		resshort1 = vshort[ii] + vshort[ii+1] + vshort[ii+2] + vshort[ii+3];
		resoctet1 = voctet[ii] + voctet[ii+1] + voctet[ii+2] + voctet[ii+3];
		reslonglong1 = vlonglong[ii] + vlonglong[ii+1] + vlonglong[ii+2] + vlonglong[ii+3];
		resushort1 = vushort[ii] + vushort[ii+1] + vushort[ii+2] + vushort[ii+3];
		resulonglong1 = vulonglong[ii] + vulonglong[ii+1] + vulonglong[ii+2] + vulonglong[ii+3];
		resint8t1 = vint8t[ii] + vint8t[ii+1] + vint8t[ii+2] + vint8t[ii+3];
		resint16t1 = vint16t[ii] + vint16t[ii+1] + vint16t[ii+2] + vint16t[ii+3];
		resint32t1 = vint32t[ii] + vint32t[ii+1] + vint32t[ii+2] + vint32t[ii+3];
		resint64t1 = vint64t[ii] + vint64t[ii+1] + vint64t[ii+2] + vint64t[ii+3];
		resd1 = vdouble[ii] + vdouble[ii+1] + vdouble[ii+2] + vdouble[ii+3];
		if(strcmp(vstring[ii].data,"Hexagon") != 0){
			FARF(ALWAYS, "===============String data Mismatched===============");
			goto bail;
		}
		if ((int)res1 != 10 || (int)resf1 != 10 || (int)resc1 != 10 || (int)reswc1 != 10 || (int)resshort1 != 10 || (int)resoctet1 != 10 || (int)reslonglong1 != 10 || (int)resushort1 != 10 || (int)resulonglong1 != 10 || (int)resint8t1 != 10 || (int)resint16t1 != 10 || (int)resint32t1 != 10 || (int)resint64t1 != 10 || (int)resd1 != 10 ) {
			FARF(RUNTIME_HIGH, "===============     Bit Mismatch- complex_seq_sum ii=%d, res=%ld ===============", ii,resf1);
			goto bail;
		}

		res1 = resf1 = resc1 = reswc1 = resshort1 = resoctet1 = reslonglong1 = resushort1 = resulonglong1 = resint8t1 = resint16t1 = resint32t1 = resint64t1 = resd1 = 0;
	}
	return 0;
	bail:
		return -1;
}

int complex_seq_diff(remote_handle64 _h, int* vec, int vecLen, float* vfloat, int vfloatLen, char* vchar, int vcharLen, _wchar_t* vwchar, int vwcharLen, short* vshort, int vshortLen, unsigned char* voctet, int voctetLen, int64_t* vlonglong, int vlonglongLen, unsigned short* vushort, int vushortLen, uint64_t* vulonglong, int vulonglongLen, int8_t* vint8t, int vint8tLen, int16_t* vint16t, int vint16tLen, int32_t* vint32t, int vint32tLen, int64_t* vint64t, int vint64tLen, double* vdouble, int vdoubleLen, _cstring1_t* vstring, int vstringLen) 
{
	int ii = 0;
	for (ii = 0; ii < vecLen; ++ii) {
		vdouble[ii] = vint64t[ii] = vint32t[ii] = vint16t[ii] = vint8t[ii] = vulonglong[ii] = vushort[ii] = vlonglong[ii] = voctet[ii] = vshort[ii] =  vwchar[ii] = vchar[ii] = vfloat[ii] = vec[ii] = (char)((ii%4)+10);
		strncpy(vstring[ii].data,"Octagon",9);
		vstring[ii].dataLen=8;
	}
    return 0;
}

int complex_seq_mult(remote_handle64 _h, const type_LIBS* libs, const complex_seq_type_LIB* lib, int64_t* res)
{
	int i=0;
	*res = 0;
	int64_t res1 = 0;
	int t = libs->test3Len;
	for (i = 0; i < t; i = i+4) {
		res1 = libs->test3[i] + libs->test3[i+1] + libs->test3[i+2] + libs->test3[i+3];
		if (res1 != 10) {
			FARF(RUNTIME_HIGH, "===============     Bit Mismatch- complex_seq_mult %ld %d ===============", res1,i);
			goto bail;
		}
		res1 = 0;
	}
	*res = libs->k + libs->l + libs->color1;
	*res = *res + lib->li + lib->ki + lib->mi;
	FARF(RUNTIME_HIGH, "=============== complex_seq_mult final :: %ld===============",*res);
	if(*res != 197)
		goto bail;
	return 0;
	bail:
		return -2;
}

int complex_seq_mult1(remote_handle64 _h, type_LIBS* libs, complex_seq_type_LIB* lib, int64_t* res)
{
	int i=0;
	int t =0;
	*res = 0;
	libs->k = 3.5;
	libs->l = 4.5;
	lib->ki = 40.25;
	lib->li = 40.75;
	lib->mi = 100;
	libs->color1 = RED;
	t = libs->test3Len;
	for(i=0;i<t;i++){
		libs->test3[i] = 1;
		*res = *res + libs->test3[i];
	}
	FARF(RUNTIME_HIGH, "=============== complex_seq_mult1 :: %ld===============",*res);
	*res = *res + libs->k + libs->l + libs->color1;
	*res = *res + lib->li + lib->ki + lib->mi;
	FARF(RUNTIME_HIGH, "=============== complex_seq_mult1 final :: %ld===============",*res);
	return 0;
}

int complex_seq_div(remote_handle64 _h, const complex_seq_type_LIBS1* libs, int libsLen, const complex_seq_type_LIB1* lib, int libLen, int64_t* res)
{
	int ii = 0 ;
	int i = 0;
	int t = 0;
	int64_t res1 = 0;
	for (ii=0 ; ii < libsLen ; ++ii ){
		t = libs[ii].test3Len;
		*res = 0;
		for (i = 0; i < t; i = i+4) {
			res1 = libs[ii].test3[i] + libs[ii].test3[i+1] + libs[ii].test3[i+2] + libs[ii].test3[i+3];
			if (res1 != 10) {
				FARF(RUNTIME_HIGH, "===============     Bit Mismatch- complex_seq_div %ld %d %d %d %d ===============", res1,libs[ii].test3[i+1],libs[ii].test3[i+2],libs[ii].test3[i+3],i);
				goto bail;
			}
			res1 = 0;
		}
		*res = libs[ii].k + libs[ii].l + libs[ii].color1;
		if (*res != 5) {
			FARF(RUNTIME_HIGH, "===============Input complex sequence failure %ld===============", *res);
			goto bail;
		}
	}

	for (ii=0 ; ii < libLen; ++ii ){
		*res = 0;
		*res = lib[ii].ki + lib[ii].li + lib[ii].mi;
		if (*res != 50) {
			FARF(RUNTIME_HIGH, "===============Input simple sequence failure %ld===============", *res);
			goto bail;
		}
	}
	return 0;
	bail:
		return -2;
}

int complex_seq_seq(remote_handle64 _h, complex_seq_type_LIBS1* libs, int libsLen, complex_seq_type_LIB1* lib, int libLen, int64_t* res3)
{
	int ii = 0 ;
	int i = 0;
	int64_t res1 = 0;
	int64_t res2 = 0;
	int64_t res = 0;
	*res3 = 0;
	int t = 0;
	for (ii=0 ; ii < libsLen ; ++ii ){
		libs[ii].k = 20;
		libs[ii].l = 30;
		libs[ii].color1 = RED;
		t = libs[ii].test3Len;
		for(i=0;i<t;i++){
			libs[ii].test3[i] = 1;
			res = res + libs[ii].test3[i];
		}
		res1 = libs[ii].k + libs[ii].l + libs[ii].color1;
	}
	for (ii=0 ; ii < libLen; ++ii ){
		lib[ii].ki = 10;
		lib[ii].li = 20;
		lib[ii].mi = 20;
		res2 = lib[ii].ki + lib[ii].li + lib[ii].mi;
	}
	*res3 = res1 + res + res2 ;
	FARF(RUNTIME_HIGH, "=============== output complex seq :: %ld %ld %ld %ld===============",res1,res,res2,*res3);
	if((res1 != 50) || (res != 80) || (res2 != 50))
		goto bail;
	return 0;
	bail:
		return -2;
}
