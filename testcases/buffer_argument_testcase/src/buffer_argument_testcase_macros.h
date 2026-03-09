//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#ifndef BUFFER_ARGUMENT_TESTCASE_MACROS_H
#define BUFFER_ARGUMENT_TESTCASE_MACROS_H

#define error(...) { printf("[ERROR]:%s:%d: ",__func__,__LINE__); \
    printf(__VA_ARGS__); }

#define MEMALLOC_FAILED(ret_value) do{ \
        ret_value = AEE_ENORPCMEMORY; \
        error("0x%x: memory allocation failed\n", ret_value); \
        goto bail; \
    }while(0)

#define MEM_FREE(ptr) do{ \
    if (ptr) \
        rpcmem_free(ptr); \
    }while(0)

#endif
