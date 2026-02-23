#ifndef QAIC_STRING_TEST_H
#define QAIC_STRING_TEST_H
//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include "AEEStdDef.h"
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

int qaic_string_test(int num, int domain_id, bool is_signedpd_requested);

#ifdef __cplusplus
}
#endif

#endif // QAIC_STRING_TEST_H