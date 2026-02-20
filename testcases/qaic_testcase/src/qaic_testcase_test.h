#ifndef QAIC_TESTCASE_TEST_H
#define QAIC_TESTCASE_TEST_H
/*==============================================================================
  Copyright (c) 2012-2014, 2020 Qualcomm Technologies, Inc.
  All rights reserved. Qualcomm Proprietary and Confidential.
==============================================================================*/

#include "AEEStdDef.h"
#include <stdbool.h>
#include "remote.h"

#ifdef __cplusplus
extern "C" {
#endif

int qaic_testcase_test(int domain, bool is_signedpd_requested);

#ifdef __cplusplus
}
#endif

#endif // QAIC_TESTCASE_TEST_H

