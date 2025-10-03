// Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
// SPDX-License-Identifier: BSD-3-Clause

#ifndef QAIC_WSTRING_H
#define QAIC_WSTRING_H
#include <stdint.h>
#ifndef _AECHAR_DEFINED
typedef uint16_t            AECHAR;
#define _AECHAR_DEFINED
#endif

_ATTRIBUTE_UNUSED
static __inline int _std_wstrlen(const AECHAR* s)
{
   const AECHAR *sEnd = s;

   if (! *sEnd)
      return 0;

   do {
      ++sEnd;
   } while (*sEnd);

   return sEnd - s;
}
#endif //QAIC_WSTRING_H
