// Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
// SPDX-License-Identifier: BSD-3-Clause

#include <stdlib.h>

#ifndef INTERFACE_OBJECT
#define INTERFACE_OBJECT
static __inline uint32_t INTERFACE_Object_AddRef(void* pif) {
   return 1;
}
static __inline uint32_t INTERFACE_Object_Release(void* pif) {
   return 0;
}
#endif // INTERFACE_Object

typedef int (*Function)(void*,...);

#define SO_OBJECT_START(iface) \
   struct iface##_Vtbl {\
      const AEEVTBL(IQI)* pvt; \
   };\
   static const Function vt##iface[] = {\
       (Function) iface##_Object_AddRef\
      ,(Function) iface##_Object_Release\

#define SO_OBJECT_END(iface, pslim) \
   };\
   static const struct iface##_Vtbl g##iface##_Vtbl = { (const AEEVTBL(IQI)*) & vt##iface };\
   static __inline IQI* iface##_IQI() {\
      return (IQI*) & g##iface##_Vtbl;\
   }\
   int iface##_skel_invoke(uint32_t uScalars, RemoteArg* pra) {\
      return slim_skel_method_invoke(SO_IENV, iface##_IQI(), pslim, uScalars, pra); \
   }


