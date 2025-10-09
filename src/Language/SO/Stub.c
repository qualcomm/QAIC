// Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
// SPDX-License-Identifier: BSD-3-Clause-Clear

#ifndef INTERFACE_OBJECT
#define INTERFACE_OBJECT
static __inline uint32_t INTERFACE_Object_AddRef(void* pif) {
   return 1;
}
static __inline uint32_t INTERFACE_Object_Release(void* pif) {
   return 0;
}
#endif // INTERFACE_Object

static int INTERFACE_Object_IRemoteObject_QueryInterface(IRemoteObject* pif,  AEEIID iid, void** ppOut) {
   switch(iid) {
      case AEEIID_IQI:
      case AEEIID_IRemoteObject:
         *ppOut = pif;
         return AEE_SUCCESS;
   }
   return AEE_EUNSUPPORTED;
}

static int INTERFACE_Object_IRemoteObject_InvokeX(IRemoteObject* piro, uint32_t dwScalars, RemoteArg* pra) {
   return remote_handle_invoke(INTERFACE_handle, dwScalars, (remote_arg*)pra);
}

static const AEEVTBL(IRemoteObject) INTERFACE_vtIRemoteObject = {
    (uint32_t (*)(IRemoteObject *)) INTERFACE_Object_AddRef
   ,(uint32_t (*)(IRemoteObject *)) INTERFACE_Object_Release
   ,INTERFACE_Object_IRemoteObject_QueryInterface
   ,INTERFACE_Object_IRemoteObject_InvokeX
};

struct INTERFACE_IRemoteObject {
   const AEEVTBL(IRemoteObject)* pvt;
};
static const struct INTERFACE_IRemoteObject gINTERFACE_IRemoteObject = { &INTERFACE_vtIRemoteObject };

static __inline IRemoteObject* INTERFACE_IRemoteObject() {
   return (IRemoteObject*) & gINTERFACE_IRemoteObject;
}
