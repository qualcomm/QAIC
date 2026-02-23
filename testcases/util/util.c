//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <errno.h>
#include <string.h>
#include <inttypes.h>
#include <unistd.h>

#include "AEEStdErr.h"
#include "remote.h"
#include "util.h"

const char *get_domain_name(int domain_id)
{
	switch (domain_id) {
	case ADSP_DOMAIN_ID:
		return ADSP_DOMAIN_NAME;
	case MDSP_DOMAIN_ID:
		return MDSP_DOMAIN_NAME;
	case SDSP_DOMAIN_ID:
		return SDSP_DOMAIN_NAME;
	case CDSP_DOMAIN_ID:
		return CDSP_DOMAIN_NAME;
	case CDSP1_DOMAIN_ID:
		return CDSP1_DOMAIN_NAME;
	default:
		break;
	}
	return "INVALID DSP NAME";
}

int get_uri(int domain_id, char *module_uri, int module_uri_len, char **uri)
{
	int uri_len;
	char *domain_uri;
	int err;

	switch (domain_id) {
	case ADSP_DOMAIN_ID:
		 domain_uri = ADSP_DOMAIN;
		break;
	case MDSP_DOMAIN_ID:
		 domain_uri = MDSP_DOMAIN;
		break;
	case SDSP_DOMAIN_ID:
		 domain_uri = SDSP_DOMAIN;
		break;
	case CDSP_DOMAIN_ID:
		 domain_uri = CDSP_DOMAIN;
		break;
	case CDSP1_DOMAIN_ID:
		 domain_uri = CDSP1_DOMAIN;
		break;
	default:
		return -EINVAL;
	}

	uri_len = module_uri_len + MAX_DOMAIN_URI_SIZE;
	*uri = (char *)malloc(uri_len);
	if (*uri == NULL) {
		printf("Unable to allocate memory for module %s uri\n", module_uri);
		return -ENOMEM;
	}

	err = snprintf(*uri, uri_len, "%s%s", module_uri, domain_uri);
	if (err < 0) {
		free(*uri);
		return err;
	}
	return 0;
}

bool is_unsignedpd_supported(int domain_id)
{
	struct remote_dsp_capability dsp_capability_domain = {domain_id, UNSIGNED_PD_SUPPORT, 0};
	int err;

	err = remote_handle_control(DSPRPC_GET_DSP_INFO, &dsp_capability_domain, sizeof(dsp_capability_domain));
	if (err) {
            printf("Test: Failed to get DSP Capability (%x)\n", err);
            return false;
        }

	if (dsp_capability_domain.capability == UNSIGNED_PD_SUPPORT)
            return true;

	return false;
}

int check_and_enable_unsigned_module_loading(int domain_id)
{
	struct remote_rpc_control_unsigned_module data;
	int err;

	if (!is_unsignedpd_supported(domain_id)) {
		printf("Error: Unsigned module loading is not supported for [%d] domain\n",
			       	domain_id);
		return -EINVAL;
	}

	data.domain = domain_id;
	data.enable = 1;

	if (&remote_session_control) {
        err = remote_session_control(DSPRPC_CONTROL_UNSIGNED_MODULE, (void *)&data, sizeof(data));
    } else {
        printf("Error: remote_session_control interface is not supported on this device\n");
        return AEE_EUNSUPPORTED;
    }
     if (err) {
         printf("Error, remote_session_control failed [%x]\n", err);
         return err;
     }
     return 0;
 }

int set_unsigned_module_loading(int domain_id, bool is_signedpd_requested)
{
    int nErr = 0;
    struct remote_rpc_control_unsigned_module data;
	data.domain = domain_id;
    data.enable = is_signedpd_requested ? 0 : 1;
	if (data.enable == 1) {
        // Request is to ENABLE unsigned PD
        if (!is_unsignedpd_supported(domain_id)) {
            printf("Error: Unsigned module loading is not supported for [%d] domain\n", domain_id);
            return -EINVAL;
        }
        if (&remote_session_control) {
            nErr = remote_session_control(DSPRPC_CONTROL_UNSIGNED_MODULE, (void*)&data, sizeof(data));
            if (AEE_SUCCESS != nErr) {
                printf("ERROR 0x%x: remote_session_control failed for domain %d\n", nErr, domain_id);
                return nErr;
            }
        } else {
            nErr = AEE_EUNSUPPORTED;
            printf("ERROR 0x%x: remote_session_control interface is not supported on this device\n", nErr);
            return nErr;
        }
    } else {
        // Request is to DISABLE unsigned PD (signed PD mode)
        // If control API is available, explicitly disable; if not, assume default is signed and continue.
        if (&remote_session_control) {
            nErr = remote_session_control(DSPRPC_CONTROL_UNSIGNED_MODULE, (void*)&data, sizeof(data));
            if (AEE_SUCCESS != nErr) {
                printf("ERROR 0x%x: remote_session_control failed for domain %d\n", nErr, domain_id);
                return nErr;
            }
        } else {
            // No remote_session_control — treat as success for signed PD
            nErr = 0;
        }
    }
	return 0;
}

