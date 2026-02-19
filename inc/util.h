#ifndef __HEXAGON_TEST_UTIL_H__
#define __HEXAGON_TEST_UTIL_H__

#include <stdbool.h>

#include "AEEStdDef.h"
#include "remote.h"

#ifdef __cplusplus
extern "C" {
#endif

int get_uri(int domain_id, char *module_uri, int module_uri_len, char **uri);
bool is_unsignedpd_supported(int domain_id);
int check_and_enable_unsigned_module_loading(int domain_id);
const char *get_domain_name(int domain_id);
int set_unsigned_module_loading(int domain_id, bool is_signedpd_requested);

#ifdef __cplusplus
}
#endif

#endif /* __HEXAGON_TEST_UTIL_H__ */
