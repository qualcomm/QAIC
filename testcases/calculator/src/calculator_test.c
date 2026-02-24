//% Copyright (c) Qualcomm Technologies, Inc. and/or its subsidiaries.
//% SPDX-License-Identifier: BSD-3-Clause-Clear

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <inttypes.h>
#include <unistd.h>

#include "AEEStdErr.h"
#include "calculator.h"
#include "rpcmem.h"
#include "remote.h"
#include "util.h"

int calculator_test(int domain_id, int nums) {
	remote_handle64 handle;
	char *uri = NULL;
	int err, i;
	int *buffer = NULL;
    int64_t result = 0;

	err = get_uri(domain_id, calculator_URI, strlen(calculator_URI), &uri);
	if (err)
		goto error;

	err = calculator_open(uri, &handle);
	if (!err) {
		buffer = (int *)rpcmem_alloc(RPCMEM_HEAP_ID_SYSTEM, RPCMEM_DEFAULT_FLAGS, nums * sizeof(int));
		if (!buffer) {
			printf("Error: Failed to allocate memory\n");
			return -ENOMEM;
		}
		for (i = 0; i < nums; i++)
			buffer[i] = i;
	
		err = calculator_sum(handle, buffer, nums, &result);
		if (!err) {
			printf("Sum = %" PRId64 "\n", (int64_t)result);
	    	
		} else {
			printf("Error invoking sum method [%x]\n", err);
		}
		err = calculator_close(handle);
		if (err) 
			printf("ERROR 0x%x: Failed to close handle\n", err);
			} else {
        printf("Error: calculator_open failed 0x%x\n", err);
	}

error:
	if (uri)
		free(uri);

	if (buffer) 
		rpcmem_free(buffer);
	return err;
}

int main(int argc, char* argv[])
{
	int option = 0, err;
	int domain_id = -1;
	int nums = 25;
	int unsigned_pd = 1;

	while ((option = getopt(argc, argv, "d:n:U:")) != -1) {
		switch (option) {
	    	case 'd' : 
			domain_id = atoi(optarg);
		break;
	    	case 'n' : 
			nums = atoi(optarg);
		break;
		case 'U':
                unsigned_pd = atoi(optarg);
        break;
		default:
			printf("Usage:\n"
				"    calculator_test [-d domain] [-n numbers] [-U unsigned_pd] \n\n"
				"-d domain: Run on a specific domain.\n"
				"    0: Run the example on ADSP\n"
	     			"    3: Run the example on CDSP\n"
		     		"    Default Value: 3(CDSP) for targets having CDSP and 0(ADSP) for targets not having CDSP like Agatti.\n"
				"-n N numbers: how many numbers to sum.\n"
				"-U unsigned_pd: 1 for unsigned, 0 for signed.\n");
                return -1;
		}
	}

	/* default to CDSP */
	if (domain_id == -1)
		domain_id = CDSP_DOMAIN_ID;

	printf("Starting test on (%s) with nums=%d, unsigned_pd=%d\n",
           get_domain_name(domain_id), nums, unsigned_pd);
	err = set_unsigned_module_loading(domain_id, (unsigned_pd == 0));
	if (err) {
        printf("Warning: Failed to set module loading mode: 0x%x\n", err);
	}

	return calculator_test(domain_id, nums);
}
