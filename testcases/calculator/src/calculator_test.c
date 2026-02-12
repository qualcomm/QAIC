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
	int *buffer;
    	int64_t result = 0;

	err = get_uri(domain_id, calculator_URI, strlen(calculator_URI), &uri);
	if (err)
		goto error;

	err = calculator_open(uri, &handle);
	if (!err) {
		buffer = (int *)rpcmem_alloc(RPCMEM_HEAP_ID_SYSTEM, RPCMEM_DEFAULT_FLAGS, nums * sizeof(int));
		if (!buffer)
			return -ENOMEM;
		for (i = 0; i < nums; i++)
			buffer[i] = i;
	
		err = calculator_sum(handle, buffer, nums, &result);
		if (!err) {
			printf("Sum = %" PRId64 "\n", result);
	    	
		} else {
			printf("Error invoking sum method [%x]\n", err);
		}
		err = calculator_close(handle);
		if (err) 
			printf("ERROR 0x%x: Failed to close handle\n", err);
	}

error:
	if (uri)
		free(uri);

	rpcmem_free(buffer);
	return err;
}

int main(int argc, char* argv[])
{
	int option = 0, err;
	int domain_id = CDSP_DOMAIN_ID;
	int nums = 25;

	while ((option = getopt(argc, argv, "d:n:")) != -1) {
		switch (option) {
	    	case 'd' : 
			domain_id = atoi(optarg);
		break;
	    	case 'n' : 
			nums = atoi(optarg);
		break;
		default:
			printf("Usage:\n"
				"    calculator_test [-d domain]\n\n"
				"-d domain: Run on a specific domain.\n"
				"    0: Run the example on ADSP\n"
	     			"    3: Run the example on CDSP\n"
		     		"        Default Value: 3(CDSP) for targets having CDSP and 0(ADSP) for targets not having CDSP like Agatti.\n"
				"-n N numbers: how many numbers to sum .\n"
				"    Default Value: 25\n");
                return -1;
		}
	}

	/* default to CDSP */
	if (domain_id == -1)
		domain_id = CDSP_DOMAIN_ID;

	printf("Starting test on (%s)\n", get_domain_name(domain_id));
	/* We only support Unsigned module loading */
	err = check_and_enable_unsigned_module_loading(domain_id);
	if (err)
		return err;

	return calculator_test(domain_id, nums);
}
