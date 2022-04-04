#include <stdio.h>
#include "libcrate_config_config.h"

void test_c_print(void) {
    printf("C -> Crate_Version: %s\n", CRATE_VERSION);
    printf("C -> Crate_Name: %s\n", CRATE_NAME);
    printf("C -> Alire_Host_OS: %s\n", ALIRE_HOST_OS);
    printf("C -> Alire_Host_Arch: %s\n", ALIRE_HOST_ARCH);
    printf("C -> Alire_Host_Distro: %s\n", ALIRE_HOST_DISTRO);
    printf("C -> Var_Bool: %d\n", Var_Bool);
    printf("C -> Var_String: '%s'\n", Var_String);
    printf("C -> Var_Int: %d\n", Var_Int);
    printf("C -> Var_Real: %f\n", Var_Real);
    printf("C -> Var_Enum: %d\n", Var_Enum);
}
