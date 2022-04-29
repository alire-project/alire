#include <stdio.h>
#include "libcrate_config_config.h"

void test_c_print(void) {
    printf("C -> Crate_Version: %s\n", CRATE_VERSION);
    printf("C -> Crate_Name: %s\n", CRATE_NAME);
    printf("C -> Alire_Host_OS: %s\n", ALIRE_HOST_OS);
    printf("C -> Alire_Host_Arch: %s\n", ALIRE_HOST_ARCH);
    printf("C -> Alire_Host_Distro: %s\n", ALIRE_HOST_DISTRO);
    printf("C -> VAR_BOOL: %d\n", VAR_BOOL);
    printf("C -> VAR_STRING: '%s'\n", VAR_STRING);
    printf("C -> VAR_INT_FIRST: %d\n", VAR_INT_FIRST);
    printf("C -> VAR_INT_LAST: %d\n", VAR_INT_LAST);
    printf("C -> VAR_INT: %d\n", VAR_INT);
    printf("C -> VAR_REAL_FIRST: %f\n", VAR_REAL_FIRST);
    printf("C -> VAR_REAL_LAST: %f\n", VAR_REAL_LAST);
    printf("C -> VAR_REAL: %f\n", VAR_REAL);
    printf("C -> VAR_ENUM_A: %d\n", VAR_ENUM_A);
    printf("C -> VAR_ENUM_B: %d\n", VAR_ENUM_B);
    printf("C -> VAR_ENUM_C: %d\n", VAR_ENUM_C);
    printf("C -> VAR_ENUM: %d\n", VAR_ENUM);
}
