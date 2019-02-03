#include "scm.h"

#define TYPES_MAX 32

static int ntype = 0;
static const char* typenames[TYPES_MAX] = { 0 };
static scm_printer printers[TYPES_MAX];

scm_type_t register_type(const char* name) {
    if(ntype >= TYPES_MAX) {
        fprintf(stderr, "!!! too many defined types\n");
        exit(1);
    }
    scm_type_t type = ntype;
    typenames[type] = name;
    printers[type] = NULL;
    ++ntype;
    return type;
}

void register_type_printer(scm_type_t type, scm_printer printer) {
    printers[type] = printer;
}

scm_type_t register_type_with_printer(const char* name, scm_printer printer) {
    scm_type_t type = register_type(name);
    register_type_printer(type, printer);
    return type;
}

const char* get_type_name(scm_type_t type) {
    return typenames[type];
}

scm_printer get_type_printer(scm_type_t type) {
    return printers[type];
}

