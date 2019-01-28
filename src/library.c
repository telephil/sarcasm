#include <dlfcn.h>
#include "scm.h"

// globals
static scmval library_cache;

scmval make_library(scmval name) {
    scm_library_t* lib = scm_new(scm_library_t);
    lib->name       = name;
    lib->symbols    = make_dict();
    lib->exports    = scm_null;
    return make_ptr(SCM_TYPE_LIBRARY, lib);
}

void register_library(scmval library) {
    library_cache = cons(library, library_cache);
}

static void env_copy_callback(scmval key, scmval val, scmval lib) {
    dict_set(library_symbols(lib), key, val);
    library_add_export(lib, key);
}

scmval make_core_library(scmval env) {
    scmval corelib = make_library(lib("sarcasm", "core"));
    dict_foreach(env_globals(env), env_copy_callback, corelib);
    register_library(corelib);
    return corelib;
}

static scmval find_in_cache(scmval name) {
    foreach(lib, library_cache) {
        if(is_equal(name, library_name(lib))) {
            return lib;
        }
    }
    return scm_undef;
}

static inline scmval library_to_filename(scmval name) {
    scmval p = scm_open_output_string();
    scm_printf(p, "./lib/");
    scm_display(car(name), p);
    scm_putc(p, '/');
    if(is_number(cadr(name))) {
        scm_display(car(name), p);
        scm_putc(p, '-');
        scm_display(cadr(name), p);
    } else {
        scm_display(cadr(name), p);
    }
    scm_puts(p, ".scm");
    scmval s = scm_get_output_string(p);
    return s;
}

scmval load_library(scmval name, scmval env) {
    scmval lib = find_in_cache(name);
    if(!is_undef(lib))
        return lib;
    scmval filename = library_to_filename(name);
    if(is_false(scm_file_exists_p(filename)))
        error(intern("file-error"), "library %s not found", scm_to_cstr(name));
    lib = load(c_cstr(filename), env);
    return lib;
}

typedef void(*init_fn)(scmval);
void import_c_module(scmval env, scmval name) {
    char module_name[PATH_MAX];
    sprintf(module_name, "%s.dylib", c_cstr(name));
    void* handle = dlopen(module_name, RTLD_LAZY);
    if(handle == NULL)
        error(scm_undef, "unable to find C module (%s)", dlerror());
    init_fn init = (init_fn)dlsym(handle, "init_module");
    if(init == NULL)
        error(scm_undef, "invalid C module (%s)", dlerror());
    init(env);
}

void init_library(scmval env) {
    library_cache = scm_null;
}

