#include "scm.h"

// globals
static scmval library_cache;
static scmval scm_export;
static scmval scm_import;
static scmval scm_include;
static scmval scm_begin;

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
    scmval corelib = make_library(read_from_string("(sarcasm core)"));
    dict_foreach(env_globals(env), env_copy_callback, corelib);
    register_library(corelib);
    return corelib;
}

static scmval find_in_cache(scmval name) {
    foreach(lib, library_cache) {
        if(is_equal(name, library_name(lib)))
            return lib;
    }
    return scm_undef;
}

scmval load_library(scmval name, scmval env) {
    scmval lib = find_in_cache(name);
    if(!is_undef(lib))
        return lib;

    scmval p = scm_open_output_string();
    scm_printf(p, "./lib");
    foreach(elt, name) {
        scm_putc(p, '/');
        write(p, elt, scm_mode_display);
    }
    scm_puts(p, ".scm");
    scmval s = scm_get_output_string(p);
    if(is_false(scm_file_exists_p(s)))
        error(intern("file-error"), "library %s not found", scm_to_cstr(name));
    lib = load(c_str(s), env);
    return lib;
}

void init_library(scmval env) {
    library_cache = scm_null;
    scm_export  = intern("export");
    scm_import  = intern("import");
    scm_include = intern("include");
    scm_begin   = intern("begin");
}

