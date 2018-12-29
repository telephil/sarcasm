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

static void env_copy_callback(scmval key, scmval val, scmval lib) {
    dict_set(library_symbols(lib), key, val);
    library_add_export(lib, key);
}

scmval make_core_library(scmval env) {
    scmval corelib = make_library(cons(intern("sarcasm"), cons(intern("core"), scm_null)));
    dict_foreach(env_globals(env), env_copy_callback, corelib);
    library_cache = cons(corelib, library_cache);
    return corelib;
}

static scmval find_in_cache(scmval name) {
    foreach(lib, library_cache) {
        if(is_equal(name, library_name(lib)))
            return lib;
    }
    return scm_undef;
}

scmval load_library(scmval name) {
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
    printf("lib '%s' => '%s'\n", scm_to_cstr(name), c_str(s));

    return scm_undef;
}

scmval define_library(scmval expr) {
    scmval name = car(expr);
    if(!is_list(expr)) error(syntax_error_type, "%s is not a valid library name", scm_to_cstr(name));
    // XXX check identifier + numbers
    scmval exports  = scm_null;
    scmval imports  = scm_null;
    scmval includes = scm_null;
    scmval body     = scm_null;
    for(scmval obj, lst = cdr(expr); !is_null(lst) && !is_undef(obj = car(lst)); lst = cdr(lst)) {
        if(!is_list(obj))
            error(syntax_error_type, "%s is not a valid library declaration", scm_to_cstr(name));
        if(is_eq(car(obj), scm_export)) {
            exports = cons(cdr(obj), body);
        } else if(is_eq(car(obj), scm_import)) {
            imports = cons(cdr(obj), imports);
        } else if(is_eq(car(obj), scm_include)) {
            includes = cons(cdr(obj), includes);
        } else if(is_eq(car(obj), scm_begin)) {
            body = cons(cdr(obj), body);
        }
    }
    scmval library = make_library(name);
    return library;
}

void init_library(scmval env) {
    library_cache = scm_null;
    scm_export  = intern("export");
    scm_import  = intern("import");
    scm_include = intern("include");
    scm_begin   = intern("begin");
}

