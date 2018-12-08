#include "scm.h"

void dstats() {
    printf("\n** [heap: %zd] [free: %zd] [bytes-since-gc: %zd] [total: %zd]\n",
            GC_get_heap_size(),
            GC_get_free_bytes(),
            GC_get_bytes_since_gc(),
            GC_get_total_bytes());
}

static scm_ctx_t* scm_init() {
    scm_ctx_t* ctx;

    GC_INIT();

    ctx = init_context();
    init_errors(ctx);
    init_bool(ctx);
    init_pair(ctx);
    init_vector(ctx);
    init_port(ctx);

    return ctx;
}


int main(int argc, char* argv[]) {
    scm_ctx_t* ctx = scm_init();

    printf("scm v0.1\n");

    scmval write  = lookup(ctx, make_symbol("write"));
    scmval vector = lookup(ctx, make_symbol("vector"));

    scmval v = 
        cons(make_fixnum(42),
            cons(make_flonum(3.14),
                cons(make_string("hello"),
                    cons(scm_true, scm_null))));

    apply(ctx, write, cons(v, scm_null));
    printf("\n");

    try(ctx) {
        scmval vec = apply(ctx, vector, v);
        apply(ctx, write, cons(vec, scm_null));
    } catch {
        printf("%s: %s\n",
                string_to_cstr(error_type(ctx->stack->err)),
                string_to_cstr(error_message(ctx->stack->err)));
    }

    return 0;
}

