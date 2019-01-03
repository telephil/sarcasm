#include "scm.h"

scmval make_record(scmval type, scmval slots) {
    scm_record_t* record = scm_new(scm_record_t);
    record->type = type;
    record->slots = slots;
    return make_ptr(SCM_TYPE_RECORD, record);
}

scmval scm_make_record(scmval type, scmval slots) {
    return make_record(type, slots);
}

scmval scm_record_p(scmval obj) {
    return scm_bool(is_record(obj));
}

scmval scm_record_type(scmval obj) {
    check_arg("record-type", record_c, obj);
    return record_type(obj);
}

scmval scm_record_slots(scmval obj) {
    check_arg("record-slots", record_c, obj);
    return record_slots(obj);
}

void init_record(scmval env) {
    define(env, "%make-record", scm_make_record,     arity_exactly(2));
    define(env, "%record?",      scm_record_p,       arity_exactly(1));
    define(env, "%record-type",  scm_record_type,    arity_exactly(1));
    define(env, "%record-slots", scm_record_slots,   arity_exactly(1));
}

