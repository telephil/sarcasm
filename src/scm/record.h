typedef struct scm_record scm_record_t;

struct scm_record {
    scmval  type;
    scmval  slots;
};
// constructor
scmval make_record(scmval, scmval);
// accessors
static inline scm_record_t* get_record(scmval v) { return (scm_record_t*)v.o; }
// predicate
static inline bool is_record(scmval v) { return type_of(v) == SCM_TYPE_RECORD; }
// contract
define_contract(record_c, "record", is_record);
// standard lib
void init_record(scmval);

static inline scmval record_type(scmval v) { return get_record(v)->type; }
static inline scmval record_slots(scmval v) { return get_record(v)->slots; }

