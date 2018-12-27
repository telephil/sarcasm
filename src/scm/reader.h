extern scmval scm_quote;
extern scmval scm_quasiquote;
extern scmval scm_unquote;
extern scmval scm_unquote_splicing;

void init_reader();
scmval read(scmval);
scmval read_from_string(const char*);
void load(const char*);
