extern scmval scm_quote;

void init_reader();
scmval read(scmval);
scmval read_from_string(const char*);
void load(const char*);
