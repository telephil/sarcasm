extern scmval scm_quote;

void init_reader();
scmval read(scmval);
void load(const char*);
