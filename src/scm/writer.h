enum {
    scm_mode_write      = 1<<0,
    scm_mode_display    = 1<<1,
    scm_mode_pp_quote   = 1<<2
};
// write : port? any?
void write(scmval, scmval, scmfix);
