enum write_mode { 
    WRITE_MODE_WRITE,
    WRITE_MODE_DISPLAY
};

typedef enum write_mode write_mode;

// write : port? any?
void write(scmval, scmval, write_mode);
