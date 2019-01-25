// SARCASM CONFIGURATION

#define IMPLEMENTATION_NAME     "sarcasm"
#define IMPLEMENTATION_VERSION  "0.1"

#if defined(__x86_64__)
    #define ARCH        "x86-64"
#elif defined(__i386__)
    #define ARCH        "i386"
#else
    #define ARCH        "unknown-arch"
#endif

#if defined(__linux__)
    #define OS  "linux"
#elif defined(__APPLE__)
    #define OS  "darwin"
#else
    #define OS  "unknown-platform"
#endif


#if defined(__LITTLE_ENDIAN__)
    #define ENDIANESS   "little-endian"
#elif defined(__BIG_ENDIAN__)
    #define ENDIANESS   "big-endian"
#else
    #define ENDIANESS   "unknown endianess"
#endif

