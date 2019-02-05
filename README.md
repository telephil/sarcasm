# sarcasm
A simple (even naive) work-in-progress R7RS scheme interpreter written in C11.

### Building
Type ```make``` in the root directory to create the ```sarcasm``` executable.
```shell
$ make
```
**Note:** Build has only been tested on Mac OSX

#### Dependencies
- [Boehm GC](http://www.hboehm.info/gc/)
- [GMP](https://gmplib.org/)
- [FFI](https://sourceware.org/libffi/)
- [GNU Readline](https://tiswww.cwru.edu/php/chet/readline/rltop.html)

### Running
The run the sarcasm interpreter, type the following command
```shell
$ sarcasm
```
Following options are recognized by the interpreter:
```
  -h, --help                    print help and exit
  -e, --eval <form>             evaluate form and exit
  -s, --script <filename>       execute file as a shell script
  -l, --load <filename>         load file then run repl
```

### Licence
See ```LICENSE```


