# sarcasm
A simple (even naive) work-in-progress R7RS scheme interpreter written in C11.

### Building and installing
To build and install the ```sarcasm``` executable, type ```make install``` in the root directory:
```shell
$ make install
```
By default, installation is done in ```/usr/local```. This can be changed by adding a ```prefix``` variable to the ```make```command
```shell
$ make install prefix=/my/path
```

**Note:** Build has been tested on:
- Mac OSX Mojave (10.14)
- Ubuntu Cosmic (18.10)

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


