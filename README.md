# sarcasm
A simple (even naive) work-in-progress R7RS scheme interpreter written in C11.

### Building
Type ```make``` in the root directory to create the ```scm``` executable.
```shell
$ make
```
**Note:** Build has only been tested on Mac OSX

#### Dependencies
- [Boehm GC](http://www.hboehm.info/gc/)
- [GNU Readline](https://tiswww.cwru.edu/php/chet/readline/rltop.html)

### Running
The run the sarcasm interpreter, type the following command
```shell
$ scm
```
It is also possible to load a scheme file on startup by passing it as an argument
```shell
$ scm <filename>
```

### Licence
See ```LICENSE```


