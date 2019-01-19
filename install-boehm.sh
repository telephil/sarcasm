#!/bin/sh
wget https://www.hboehm.info/gc/gc_source/gc-8.0.2.tar.gz
tar xvzf gc-8.0.2.tar.gz
cd gc-8.0.2 && ./configure --prefix=/usr && make && sudo make install

