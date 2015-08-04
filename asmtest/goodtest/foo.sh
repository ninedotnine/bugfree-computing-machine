#!/bin/bash

sax -l foo.list -o foo.out foo.sax
sxxl foo.out > foo.sxx
sxx foo.sxx
