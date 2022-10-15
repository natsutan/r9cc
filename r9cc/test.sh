#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  cargo run "$input"  >& cargo.log
  gcc -static -o tmp tmp.s
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}



assert 1 'return 1; 2; 3;'
assert 2 '1; return 2; 3;'
assert 3 '1; 2; return 3;'

assert 3 'foo=3; return foo;'
assert 8 'foo123=3; bar=5; return foo123+bar;'
assert 2 'foo=3; faa=5; return faa-foo;'

assert 100 'return 100;'
assert 3 '1; 2; return 3;'

assert 3 'a=3; return a;'
assert 8 'a=3; z=5; return a+z;'
assert 6 'a=b=3; return a+b;'


assert 100 'return 100;'
assert 49 'return 49;'
assert  6  'return 4+2;'
assert  2  'return 4-2;'
assert  9  'return 3*3;'
assert  4  'return 12/3;'

assert 47 '5+6*7;'
assert 15 '5*(9-6);'
assert 4 '(3+5)/2;'
assert 8 '5+-1*(-6+3);'

assert 1 '42==42;'
assert 0 '0==1;'
assert 1 '0!=1;'
assert 0 '42!=42;'

assert 1 '0<1;'
assert 0 '1<1;'
assert 0 '2<1;'
assert 1 '0<=1;'
assert 1 '1<=1;'
assert 0 '2<=1;'

assert 1 '1>0;'
assert 0 '1>1;'
assert 0 '1>2;'
assert 1 '1>=0;'
assert 1 '1>=1;'
assert 0 '1>=2;'

echo OK