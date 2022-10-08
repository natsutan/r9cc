#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  cargo run "$input"  >& cargo.log
  cc -o tmp tmp.s
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 100 '100'
assert 49 '49'
assert  6  '4+2'
assert  2  '4-2'
assert  9  '3*3'
assert  4  '12/3'

assert 47 '5+6*7'
assert 15 '5*(9-6)'
assert 4 '(3+5)/2'
assert 8 '5+-1*(-6+3)'

assert 0 '0==1'
assert 1 '42==42'
assert 1 '0!=1'
assert 0 '42!=42'

assert 1 '0<1'
assert 0 '1<1'
assert 0 '2<1'
assert 1 '0<=1'
assert 1 '1<=1'
assert 0 '2<=1'

assert 1 '1>0'
assert 0 '1>1'
assert 0 '1>2'
assert 1 '1>=0'
assert 1 '1>=1'
assert 0 '1>=2'

echo OK