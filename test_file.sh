#!/usr/bin/env bash

prog=$1
cmp="cargo run --"
gcc -w $prog
./a.out
expected_exit_code=$?
rm a.out

$cmp $prog >/dev/null
base="${prog%.*}" #name of executable (filename w/out extension)
./$base
actual_exit_code=$?
test_name=$(basename $base)
printf '%s' "$test_name"
printf '%*.*s' 0 $((padlength - ${#test_name})) "$padding_dots"

if [ "$expected_exit_code" -ne "$actual_exit_code" ]
then
    echo "FAIL"
else
    echo "OK"
fi
rm $base