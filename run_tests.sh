#!/bin/bash

TESTS_SUCCEED="
    fib2.lox
    make-point.lox
    modify_outside_function.lox
    counter.lox
    fun.lox
    scope2.lox
    clock.lox
    logical.lox
    string-concat.lox
    comments.lox
    cls-print.lox
    cls-callback.lox
    while-loop-control.lox
    for-loop-control.lox
    anonymous-fun.lox
    cls-methods.lox
    cls-accessor.lox
    multiline-string.lox
    cls-inheritance.lox
    cls-inheritance-super.lox
    exceptions.lox
"

TESTS_FAIL="
    scope.lox
    this-invalid.lox
    bad-unary.lox
    arity.lox
    funny-scope.lox
    for-undefined.lox
    cls-inheritance-invalid-super.lox
"

run() {
    java -jar target/scala-2.12/slox-assembly-0.1.0-SNAPSHOT.jar tests/$1
}

failures=0

for i in $TESTS_SUCCEED; do
    echo
    echo "********** START: $i"
    run $i && echo "********** PASS: $i" || { failures=$((failures+1)); echo "********** FAIL: $i"; }
done

for i in $TESTS_FAIL; do
    echo
    echo "********** START: $i"
    run $i && { failures=$((failures+1)); echo "********** FAIL: $i"; } || echo "********** PASS: $i"
done

[ $failures -eq 0 ]
