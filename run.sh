#!/bin/bash

ocamlc linear.ml linear_test.ml && ./a.out
ocamlc repr.ml repr_test.ml && ./a.out

