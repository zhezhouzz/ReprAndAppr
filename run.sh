#!/bin/bash

ocamlc linear.ml linear_test.ml && ./a.out
ocamlc repr.ml repr_test.ml && ./a.out
ocamlc linear.ml apprlist.ml apprlist_test.ml && ./a.out
ocamlc linear.ml repr.ml apprlist.ml appr.ml appr_test.ml && ./a.out
