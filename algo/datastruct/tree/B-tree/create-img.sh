#!/usr/bin/bash

src/bt2dot img/btree-example.dot "(((A, B), C, (D, E, F), G, (H, I, J, K)), M, ((N, O), P, (Q, R, S), T, (U, V), W, (X, Y, Z)))"
src/bt2dot img/btree-insert-2-3-4.dot "(((A), C, (D)), E, ((G, J, K), M, (N, O)), P, ((R), S, (T), U, (V), X, (Y, Z)))"
src/bt2dot img/btree-insert-3.dot "((A, C), D, (E, G, J, K), M, (N, O), P, (R, S), T, (U, V, X, Y, Z))"
