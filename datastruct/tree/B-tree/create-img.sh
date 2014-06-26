#!/bin/bash

#src/bt2dot img/btree-example.dot "(((A, B), C, (D, E, F), G, (H, I, J, K)), M, ((N, O), P, (Q, R, S), T, (U, V), W, (X, Y, Z)))"
#src/bt2dot img/btree-insert-2-3-4.dot "(((A), C, (D)), E, ((G, J, K), M, (N, O)), P, ((R), S, (T), U, (V), X, (Y, Z)))"
#src/bt2dot img/btree-insert-3.dot "((A, C), D, (E, G, J, K), M, (N, O), P, (R, S), T, (U, V, X, Y, Z))"
#src/bt2dot img/btree-insert-fp-234.dot "(((A), C, (D)), E, ((G, J, K), M, (N)), O, ((P), R, (S), T, (U), V, (X, Y, Z)))"
#src/bt2dot img/btree-insert-fp-3.dot "((A, C, D, E), G, (J, K), M, (N, O), P, (R, S), T, (U, V, X, Y, Z))"

#src/bt2dot img/btree-del-before.dot "(((A, B), C, (D, E, F), G, (J, K, L), M, (N, O)), P, ((Q, R, S), T, (U, V), X, (Y, Z)))"
#src/bt2dot img/btree-del-F.dot "(((A, B), C, (D, E), G, (J, K, L), M, (N, O)), P, ((Q, R, S), T, (U, V), X, (Y, Z)))"
#src/bt2dot img/btree-del-M.dot "(((A, B), C, (D, E), G, (J, K), L, (N, O)), P, ((Q, R, S), T, (U, V), X, (Y, Z)))"
#src/bt2dot img/btree-del-G.dot "(((A, B), C, (D, E, J, K), L, (N, O)), P, ((Q, R, S), T, (U, V), X, (Y, Z)))"
#src/bt2dot img/btree-del-D.dot "((A, B), C, (E, J, K), L, (N, O), P, (Q, R, S), T, (U, V), X, (Y, Z))"
#src/bt2dot img/btree-del-B.dot "((A, C), E, (J, K), L, (N, O), P, (Q, R, S), T, (U, V), X, (Y, Z))"
#src/bt2dot img/btree-del-U.dot "((A, C), E, (J, K), L, (N, O), P, (Q, R), S, (T, V), X, (Y, Z))"

#src/bt2dot img/btree-del-fp-before.dot "(((A, B), C, (D, E, F), G, (H, I, J, K)), M, ((N, O), P, (Q, R, S), T, (U, V), W, (X, Y, Z)))"
#src/bt2dot img/btree-del-fp-E.dot "(((A, B), C, (D, F), G, (H, I, J, K)), M, ((N, O), P, (Q, R, S), T, (U, V), W, (X, Y, Z)))"
#src/bt2dot img/btree-del-fp-G.dot "(((A, B), C, (D, F), H, (I, J, K)), M, ((N, O), P, (Q, R, S), T, (U, V), W, (X, Y, Z)))"
#src/bt2dot img/btree-del-fp-A.dot "((B, C, D, F), H, (I, J, K), M, (N, O), P, (Q, R, S), T, (U, V), W, (X, Y, Z))"
#src/bt2dot img/btree-del-fp-M.dot "((B, C, D, F), H, (I, J, K, N, O), P, (Q, R, S), T, (U, V), W, (X, Y, Z))"
#src/bt2dot img/btree-del-fp-U.dot "((B, C, D, F), H, (I, J, K, N, O), P, (Q, R, S, T, V), W, (X, Y, Z))"

src/bt2dot img/btree-insert-simple1.dot "(((1, 2), 4, (5, 8, 9), 11, (12, 15, 16, 17)), 20, ((21, 25), 26, (30, 31, 37), 38, (40, 42), 45, (46, 47, 50)))"
src/bt2dot img/btree-insert-simple2.dot "(((1, 2), 4, (5, 8, 9), 11, (12, 15, 16, 17)), 20, ((21, 22, 25), 26, (30, 31, 37), 38, (40, 42), 45, (46, 47, 50)))"
