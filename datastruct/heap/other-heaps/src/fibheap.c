/*
 * fibheap.c, Fibonacci heap
 * Copyright (C) 2011, Liu Xinyu (liuxinyu95@gmail.com)

 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * Based on Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest 
 * and Clifford Stein. ``Introduction to Algorithms, Second Edition.''
 *  Chapter 20, Fibonacci heaps. The MIT Press 2001, ISBN:0262032937
 */

typedef int Key;

/* Definition of tree*/
struct node{
  Key key;
  struct node *next, *prev, *parent, *children;
  bool mark;
};

struct FibHeap{
  struct node *roots;
  struct node *minTr; /*Point to tree contains the minimum element in root*/
};

