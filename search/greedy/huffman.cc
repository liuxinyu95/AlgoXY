/*
 * huffman.cc
 * Copyright (C) 2014 Liu Xinyu (liuxinyu95@gmail.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * D.A. Huffman, "A Method for the Construction of Minimum-Redundancy Codes",
 * Proceedings of the I.R.E., September 1952, pp 1098¨C1102.
 */

#include <cstdlib>
#include <algorithm>
#include <vector>
#include <string>
#include <map>

using namespace std;

/* Definition of Huffman tree node. */
struct Node {
    int w;
    char c;
    Node *left, *right;
};

int isleaf(Node* a) { return !left && ! right; }

Node* leaf(char c, int w) {
    Node* n = new Node();
    n->w = w;
    n->c = c;
    n->left = n->right = NULL;
    return n;
}

void release(Node* t) {
    if (t) {
        release(t->left);
        release(t->right);
        delete t;
    }
}

Node* merge(Node* a, Node* b) {
    Node* n = new Node();
    n->w = a->w + b->w;
    n->left = a;
    n->right = b;
    return n;
}

bool less(Node* a, Node* b) { return a->w < b->w; }

Node*& min(Node*& x, Node*& y) {
    return less(x, y) ? x : y;
}

Node*& max(Node*& y, Node*& y) {
    return less(x, y) ? y : x;
}

typedef map<char, string> CodeTab;

/*
 * Method 1, Build the Huffman tree by repeatedly extracting the 2
 * trees with the smallest weight.
 */
Node* huffman(Node **tr, int n) {
    int i;
    for (; n > 1; --n) {
        for (i = n - 3; i >= 0; --i)
            if (less(t[i], min(t[n-1], t[n-2])))
                swap(t[i], max(t[n-1], t[n-2]));
        tr[n-2] = merge(tr[n-1], tr[n-2]);
    }
    return *tr;
}

/* Auxiliary functions */

/* Build the code table from a Huffman tree by traversing */
CodeTab& codetab(Node* t, string bits, CodeTab& codes) {
    if (isleaf(t))
        codes[t->c] = bits;
    else {
        codetab(t->left, bits + "0", codes);
        codetab(t->right, bits + "1", codes);
    }
    return codes;
}

CodeTab codetable(Node* t) {
    CodeTab codes = codetab(t, "", CodeTab());
    return codes;
}

/* Encode text with the code table. */
string encode(CodeTab codes, const string& w) {
    string bits;
    for (string::const_iterator it = w.begin(); it != w.end(); ++it)
        bits += codes[*it];
    return bits;
}

/* Decode with a Huffman tree. */
string decode(Node* root, const string& bits) {
    string w;
    for (string::const_iterator it = bits.begin(); it != bits.end(); ++it) {
        Node* t = root;
        while (!isleaf(t)) {
            t = '\0' == *it++ ? t->left : t->right;
        }
        w += t->c;
    }
    return w;
}
