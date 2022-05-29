# bestbuy.py
# Copyright (C) 2022 Liu Xinyu (liuxinyu95@gmail.com)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""
Given a seller product price catalog

1: {A:20, B:15, C:22, ...}
2: {B:10, C:25, ....}
....

and a purchase list

"A, B, C, ..."

find the cheapest buy plan, each seller charges 8$ delivery fee
"""

import random

INF = 1000000
DELIVER_FEE = 8

# Method 1, Dynamic Programming
#
# Buy m product: o[1], o[2], ..., o[m] from n sellers: s[1], s[2], ..., s[n]
#   DP table: tab[1..n].
#   For each product o[i], tab[j] is the min cost to buy the first i products among the first j sellers.

def bestbuy(catalog, order):
    def new_plan(plan, p, s):
        cost, sellers = plan
        return (cost + catalog[s][p] + (0 if s in sellers else DELIVER_FEE), sellers + [s])
    print(catalog)
    tab = [(0, []) for _ in catalog]
    for prod in order:
        print("buy", prod)
        min_sofar = tab[-1]
        if min_sofar[0] >= INF:
            return (INF, [])
        sel = None
        for i, seller in enumerate(catalog):
            if prod in seller:
                if sel is None:
                    sel = i
                    min_sofar = min(new_plan(min_sofar, prod, i), new_plan(tab[i], prod, i))
                else:
                    min_sofar = min(min_sofar, new_plan(tab[i], prod, i))
            tab[i] = (INF,  []) if sel is None else min_sofar
            print("tab[", i, "]:", tab[i])
        print("tab:", tab)
    return tab[-1]

# Method 2, recursive DFS

def findbest(catalog, order):
    def dfs(rest, sellers):
        if rest == []:
            return costof(catalog, order, sellers), sellers
        prod = rest[0]
        cost = (INF, [])
        for i, seller in enumerate(catalog):
            if prod in seller:
                cost = min(cost, dfs(rest[1:], sellers + [i]))
        return cost
    return dfs(order, [])

def costof(catalog, order, sellers):
    return len(set(sellers)) * DELIVER_FEE + sum([catalog[s][o] for (o, s) in zip(order, sellers)])

# test data
PRODS = ["mouse", "keyboard", "ear phone", "battery", "cable",
         "charger", "speaker", "touch pad", "notebook", "pen",
         "ink", "cap", "eraser", "clip", "stamp", "ruler",
         "tape", "post", "marker", "pencil", "paper", "cover",
         "envelope", "sharpener"]

def test():
    def gen_catalog():
        n = random.randrange(1, 10) # max 10 sellers
        catalog = []
        for _ in range(n):
            prod = random.sample(PRODS, random.randrange(1, 5)) # max 5 products
            price = [random.randrange(1, 50) for _ in prod]
            seller = dict(zip(prod, price))
            catalog.append(seller)
        return catalog

    def prod_of(catalog):
        prods = set([])
        for s in catalog:
            prods = prods | set(s.keys())
        return prods

    for _ in range(10):
        catalog = gen_catalog()
        print(catalog)
        prods = prod_of(catalog)
        for _ in range(10): # happy case
            m = min(6, random.randrange(1, len(prods)))
            order = random.sample(prods, m)
            (p1, s1) = bestbuy(catalog, order)
            (p2, s2) = findbest(catalog, order)
            if p1 != costof(catalog, order, s1):
                print("DP err, cost=", p1, "sellers:", s1)
            if p2 != costof(catalog, order, s2):
                print("DFS err, cost=", p2, "sellers:", s2)
            if p1 != p2:
                print("order:", order)
                print("DP != DFS")
                print("DP : cost=", p1, "sellers:", s1)
                print("DFS: cost=", p2, "sellers:", s2)
        # negative case
        m = min(6, random.randrange(1, len(prods)))
        order = random.sample(prods, m) + ["null thing"]
        (p1, s1) = bestbuy(catalog, order)
        (p2, s2) = findbest(catalog, order)
        if s1:
            print("DP negative case err:", (p1, s1))
        if s2:
            print("DFS negative case err:", (p2, s2))
    print("test done")

# failed case:
CATALOG = [{'notebook': 10, 'eraser': 14, 'cover': 11},
           {'eraser': 10, 'mouse': 11, 'notebook': 26, 'charger': 49},
           {'ink': 32, 'cover': 9},
           {'speaker': 42}]

ORDER = ['speaker', 'mouse', 'eraser', 'cover', 'notebook']
# DP != DFS
# DP : cost= 114 sellers: [3, 1, 1, 2, 0]
# DFS: cost= 108 sellers: [3, 1, 1, 0, 0]

if __name__ =="__main__":
    #test()
    print(bestbuy(CATALOG, ORDER))
