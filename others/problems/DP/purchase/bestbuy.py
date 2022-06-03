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

def bestbuy(catalog, order):
    def update(os1, os2, seller):
        delta = 0
        plan = {}
        for o in os2:
            if o not in tab[os1]:
                plan[o] = seller
            else:
                prev_seller = tab[os1][o]
                if o not in catalog[seller]:
                    plan[o] = prev_seller
                else:
                    d = catalog[prev_seller][o] - catalog[seller][o]
                    if d > 0:
                        delta = delta + d
                    plan[o] = seller if d > 0 else prev_seller
        if os1 == os2 and delta < DELIVER_FEE:
            return tab[os1]
        return plan
    print("catalog", catalog)
    print("order", order)
    tab = {}
    os = frozenset(order)
    for i, seller in enumerate(catalog):
        osi = os & frozenset(seller.keys())   # what can buy from seller i.
        print("seller", i, osi)
        if osi not in tab:
            tab[osi] = dict(zip(list(osi),  [i] * len(osi)))
        newtab = {}
        for objs in tab:
            newobjs = osi | objs # what we can buy additionally
            newtab[newobjs] = update(objs, newobjs, i)
        for objs in newtab:
            tab[objs] = newtab[objs]
        print("tab:", tab)
    if os in tab:
        print("os",  os)
        print("tab[os]", tab[os])
        sellers = [tab[os][o] for o in order]
        print("reordered to:", order, sellers)
        return (costof(catalog, order, sellers), sellers)
    else:
        return (INF, [])

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

    def gen_order(prods):
        n = len(prods)
        return random.sample(prods, 1 if n == 1 else min(6, random.randrange(1, n)))

    for _ in range(10):
        catalog = gen_catalog()
        print(catalog)
        prods = prod_of(catalog)
        for _ in range(10): # happy case
            order = gen_order(prods)
            (p1, s1) = bestbuy(catalog, order)
            (p2, s2) = findbest(catalog, order)
            if p1 <= INF and p1 != costof(catalog, order, s1):
                print("DP err, cost=", p1, "sellers:", s1)
            if p2 <= INF and p2 != costof(catalog, order, s2):
                print("DFS err, cost=", p2, "sellers:", s2)
            if p1 != p2:
                print("order:", order)
                print("DP != DFS")
                print("DP : cost=", p1, "sellers:", s1)
                print("DFS: cost=", p2, "sellers:", s2)
        # negative case
        order = gen_order(prods) + ["null thing"]
        (p1, s1) = bestbuy(catalog, order)
        (p2, s2) = findbest(catalog, order)
        if s1:
            print("DP negative case err:", (p1, s1))
        if s2:
            print("DFS negative case err:", (p2, s2))
    print("test done")

# example case:
CATALOG = [{'ear phone': 46, 'charger': 7},
           {'tape': 37, 'touch pad': 9, 'charger': 45},
           {'keyboard': 18, 'clip': 21},
           {'eraser': 2, 'keyboard': 14, 'mouse': 47}]

ORDER =  ['keyboard', 'clip', 'mouse', 'charger']
# DFS: cost= 113 sellers: [3, 2, 3, 0]

if __name__ =="__main__":
    test()
    #print(bestbuy(CATALOG, ORDER))
