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

INF = 100000
DELIVER_FEE = 8

# method 1, Dynamic Programming
#
# Buy m product: o[1], o[2], ..., o[m] from n sellers: s[1], s[2], ..., s[n]
#   DP table: tab[1..n].
#   For each product o[i], tab[j] is the min cost to buy first i products among first j sellers.

def bestbuy(catalog, order):
    tab = [(0, []) for _ in catalog]
    print(tab)
    for prod in order:
        print("buy", prod)
        min_sofar = (INF, [])
        for i, seller in enumerate(catalog):
            print("seller", i, seller)
            cost, sellers = tab[i]
            tab[i] = min_sofar
            if prod in seller:
                newcost = cost + seller[prod] + (0 if i in sellers else DELIVER_FEE)
                print("new cost = ", newcost)
                if newcost < min(INF, min_sofar[0]):
                    min_sofar = (newcost, [i] + sellers)
                    tab[i] = min_sofar
            print("tab[", i, "]:", tab[i])
        print("tab:", tab)
    return tab[-1]

# method 2, recursive DFS
def findbest(catalog, order):
    def costof(sellers):
        return len(set(sellers)) * DELIVER_FEE + sum([catalog[s][o] for (s, o) in zip(sellers, reversed(order))])
    def dfs(rest, sellers):
        if rest == []:
            return (costof(sellers), sellers)
        prod = rest[0]
        cost = (INF, [])
        for i, seller in enumerate(catalog):
            if prod in seller:
                cost = min(cost, dfs(rest[1:], [i] + sellers))
        return cost
    return dfs(order, [])

# test data
CATALOG1 = [{"mouse":6, "keyboard":10, "ear phone":12},
            {"keyboard":8, "mouse":13, "battery":15},
            {"USB cable": 5, "mouse":6, "ear phone":10},
            {"battery":12, "ear phone":11, "speaker":16}]

def test():
    print(bestbuy(CATALOG1, ["mouse", "keyboard", "battery"]))
    print(findbest(CATALOG1, ["mouse", "keyboard", "battery"]))

if __name__ =="__main__":
    test()
