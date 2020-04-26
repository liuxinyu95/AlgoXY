# purchase.py
# Copyright (C) 2019 Liu Xinyu (liuxinyu95@gmail.com)
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

# Problem: Given multiple product ordering plans,
#   for e.g. Plan P sell products A, B, C together at price X
# For a product list [M, N, ...] find the optimal purchase plan list
# at the minimum cost

# For example, with the below plans:

INPUT1 = {"plan1" : ("AB", 100),
          "plan2" : ("BCD", 150),
          "plan3" : ("AD", 125),
          "plan4" : ("CD", 135)}

# Below are the optimal purchase with the minimum cost:
#    lowest cost of "BAD"  # plan1, plan3  ==> 225
#    lowest cost of "BAC"  # plan1, plan4  ==> 235
#    lowest cost of "BCD"  # plan2 ==> 150

# Here is another example. We use 0-9A-Z to enumerate products:

INPUT2 = {"plan0" : ("816309", 11),
          "plan1" : ("7824", 28),
          "plan2" : ("487i620", 47),
          "plan3" : ("649", 57),
          "plan4" : ("407396812", 57),
          "plan5" : ("986750123", 64),
          "plan6" : ("9642", 86),
          "plan7" : ("16480579", 107),
          "plan8" : ("9648350", 111),
          "plan9" : ("8937514", 128)}

# the optimal purchase for product list "704938521" is
#   ==> (set(['plan5', 'plan1']), 92)

# Dynamic Programming solution:

def dp(plans):
    tab = { 0 : (set([]), set(""))}  # DP table { cost : ([plan], [products]) }
    for plan, (prods, price) in plans.items():
        for cost, (plan_set, prod_set) in tab.items():
            _cost = price + cost
            _prod_set = set(prods)
            if _cost not in tab or not _prod_set.issubset(prod_set):
                tab[_cost] = (plan_set.union([plan]), _prod_set.union(prod_set))
    return tab

def lowest(prods, tab):
    _prod_set = set(prods)
    for cost in sorted(tab.keys()):
        plan_set, prod_set = tab[cost]
        if _prod_set.issubset(prod_set):
            return (plan_set, cost)
    return None

# Brute force solution for verification purpose:

fst = lambda (a, b) : a
snd = lambda (a, b) : b

def brute_force(prods, offers):
    def cost_of(plans):
        return sum([snd(offers[p]) for p in plans])
    def to_set(plans):
        return set(plans.split(","))
    def from_set(plan_set):
        return ",".join(plan_set)
    def expand(ps, plans):
        res = set([])
        for plan_set in ps:
            lst = [from_set(p.union(plan_set)) for p in plans]
            res = res.union(set(lst))
        return map(to_set, res)
    prods = "".join(set(prods))
    ps = [set([])]
    for s in prods:
        ps = expand(ps, [set([p]) for p, (ss, _) in offers.items() if s in ss])
    costs = [(plan_set, cost_of(plan_set)) for plan_set in ps]
    # print costs
    return min(costs, key = snd)

# Verification:

def verify(prods, dp_tab, offers):
    print "dp for", prods, "==>", lowest(prods, dp_tab)
    print "brute force for", prods, "==>", brute_force(prods, offers)

def test():
    offers = INPUT1
    tab = dp(INPUT1)
    verify("BAD", tab, offers)   # plan1, plan3  ==> 225
    verify("BAC", tab, offers)  # plan1, plan4  ==> 235
    verify("BCD", tab, offers)  # plan2 ==> 150
    offers = INPUT2
    tab = dp(INPUT2)
    verify("704938521", tab, offers)

if __name__ == "__main__":
    test()
