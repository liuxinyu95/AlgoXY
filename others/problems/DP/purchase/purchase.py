# Problem: Given multple of product ordering plans,
#   for e.g. Plan P sell prodcut A, B, C together at price X
# For a product list [M, N, ...] find the optimal purchase plan list
# at the minimum cost

# For example, with the below plans:

INPUT1 = {"plan1" : ("AB", 100),
          "plan2" : ("BCD", 150),
          "plan3" : ("AD", 125),
          "plan4" : ("CD", 135)}

# Below are the optimal purchase with the minimum cost:
#    print lowest("BAD")  # plan1, plan3  ==> 225
#    print lowest("BAC")  # plan1, plan4  ==> 235
#    print lowest("BCD")  # plan2 ==> 150

# Here is another example. We use 0-9A-Z to enumerate products:
# 0123...9ABCDEFG...XYZ

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

# the optimial purchase for prodcut list "704938521" is
#   ==> (set(['plan5', 'plan1']), 92)

fst = lambda (a, b) : a
snd = lambda (a, b) : b

def dp(plans):
    def cost_of(plan_list):
        return sum([snd(INPUT[p]) for p in set(plans)])
    tab = {}  # { pkg : [plan] }
    for p, (services, price) in plans.items():
        for s in services:
            if (s not in tab) or price < cost_of(tab[s]):
                tab[s] = [p]
            for pkg, (plan_set, cost) in tab.items():
                if s not in pkg:
                    new_pkg = "".join(set(pkg + s))
                    new_price = cost if p in plan_set else cost + price
                    if (new_pkg not in tab) or new_price < snd(tab[new_pkg]):
                        tab[new_pkg] = (plan_set.union(set([p])), new_price)
                else:
                    # can lower the price
            # print tab
    return tab

TAB = dp(INPUT)
#print TAB

def lowest(sevs):
    sevs = "".join(set(sevs))
    return TAB[sevs] if sevs in TAB else None

def brute_force(sevs):
    def cost_of(plans):
        return sum([snd(INPUT[p]) for p in plans])
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
    sevs = "".join(set(sevs))
    ps = [set([])]
    for s in sevs:
        ps = expand(ps, [set([p]) for p, (ss, _) in INPUT.items() if s in ss])
    costs = [(plan_set, cost_of(plan_set)) for plan_set in ps]
    # print costs
    return min(costs, key = snd)

def smoke():
    print lowest("BAD")  # plan1, plan3  ==> 225
    print lowest("BAC")  # plan1, plan4  ==> 235
    print lowest("BCD")  # plan2 ==> 150

def verify(srvs):
    print "dp for", srvs, "==>", lowest(srvs)
    print "brute force for", srvs, "==>", brute_force(srvs)

verify("704938521")
