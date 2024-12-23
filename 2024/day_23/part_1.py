import sys


if __name__ == "__main__":
    with open(sys.argv[1], "r") as f:
        edges = [line.split("-") for line in f.read().split()]

    adj_set = {}
    for (u, v) in edges:
        if u not in adj_set:
            adj_set[u] = set()
        adj_set[u].add(v)

        if v not in adj_set:
            adj_set[v] = set()
        adj_set[v].add(u)

    lans = set()
    for v in adj_set:
        for u in adj_set[v]:
            for w in adj_set[u]:
                if (w != v) and (v in adj_set[w]) and ("t" in (v[0] + u[0] + w[0])):
                    lans.add(frozenset((v, u, w)))

    print(len(lans))