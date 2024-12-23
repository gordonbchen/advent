import sys

def in_network(adj_set, v, network):
    return network.issubset(adj_set[v])

def build_network(adj_set, v, network):
    if v in network:
        return

    network.add(v)
    for u in adj_set[v]:
        if in_network(adj_set, u, network):
            build_network(adj_set, u, network)

    return network

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

    print(adj_set)

    largest_network = set()
    for v in adj_set:
        network = build_network(adj_set, v, set())
        if len(network) > len(largest_network):
            largest_network = network
            print(network)

    print(",".join(sorted(largest_network)))