def get_tree_size(ground, root, graph):
    leaves = list(filter(lambda x: x != ground, graph[root]))
    if not leaves:
        return 1
    else:
        return 1 + sum([get_tree_size(root, leaf, graph) for leaf in leaves])


def get_cut_count(ground, graph, before=None):
    leaves = list(filter(lambda x: x != before, graph[ground]))
    if not leaves:
        return 0
    else:
        count = 0
        for leaf in leaves:
            size_of_sub_tree = get_tree_size(ground, leaf, graph)
            if size_of_sub_tree % 2 == 0:
                count += 1
            count += get_cut_count(leaf, graph, ground)
    return count


def find_max_cut_count(graph):
    return max([get_cut_count(node, graph) for node in graph.keys()])


if __name__ == '__main__':
    vertices, edges = map(int, input().split())
    input_graph = {}
    for _ in range(0, edges):
        vertex, to = map(int, input().split())
        if vertex not in input_graph:
            input_graph[vertex] = [to]
        else:
            input_graph[vertex].append(to)

        if to not in input_graph:
            input_graph[to] = [vertex]
        else:
            input_graph[to].append(vertex)

    print(find_max_cut_count(input_graph))
