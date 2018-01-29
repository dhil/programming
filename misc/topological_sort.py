# Print the topological sorting of a graph

class Graph:
    def __init__(self, numVertices, edges):
        vertices = {}
        # Allocate adjacency lists for each vertex
        for i in range(0, numVertices):
            vertices[i] = []
        # Populate the adjacency lists
        for (i, j) in edges:
            vertices[i].append(j)
        self.graph = vertices

    def topologicalSort(self):
        # Initialise all in-degrees as 0
        in_degree = [0] * (len(self.graph))

        # Compute in-degrees by looping through the adjacency
        # lists. Time: O(V + E).
        for i in range(0, len(self.graph)):
            for j in self.graph[i]:
                in_degree[j] += 1

        # Initialise work queue with vertices which have no incoming
        # edges.
        queue = []
        for i in range(0, len(self.graph)):
            if in_degree[i] == 0:
                queue.append(i)

        # Initialise counter to keep track of number of visited
        # vertices.
        count = 0

        # Initialise result container
        result = []

        # Dequeue vertices one by one
        while queue:
            # Pop front
            v = queue.pop(0)
            result.append(v)

            # Iterate through its neighbours and decrease their
            # in-degrees.
            for i in self.graph[v]:
                in_degree[i] -= 1
                if in_degree[i] == 0:
                    queue.append(i)

            count += 1

        # Check whether the sort was successful
        if count == len(self.graph):
            print(result)
        else:
            print("error: the graph is cyclic.")

g = Graph(6, [(5,2), (5,0), (4,0), (4,1), (2,3), (3,1)])
g.topologicalSort()
