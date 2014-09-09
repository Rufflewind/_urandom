#include <vector>

/// Find the connected components of an undirected graph.  Returns a vector
/// containing the *colors* of each node: colors are arbitrary numbers
/// uniquely assigned to each connected component.
///
/// @tparam AdjFunc
/// A function compatible with `(std::size_t, std::size_t) -> bool`.
///
/// @param adjacency
/// A symmetric function that describes the adjacency between nodes:
/// `adjacency(x, y) == adjacency(y, x)`.  The algorithm assumes that
/// `adjacency(x, x) == true` although the function will never be called with
/// two identical arguments.
///
/// @param num_nodes
/// The total number of nodes.
///
///   - Worst case (fully disconnected): `O(num_nodes ^ 2)`
///   - Best case (fully connected): `O(num_nodes)`
///
template<class AdjFunc> inline
std::vector<std::size_t>
find_connected(AdjFunc adjacency, std::size_t num_nodes) {
    using std::size_t;
    // here we keep track of the size of `others` manually for simplicity and
    // just use the `vector` as a plain array (because of various
    // complications due to `pop_back` potentially invalidating `j`)
    std::vector<size_t> candidates, colors(num_nodes), others(num_nodes);
    size_t others_count = num_nodes, color = 0;
    for (size_t i = 0; i != num_nodes; ++i) {
        others[i] = i;
    }
    while (others_count) {
        candidates.push_back(others[--others_count]);
        while (!candidates.empty()) {
            const size_t i = candidates.back();
            candidates.pop_back();
            colors[i] = color;
            for (size_t *j = &others[0]; j != &others[0] + others_count;) {
                if (adjacency(i, *j)) {
                    candidates.push_back(*j);
                    *j = std::move(others[--others_count]);
                } else {
                    ++j;
                }
            }
        }
        ++color;
    }
    return colors;
}
