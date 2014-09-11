#include <cstddef>
#include <vector>

/// Finds the connected components of an undirected graph.  Returns a vector
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
    // The vector `others` stores the elements of a linked list: this allows
    // elements to be removed in O(1) time while preserving the ordering of
    // the elements.  While this algorithm doesn't care about the ordering, it
    // can cause branch mispredictions if the adjacency function involves
    // comparing the nodes.  This is why we don't use just a plain array with
    // erase-remove instead.
    using std::size_t;
    using std::vector;
    struct elem {
        size_t value;
        struct elem *next;
    };
    vector<size_t> candidates, colors(num_nodes);
    vector<elem> others(num_nodes);
    struct elem *others_begin = &others[0],
          *const others_end   = others_begin + num_nodes;
    size_t i, color;
    for (i = 0; i != num_nodes; ++i) {
        others[i].value = i;
        others[i].next = others_begin + i + 1;
    }
    for (color = 0; others_begin != others_end; ++color) {
        candidates.push_back(others_begin->value);
        others_begin = others_begin->next;
        while (!candidates.empty()) {
            struct elem **p_j;
            const size_t i = candidates.back();
            candidates.pop_back();
            for (p_j = &others_begin; *p_j != others_end;) {
                struct elem *const j = *p_j;
                if (adjacency(i, j->value)) {
                    candidates.push_back(j->value);
                    *p_j = j->next;
                } else {
                    p_j = &j->next;
                }
            }
            colors[i] = color;
        }
    }
    return colors;
}
