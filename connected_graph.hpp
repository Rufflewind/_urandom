#include <unordered_set>
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
    typedef std::size_t node;
    typedef std::unordered_set<node> set;
    typedef std::vector<node> vector;
    set others;
    vector candidates, colors(num_nodes), connected;
    node color = 0;
    for (node i = 0; i != num_nodes; ++i) {
        others.insert(i);
    }
    while (!others.empty()) {
        const set::const_iterator i = others.begin();
        candidates.push_back(*i);
        others.erase(i);
        while (!candidates.empty()) {
            const node i = candidates.back();
            candidates.pop_back();
            colors[i] = color;
            // note in C++14 we can erase elements from `others` while
            // iterating through `others` (without an intermediate array)
            // assuming we use `std::unordered_set`
            for (set::const_iterator j = others.begin(),
                 j_end = others.end(); j != j_end; ++j) {
                if (adjacency(i, *j)) {
                    connected.push_back(*j);
                    candidates.push_back(*j);
                }
            }
            for (vector::const_iterator j = connected.begin(),
                 j_end = connected.end(); j != j_end; ++j) {
                others.erase(*j);
            }
            connected.clear();
        }
        ++color;
    }
    return colors;
}
