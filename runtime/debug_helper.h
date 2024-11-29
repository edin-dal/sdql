#pragma once

#include "vector"
#include "smallvecdict.h"
#include "sorted_dict.h"

// base case: convert trie with Range values to unsorted hashmap
template<size_t N>
auto to_unsorted(const SortedDict<int, Range> & trie, const vector<int>& offsets) {
    phmap::flat_hash_map<int, smallvecdict<int, N>> unsorted(trie.size());
    for (const auto& [x, trie1] : trie)
        for (auto i = trie1.left(); i < trie1.right(); ++i)
            unsorted[x][offsets[i]] += 1;
    return unsorted;
}
// recursive case: convert trie with Range values to unsorted hashmap
template<size_t N, typename NestedDict>
auto to_unsorted(const SortedDict<int, NestedDict>& trie, const std::vector<int>& offsets) {
    using InnerMapType = decltype(to_unsorted<N>(std::declval<NestedDict>(), offsets));
    phmap::flat_hash_map<int, InnerMapType> unsorted(trie.size());
    for (const auto& [x, inner_trie] : trie)
        unsorted[x] = to_unsorted<N>(inner_trie, offsets);
    return unsorted;
}

// base case: convert trie with int values to unsorted hashmap
inline auto to_unsorted(const SortedDict<int, int>& dict) {
    return phmap::flat_hash_map<int, int>(dict.begin(), dict.end());
}
// recursive case: convert trie with int values to unsorted hashmap
template<typename NestedDict>
auto to_unsorted(const SortedDict<int, NestedDict>& sorted_dict) {
    using InnerMapType = decltype(to_unsorted(std::declval<NestedDict>()));
    phmap::flat_hash_map<int, InnerMapType> unsorted_map;
    for (const auto& [key, nested_dict] : sorted_dict)
        unsorted_map[key] = to_unsorted(nested_dict);
    return unsorted_map;
}
