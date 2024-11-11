#pragma once

#include "set"
#include "vector"
#include "smallvecdict.h"
#include "sorted_dict.h"

// compares trie with one level of nesting to unsorted version
template<size_t N>
auto to_unsorted(const SortedDict<int, Range> & trie, const vector<int>& offsets) {
    phmap::flat_hash_map<int, smallvecdict<int, N>> unsorted(trie.size());
    for (const auto& [x, trie1] : trie) {
        for (auto i = trie1.left(); i < trie1.right(); ++i) {
            unsorted[x][offsets[i]] += 1;
        }
    }
    return unsorted;
}

// compares trie with two levels of nesting to unsorted version
template<size_t N>
auto to_unsorted(const SortedDict<int, SortedDict<int, Range>>& trie, const std::vector<int>& offsets) {
    using InnerMapType = phmap::flat_hash_map<int, smallvecdict<int, N>>;
    phmap::flat_hash_map<int, InnerMapType> unsorted(trie.size());
    for (const auto& [x, inner_trie] : trie) {
        unsorted[x] = to_unsorted<N>(inner_trie, offsets);
    }
    return unsorted;
}

template<typename K>
auto keys_to_set(const phmap::flat_hash_map<K, int>& map) {
    std::set<K> kset;
    std::transform(map.begin(), map.end(), std::inserter(kset, kset.end()),
                   [](const auto& pair) { return pair.first; });
    return kset;
}
