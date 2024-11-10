#pragma once

#include "vector"
#include "smallvecdict.h"
#include "sorted_dict.h"

// compares trie with one level of nesting to unsorted version
template<size_t N>
auto to_unsorted(const SortedDict<int, Range> & trie0, const vector<int>& offsets) {
    phmap::flat_hash_map<int, smallvecdict<int, N>> unsorted(trie0.size());
    for (const auto& [x0, trie1] : trie0) {
        for (auto i = trie1.left(); i < trie1.right(); ++i) {
            unsorted[x0][offsets[i]] += 1;
        }
    }
    return unsorted;
}
