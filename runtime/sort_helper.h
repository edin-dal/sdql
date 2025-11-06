#pragma once

template <typename T, typename... Ts>
std::vector<int> sorted_indices(const std::vector<T>& vec0, const std::vector<Ts>&... vecs)
{
    // Ensure all vectors are the same size
    const std::size_t size = vec0.size();
    assert(((vecs.size() == size) && ...));

    // Initialize the indices vector
    std::vector<int> indices(size);
    std::iota(indices.begin(), indices.end(), 0);

    // Sort indices based on multi-vector lexicographic order
    std::sort(indices.begin(), indices.end(), [&](int i, int j) {
        return std::tie(vec0[i], vecs[i]...) < std::tie(vec0[j], vecs[j]...);
    });

    return indices;
}

template <size_t N, typename... T>
std::vector<std::tuple<T...>> sort_vec(std::vector<std::tuple<T...>> vec) {
    std::sort(vec.begin(), vec.end(), [](const std::tuple<T...>& a, const std::tuple<T...>& b)
    {
        return get<N>(a) < get<N>(b);
    });
    return vec;
}
