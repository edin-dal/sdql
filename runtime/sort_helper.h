#pragma once

template <typename T>
std::vector<int> sorted_indices(const std::vector<T> &vec)
{
    vector<int> v(vec.size());
    iota(v.begin(), v.end(), 0);
    sort(v.begin(), v.end(), [&](const int i, const int j) { return vec[i] < vec[j]; });
    return v;
}

template <size_t N, typename... T>
std::vector<std::tuple<T...>> sort_vec(std::vector<std::tuple<T...>> vec) {
    std::sort(vec.begin(), vec.end(), [](const std::tuple<T...>& a, const std::tuple<T...>& b)
    {
        return get<N>(a) < get<N>(b);
    });
    return vec;
}
