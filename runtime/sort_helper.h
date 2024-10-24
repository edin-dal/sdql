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
bool tuple_cmp(const std::tuple<T...>& a, const std::tuple<T...>& b) {
    return std::get<N>(a) < std::get<N>(b);
}

template <size_t N, typename... T>
std::vector<std::tuple<T...>> sort_vec(std::vector<std::tuple<T...>> vec) {
    std::sort(vec.begin(), vec.end(), tuple_cmp<N, T...>);
    return vec;
}
