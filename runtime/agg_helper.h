#pragma once

const auto STRING_MAX = std::string("zzzzzzzzzzzzzzzzzz");

template <typename T>
void min_inplace(T& a, const T& b) { a = std::min(a, b); }

namespace internal
{
  template<typename T, size_t... Is>
  void min(T& t1, const T& t2, std::integer_sequence<size_t, Is...>)
  {
    auto l = { (min_inplace(std::get<Is>(t1), std::get<Is>(t2)), 0)... };
    (void)l; // prevent unused warning
  }
}

template <typename...T>
void min_inplace (std::tuple<T...>& lhs, const std::tuple<T...>& rhs)
{
  internal::min(lhs, rhs, std::index_sequence_for<T...>{});
}

template <typename Container>
auto min_key(const Container& container) {
  return std::min_element(
             container.begin(),
             container.end(),
             [](const auto &p1, const auto &p2) { return p1.first < p2.first; }
             )->first;
}

template <typename T>
void max_inplace(T& a, const T& b) { a = std::max(a, b); }

namespace internal
{
  template<typename T, size_t... Is>
  void max(T& t1, const T& t2, std::integer_sequence<size_t, Is...>)
  {
    auto l = { (max_inplace(std::get<Is>(t1), std::get<Is>(t2)), 0)... };
    (void)l; // prevent unused warning
  }
}

template <typename...T>
void max_inplace (std::tuple<T...>& lhs, const std::tuple<T...>& rhs)
{
  internal::max(lhs, rhs, std::index_sequence_for<T...>{});
}

template <typename Container>
auto max_key(const Container& container) {
  return std::max_element(
             container.begin(),
             container.end(),
             [](const auto &p1, const auto &p2) { return p1.first < p2.first; }
             )->first;
}
