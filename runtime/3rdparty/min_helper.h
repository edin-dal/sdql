#pragma once

inline void min_inplace(long& a, const long& b) {
  a = (a == 0) ? b : std::min(a, b);
}

inline void min_inplace(double& a, const double& b) {
  a = (a == 0) ? b : std::min(a, b);
}

inline void min_inplace(std::string& a, const std::string& b) {
  a = a.empty() ? b : std::min(a, b);
}

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
