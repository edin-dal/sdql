#pragma once

#include <vector>

template<typename T>
class vecdict {
    std::vector<T> vec_;

public:
    class Proxy {
        vecdict &vecdict_;
        T key_;

    public:
        Proxy(vecdict &vecdict, T key) : vecdict_(vecdict), key_(key) {}

        Proxy &operator+=(long) { vecdict_.vec_.push_back(key_); return *this; }
    };

    vecdict() : vec_(std::vector<T>()) {}
    explicit vecdict(std::vector<T> vec) : vec_(std::move(vec)) {}

    [[nodiscard]] size_t size() const { return vec_.size(); }

    Proxy operator[](T key) { return Proxy(*this, key); }

    typename std::vector<T>::iterator begin() { return vec_.begin(); }
    typename std::vector<T>::iterator end() { return vec_.end(); }
};
