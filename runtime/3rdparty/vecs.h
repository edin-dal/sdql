#pragma once

#include <tuple>
#include <vector>

template <typename... Ts>
class vecs {
    std::tuple<std::vector<Ts>...> vecs_;

public:
    explicit vecs(): vecs_(std::vector<Ts>()...) {}
    explicit vecs(long n) : vecs_(std::vector<Ts>(n)...) {}
    explicit vecs(std::vector<Ts>... vecs) : vecs_(vecs...) {}

    auto size() { if constexpr (sizeof...(Ts) > 0) { return std::get<0>(vecs_).size(); } else { return 0; } }

    template <class Tuple>
    void push_back(Tuple t) { push_back_impl(std::make_index_sequence<std::tuple_size<Tuple>::value>{}, t); }

    void push_back(Ts... inputs) {
        push_back_impl(std::make_index_sequence<sizeof...(Ts)>{}, std::forward<Ts>(inputs)...);
    }

    auto begin() { return iterator(this, 0); }
    auto end() { return iterator(this, this->size()); }

private:
    template <int N, typename T>
    void push_back(T t) { return std::get<N>(vecs_).push_back(t); }

    template <std::size_t... Is>
    void push_back_impl(std::index_sequence<Is...>, Ts&&... inputs) { (push_back<Is>(std::forward<Ts>(inputs)), ...); }

    template <std::size_t... Is, class Tuple>
    void push_back_impl(std::index_sequence<Is...>, Tuple t) { (std::get<Is>(vecs_).push_back(std::get<Is>(t)), ...); }

    class iterator {
        vecs* parent_;
        size_t index_;

    public:
        using value_type = std::tuple<Ts&...>;

        iterator(vecs* parent, size_t index) : parent_(parent), index_(index) {}

        iterator& operator++() {
            ++index_;
            return *this;
        }

        iterator operator++(int) {
            iterator temp = *this;
            ++index_;
            return temp;
        }

        bool operator==(const iterator& other) const {
            return index_ == other.index_;
        }

        bool operator!=(const iterator& other) const {
            return !(*this == other);
        }

        value_type operator*() const {
            return dereference(std::make_index_sequence<sizeof...(Ts)>{});
        }

    private:
        template <std::size_t... Is>
        value_type dereference(std::index_sequence<Is...>) const {
            return std::tie(std::get<Is>(parent_->vecs_)[index_]...);
        }
    };
};
