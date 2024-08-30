#pragma once

#include <tuple>

template <size_t N, typename... Ts>
class smallvecdicts {
    std::tuple<smallvecdict<Ts, N>...> smallvecdicts_;

public:
    class Proxy {
        smallvecdicts &smallvecdicts_;
        std::tuple<Ts...> key_;

    public:
        Proxy(smallvecdicts &smallvecdicts, std::tuple<Ts...> key) : smallvecdicts_(smallvecdicts), key_(std::move(key)) {}

        Proxy &operator+=(int) {
            add_to_smallvecdicts(std::make_index_sequence<sizeof...(Ts)>{});
            return *this;
        }

    private:
        template <std::size_t... Is>
        void add_to_smallvecdicts(std::index_sequence<Is...>) {
            (..., (std::get<Is>(smallvecdicts_.smallvecdicts_)[std::get<Is>(key_)] += 1));
        }
    };

    explicit smallvecdicts(): smallvecdicts_(smallvecdict<Ts, N>()...) {}
    explicit smallvecdicts(int n) : smallvecdicts_(smallvecdict<Ts, N>(n)...) {}
    explicit smallvecdicts(std::vector<Ts>... vecs) : smallvecdicts_(smallvecdict<Ts, N>(vecs)...) {}

    auto size() { if constexpr (sizeof...(Ts) > 0) { return std::get<0>(smallvecdicts_).size(); } else { return 0; } }

    Proxy operator[](std::tuple<Ts...> key) { return Proxy(*this, key); }

    auto begin() { return iterator(this, 0); }
    auto end() { return iterator(this, this->size()); }

private:
    class iterator {
        smallvecdicts* parent_;
        size_t index_;

    public:
        using value_type = std::tuple<Ts&...>;

        iterator(smallvecdicts* parent, const size_t index) : parent_(parent), index_(index) {}

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
        [[nodiscard]] value_type dereference(std::index_sequence<Is...>) const {
            return std::tie(std::get<Is>(parent_->smallvecdicts_).svec_[index_]...);
        }
    };
};
