#pragma once

#include <tuple>

template <typename... Ts>
class vecdicts {
    std::tuple<vecdict<Ts>...> vecdicts_;

public:
    class Proxy {
        vecdicts &vecdicts_;
        std::tuple<Ts...> key_;

    public:
        Proxy(vecdicts &vecdicts, std::tuple<Ts...> key) : vecdicts_(vecdicts), key_(std::move(key)) {}

        Proxy &operator+=(long) {
            add_to_vecdicts(std::make_index_sequence<sizeof...(Ts)>{});
            return *this;
        }

    private:
        template <std::size_t... Is>
        void add_to_vecdicts(std::index_sequence<Is...>) {
            (..., (std::get<Is>(vecdicts_.vecdicts_)[std::get<Is>(key_)] += 1));
        }
    };

    explicit vecdicts(): vecdicts_(vecdict<Ts>()...) {}
    explicit vecdicts(long n) : vecdicts_(vecdict<Ts>(n)...) {}
    explicit vecdicts(std::vector<Ts>... vecs) : vecdicts_(vecdict<Ts>(vecs)...) {}

    auto size() { if constexpr (sizeof...(Ts) > 0) { return std::get<0>(vecdicts_).size(); } else { return 0; } }

    Proxy operator[](std::tuple<Ts...> key) { return Proxy(*this, key); }

    auto begin() { return iterator(this, 0); }
    auto end() { return iterator(this, this->size()); }

private:
    class iterator {
        vecdicts* parent_;
        size_t index_;

    public:
        using value_type = std::tuple<Ts&...>;

        iterator(vecdicts* parent, const size_t index) : parent_(parent), index_(index) {}

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
            return std::tie(std::get<Is>(parent_->vecdicts_).vec_[index_]...);
        }
    };
};
