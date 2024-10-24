#pragma once

#include <vector>

using namespace std;

class Range {
	size_t ll_;
	size_t rr_;

public:
	Range() : ll_(-1), rr_(-1) {}

	class Proxy {
		Range &range_;

	public:
		explicit Proxy(Range &range) : range_(range) {}

		Proxy &operator+=(int) {
			++range_.rr_;
			return *this;
		}
	};

	inline Proxy operator[](size_t const idx) {
		if (ll_ == -1) {
			ll_ = idx;
			rr_ = idx;
		}
		return Proxy(*this);
	}

	inline size_t left() const {
		return ll_;
	}

	inline size_t right() const {
		return rr_;
	}
};

template<typename KT, typename VT>
class SortedDict {
    vector<pair<KT, VT>> data_;

public:
	SortedDict() = default;

	explicit SortedDict(size_t n) {
		data_.reserve(n);
	}

	inline size_t size() const {
		return data_.size();
	}

	inline VT &operator[](const KT &key) {
		if (data_.empty() || key != data_.back().first) {
            data_.emplace_back(key, VT());
		}
		return data_.back().second;
	}

    inline VT &at(const KT &key) {
        auto it = lower_bound(data_.begin(), data_.end(), key, [](const pair<KT, VT>& a, const KT& cmp_key) {
            return a.first < cmp_key;
        });
        if (it == data_.end()) {
            data_.emplace_back(key, VT());
            return data_.back().second;
        }
        if (it->first != key)
            data_.emplace(it, key, VT());
        return it->second;
    }

    inline typename vector<pair<KT, VT>>::iterator find(const KT &key) {
        auto it = lower_bound(data_.begin(), data_.end(), key, [](const pair<KT, VT>& a, const KT& cmp_key) {
            return a.first < cmp_key;
        });
        if (it != data_.end() && it->first == key)
            return it;
        return data_.end();
    }

    inline typename vector<pair<KT, VT>>::const_iterator find(const KT &key) const {
        auto it = lower_bound(data_.begin(), data_.end(), key, [](const pair<KT, VT>& a, const KT& cmp_key) {
            return a.first < cmp_key;
        });
        if (it != data_.end() && it->first == key)
            return it;
        return data_.end();
    }

    inline typename vector<pair<KT, VT>>::iterator begin() {
        return data_.begin();
    }

    inline typename vector<pair<KT, VT>>::iterator end() {
        return data_.end();
    }

    inline typename vector<pair<KT, VT>>::const_iterator begin() const {
        return data_.begin();
    }

    inline typename vector<pair<KT, VT>>::const_iterator end() const {
        return data_.end();
    }

	// TODO convenience method - get rid of this
	inline bool contains(const KT &key) {
		return this->find(key) != this->end();
	}
};