#pragma once

#include <vector>

using namespace std;

class Range {
private:
	size_t ll_;
	size_t rr_;

public:
	Range() : ll_(-1), rr_(-1) {}

	class Proxy {
		Range &range_;

	public:
		Proxy(Range &range) : range_(range) {}

		Proxy &operator+=(int) {
			++range_.rr_;
			return *this;
		}
	};

	inline Proxy operator[](size_t idx) {
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
private:
	vector<KT> keys_;
	vector<VT> vals_;

public:
	SortedDict() = default;

	SortedDict(size_t n) {
		keys_.reserve(n);
		vals_.reserve(n);
	}

	inline size_t size() const {
		return keys_.size();
	}

	inline VT &operator[](const KT &key) {
		if (keys_.empty() || key != keys_.back()) {
			keys_.push_back(key);
			vals_.push_back(VT());
		}
		return vals_.back();
	}

	inline typename vector<VT>::iterator find(const KT &key) {
		auto it = lower_bound(keys_.begin(), keys_.end(), key);
		if (it != keys_.end() && *it == key)
			return vals_.begin() + (it - keys_.begin());
		return vals_.end();
	}

	inline typename vector<VT>::const_iterator find(const KT &key) const {
		auto it = lower_bound(keys_.begin(), keys_.end(), key);
		if (it != keys_.end() && *it == key)
			return vals_.begin() + (it - keys_.begin());
		return vals_.end();
	}

	inline typename vector<VT>::iterator begin() {
		return vals_.begin();
	}

	inline typename vector<VT>::iterator end() {
		return vals_.end();
	}

	inline typename vector<VT>::const_iterator begin() const {
		return vals_.begin();
	}

	inline typename vector<VT>::const_iterator end() const {
		return vals_.end();
	}
};