#pragma once

#include <array>
#include <set>
#include <vector>

using namespace std;

template<typename T, size_t N>
class smallvec {
	array<T, N> stack_;
	vector<T> *heap_;
	size_t size_{0};

public:
	~smallvec() { delete heap_; }

	[[nodiscard]] size_t size() const { return size_; }

	void push_back(const T &value) {
		if (size_ < N) {
			stack_[size_] = value;
		} else {
			if (size_ == N) {
				heap_ = new vector<T>(stack_.begin(), stack_.end());
			}
			heap_->push_back(value);
		}
		++size_;
	}

	T &operator[](size_t pos) {
		if (size_ <= N) {
			return stack_[pos];
		}
		return heap_->at(pos);
	}

	class iterator {
		T *ptr_;

	public:
		explicit iterator(T *ptr) : ptr_(ptr) {}

		iterator &operator++() {
			++ptr_;
			return *this;
		}

		T &operator*() { return *ptr_; }

		bool operator==(const iterator &rhs) { return ptr_ == rhs.ptr_; }

		bool operator!=(const iterator &rhs) { return ptr_ != rhs.ptr_; }
	};

	class const_iterator {
		const T *ptr_;

	public:
		explicit const_iterator(const T *ptr) : ptr_(ptr) {}

		const_iterator &operator++() {
			++ptr_;
			return *this;
		}

		const T &operator*() const { return *ptr_; }

		bool operator==(const const_iterator &rhs) const { return ptr_ == rhs.ptr_; }

		bool operator!=(const const_iterator &rhs) const { return ptr_ != rhs.ptr_; }
	};

	iterator begin() {
		if (size_ <= N) {
			return iterator(stack_.data());
		}
		return iterator(heap_->data());
	}

	iterator end() {
		if (size_ <= N) {
			return iterator(stack_.data() + size_);
		}
		return iterator(heap_->data() + size_);
	}

	[[nodiscard]] const_iterator begin() const {
		if (size_ <= N) {
			return const_iterator(stack_.data());
		}
		return const_iterator(heap_->data());
	}

	[[nodiscard]] const_iterator end() const {
		if (size_ <= N) {
			return const_iterator(stack_.data() + size_);
		}
		return const_iterator(heap_->data() + size_);
	}
};

template<typename T, size_t N>
struct container_type { using type = smallvec<T, N>; };

template<typename T>
struct container_type<T, 0> { using type = std::vector<T>; };

template<typename T, size_t N>
class smallvecdict {
	using Container = typename container_type<T, N>::type;

public:
    // public so smallvecdicts can access it
	Container svec_;

	[[nodiscard]] size_t size() const { return svec_.size(); }

	class Proxy {
		smallvecdict &svecdict_;
		T key_;

	public:
		Proxy(smallvecdict &svecdict, T key) : svecdict_(svecdict), key_(std::move(key)) {}

		Proxy &operator+=(int) {
			svecdict_.svec_.push_back(key_);
			return *this;
		}
	};

	Proxy operator[](T key) {
		return Proxy(*this, key);
	}

	typename Container::iterator begin() { return svec_.begin(); }

	typename Container::iterator end() { return svec_.end(); }

	typename Container::const_iterator begin() const { return svec_.begin(); }

	typename Container::const_iterator end() const { return svec_.end(); }

	bool operator==(smallvecdict &other) {
	    return size() == other.size()
	                 && std::multiset<T>(svec_.begin(), svec_.end())
	                        == std::multiset<T>(other.svec_.begin(), other.svec_.end());
	}

	bool operator==(const smallvecdict &other) const {
		return size() == other.size()
                 && std::multiset<T>(svec_.begin(), svec_.end())
                        == std::multiset<T>(other.svec_.begin(), other.svec_.end());
	}
};
