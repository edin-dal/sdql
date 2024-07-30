#pragma once

#include <array>
#include <iostream>
#include <vector>

using namespace std;

template<typename T, size_t N>
class smallvec {
	array<T, N> stack_;
	vector<T> *heap_;
	size_t size_{0};

public:
	~smallvec() { delete heap_; }

	inline size_t size() const { return size_; }

	inline void push_back(const T &value) {
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

	inline T &operator[](size_t pos) {
		if (size_ <= N) {
			return stack_[pos];
		} else {
			return heap_->at(pos);
		}
	}

	class iterator {
		T *ptr_;

	public:
		iterator(T *ptr) : ptr_(ptr) {}

		iterator &operator++() {
			++ptr_;
			return *this;
		}

		inline T &operator*() { return *ptr_; }

		inline bool operator==(const iterator &rhs) { return ptr_ == rhs.ptr_; }

		inline bool operator!=(const iterator &rhs) { return ptr_ != rhs.ptr_; }
	};

	class const_iterator {
		T *ptr_;

	public:
		const_iterator(T *ptr) : ptr_(ptr) {}

		const_iterator &operator++() {
			++ptr_;
			return *this;
		}

		inline const T &operator*() { return *ptr_; }

		inline bool operator==(const const_iterator &rhs) { return ptr_ == rhs.ptr_; }

		inline bool operator!=(const const_iterator &rhs) { return ptr_ != rhs.ptr_; }
	};

	inline iterator begin() {
		if (size_ <= N) {
			return iterator(stack_.data());
		} else {
			return iterator(heap_->data());
		}
	}

	inline iterator end() {
		if (size_ <= N) {
			return iterator(stack_.data() + size_);
		} else {
			return iterator(heap_->data() + size_);
		}
	}

	inline const_iterator begin() const {
		if (size_ <= N) {
			return const_iterator(stack_.data());
		} else {
			return const_iterator(heap_->data());
		}
	}

	inline const_iterator end() const {
		if (size_ <= N) {
			return const_iterator(stack_.data() + size_);
		} else {
			return const_iterator(heap_->data() + size_);
		}
	}
};

template<typename T, size_t N>
class smallvecdict {
	smallvec<T, N> svec_;

public:
	inline size_t size() const { return svec_.size(); }

	class Proxy {
		smallvecdict<T, N> &svecdict_;
		T key_;

	public:
		Proxy(smallvecdict &svecdict, T key) : svecdict_(svecdict), key_(key) {}

		inline Proxy &operator+=(bool) {
			svecdict_.svec_.push_back(key_);
			return *this;
		}
	};

	inline Proxy operator[](T key) {
		return Proxy(*this, key);
	}

	inline typename smallvec<T, N>::iterator begin() { return svec_.begin(); }

	inline typename smallvec<T, N>::iterator end() { return svec_.end(); }
};
