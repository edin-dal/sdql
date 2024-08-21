#pragma once

// prevents dead-code elimination, clang-only (https://stackoverflow.com/a/36781982)
template <class T>
__attribute__((__optnone__)) void doNotOptimiseAway(T&& datum) {
}
