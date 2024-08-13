#pragma once

#include <chrono>
#include <fstream>
#include <iomanip>

#include "3rdparty/parallel_hashmap/phmap.h"
#include "3rdparty/rapidcsv.h"

// from sdlqpy's runtime
#include "constant_string.h"
#include "high_precision_timer.h"
#include "map_helper.h"
#include "tuple_helper.h"
#include "varchar.h"
#include "vector_helper.h"

#include "bench_helper.h"
#include "csv_helper.h"
#include "min_helper.h"
#include "vecdict.h"

constexpr long DEFAULT_VEC_SIZE = 6000001;
