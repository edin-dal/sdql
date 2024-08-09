#pragma once

#include <chrono>
#include <fstream>
#include <iomanip>

#include "3rdparty/parallel_hashmap/phmap.h"
#include "3rdparty/rapidcsv.h"

using namespace std;
#include "3rdparty/tuple_helper.h"
#include "3rdparty/vector_helper.h"
#include "3rdparty/map_helper.h"
#include "3rdparty/varchar.h"
#include "3rdparty/high_precision_timer.h"

#include "3rdparty/vecdict.h"
#include "3rdparty/min_helper.h"
#include "3rdparty/converters.h"

const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
const auto SEPARATOR = rapidcsv::SeparatorParams('|');
