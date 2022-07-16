#define Architecture NallArchitecture
#include <nall/nall.hpp>
using namespace nall;
using string_vector = vector<string>;
#undef Architecture

#ifdef NDEBUG
#define debug(...)
#else
#define debug(...) print(stderr, __VA_ARGS__)
#endif

#include "core/core.hpp"
#include "architecture/architecture.hpp"
#include "architecture/table/table.hpp"
