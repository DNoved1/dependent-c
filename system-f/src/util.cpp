#include "system-f-c/util.h"

using std::string;
using std::unordered_set;

namespace util {

string gensym(string base, unordered_set<string> not_in) {
    while (set_contains(not_in, base)) {
        base += '_';
    }

    return base;
}

} /* namespace util */
