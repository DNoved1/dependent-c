#ifndef SYSTEM_F_C_UTIL_H
#define SYSTEM_F_C_UTIL_H

#include <unordered_set>

namespace util {

template <typename T>
void set_union(std::unordered_set<T>& dst, const std::unordered_set<T>& src) {
    dst.insert(src.begin(), src.end());
}

template <typename T>
bool set_contains(const std::unordered_set<T>& set, const T& member) {
    return set.find(member) != set.end();
}

template <typename T>
class Lazy {
    boost::optional<T> data;
    std::function<T()> thunk;

  public:
    Lazy(std::function<T()> thunk) : data(), thunk(thunk) {}

    bool is_forced() const {
        return (bool)data;
    }

    T& force() {
        if (!data) {
            data = thunk();
        }

        return data.get();
    }
};

} /* namespace util */

#endif /* SYSTEM_F_C_UTIL_H */
