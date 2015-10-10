#ifndef SYSTEM_F_C_UTIL_H
#define SYSTEM_F_C_UTIL_H

#include <boost/optional/optional.hpp>
#include <cassert>
#include <unordered_set>
#include <vector>

namespace util {

    template <typename T>
    void set_union(std::unordered_set<T>& dst, const std::unordered_set<T>& src) {
        dst.insert(src.begin(), src.end());
    }

    template <typename T>
    bool set_contains(const std::unordered_set<T>& set, const T& member) {
        return set.find(member) != set.end();
    }

    /* Unlike the resize method in vector, this does not require that T is
     * default-initializable.
     */
    template <typename T>
    void removing_resize(std::vector<T> vector, size_t new_size) {
        assert(new_size <= vector.size());

        vector.erase(vector.begin() + new_size, vector.end());
    }

    template <typename T>
    void reverse(std::vector<T>& vector) {
        if (vector.size() == 0) {
            return;
        }

        size_t start_index = 0;
        size_t end_index = vector.size() - 1;

        while (start_index < end_index) {
            std::swap(vector[start_index++], vector[end_index--]);
        }
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

    std::string gensym(std::string base, std::unordered_set<std::string> not_in);

} /* namespace util */

#endif /* SYSTEM_F_C_UTIL_H */
