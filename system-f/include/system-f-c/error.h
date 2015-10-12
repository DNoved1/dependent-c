#ifndef SYSTEM_F_C_ERROR_H
#define SYSTEM_F_C_ERROR_H

#include <boost/variant/variant.hpp>

namespace error {

    template <typename Error, typename Result>
    class Except {
        static_assert(!std::is_same<Error, Result>::value,
            "Exception must have different types "
            "for its error and result types.");

        boost::variant<Error, Result> value;

      public:
        Except(Error error) : value(error) {}
        Except(Result result) : value(result) {}

        operator bool();

        template <typename T, typename ErrorFunc, typename ResultFunc>
        T match(ErrorFunc ef, ResultFunc rf);

        template <typename HandlerFunc>
        Except<Error, Result> catchError(HandlerFunc handler);

        template <typename Result2, typename MapFunc>
        Except<Error, Result2> bind(MapFunc f);
    };

    /*************************************************************************\
     * Wrap an error into an exception. The type of the result cannot be     *
     * inferred, so it must be manually specified.                           *
     *                                                                       *
     * Example:                                                              *
     *   auto except = make_error<ResultClass>(error_object);                *
     *************************************************************************/
    template <typename Result, typename Error>
    Except<Error, Result> make_error(Error e) {
        return Except<Error, Result>(e);
    }

    /*************************************************************************\
     * Wrap a result into an exception. The type of the error cannot be      *
     * inferred, so it must be manually specified.                           *
     *                                                                       *
     * Example:                                                              *
     *   auto except = make_result<ErrorClass>(result_object);               *
     *************************************************************************/
    template <typename Error, typename Result>
    Except<Error, Result> make_result(Result r) {
        return Except<Error, Result>(r);
    }

    /*************************************************************************\
     * Convert an exception into a boolean. Errors evaluate to false, and    *
     * results evaluate to true.                                             *
    \*************************************************************************/
    template <typename Error, typename Result>
    Except<Error, Result>::operator bool() {
        return match<bool>(
            [](auto _) { return false; },
            [](auto _) { return true; });
    }

    /*************************************************************************\
     * Pattern match on the value of an exception.                           *
    \*************************************************************************/
    template <typename Error, typename Result>
    template <typename T, typename ErrorFunc, typename ResultFunc>
    T Except<Error, Result>::match(ErrorFunc ef, ResultFunc rf) {
        struct Visitor : public boost::static_visitor<T> {
            ErrorFunc& ef;
            ResultFunc& rf;
            Visitor(ErrorFunc& ef, ResultFunc& rf) : ef(ef), rf(rf) {}
            T operator()(Error& e) const { return ef(e); }
            T operator()(Result& r) const { return rf(r); }
        };

        return boost::apply_visitor(Visitor(ef, rf), this->value);
    }

    /*************************************************************************\
     * Map over the error value, leaving the result value the same.          *
    \*************************************************************************/
    template <typename Error, typename Result>
    template <typename HandlerFunc>
    Except<Error, Result> Except<Error, Result>::catchError(HandlerFunc handler) {
        return match<Except<Error, Result>>(
            [&handler](Error& e) { return handler(e); },
            [this](Result& r) { return *this; });
    }

    /*************************************************************************\
     * Map over the result value, leaving the error value the same.          *
    \*************************************************************************/
    template <typename Error, typename Result>
    template <typename Result2, typename MapFunc>
    Except<Error, Result2> Except<Error, Result>::bind(MapFunc f) {
        return match<Except<Error, Result2>>(
            [](Error& e) { return Except<Error, Result2>(e); },
            [&f](Result& r) { return f(r); });
    }

}

#endif /* SYSTEM_F_C_ERROR_H */
