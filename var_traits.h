#pragma once
#include "var_utils.h"
#include <type_traits>

namespace var_traits {
template <typename... Types> constexpr bool trivial_destructor = (std::is_trivially_destructible_v<Types> && ...);

template <typename... Types> constexpr bool default_constructor = std::is_default_constructible_v<first_type<Types...>>;

template <typename... Types>
constexpr bool noexcept_default_constructor = std::is_nothrow_default_constructible_v<first_type<Types...>>;

template <typename... Types> constexpr bool copy_constructor = (std::is_copy_constructible_v<Types> && ...);

template <typename... Types>
constexpr bool trivial_copy_constructor = (std::is_trivially_copy_constructible_v<Types> && ...);

template <typename... Types>
constexpr bool noexcept_copy_constructor = (std::is_nothrow_copy_constructible_v<Types> && ...);

template <typename... Types> constexpr bool move_constructor = (std::is_move_constructible_v<Types> && ...);

template <typename... Types>
constexpr bool trivial_move_constructor = (std::is_trivially_move_constructible_v<Types> && ...);

template <typename... Types>
constexpr bool noexcept_move_constructor = (std::is_nothrow_move_constructible_v<Types> && ...);

template <typename... Types>
constexpr bool copy_assign = (std::is_copy_assignable_v<Types> && ...) && (std::is_copy_constructible_v<Types> && ...);

template <typename... Types>
constexpr bool trivial_copy_assign = (std::is_trivially_copy_assignable_v<Types> && ...) &&
                                     (std::is_trivially_copy_constructible_v<Types> && ...) &&
                                     (std::is_trivially_destructible_v<Types> && ...);

template <typename... Types>
constexpr bool noexcept_copy_assign = (std::is_nothrow_copy_assignable_v<Types> && ...) &&
                                      (std::is_nothrow_copy_constructible_v<Types> && ...) &&
                                      (std::is_nothrow_destructible_v<Types> && ...);

template <typename... Types>
constexpr bool move_assign = (std::is_move_assignable_v<Types> && ...) && (std::is_move_constructible_v<Types> && ...);

template <typename... Types>
constexpr bool trivial_move_assign = (std::is_trivially_move_assignable_v<Types> && ...) &&
                                     (std::is_trivially_move_constructible_v<Types> && ...) &&
                                     (std::is_trivially_destructible_v<Types> && ...);

template <typename... Types>
constexpr bool noexcept_move_assign = (std::is_nothrow_move_assignable_v<Types> && ...) &&
                                      (std::is_nothrow_move_constructible_v<Types> && ...) &&
                                      (std::is_nothrow_destructible_v<Types> && ...);

template <typename T, typename Type>
constexpr bool noexcept_cast_constructor = std::is_nothrow_constructible_v<T, Type &&>;

template <typename Type, typename... Types>
constexpr bool cast_constructor = (sizeof...(Types) > 0) && !std::is_same_v<std::decay_t<Type>, variant<Types...>> &&
                                  !is_in_place_type<std::decay_t<Type>>::value &&
                                  !is_in_place_index<std::decay_t<Type>>::value &&
                                  (std::is_constructible_v<Types, Type &&> || ...);

template <typename Type, typename... Types> constexpr bool cast_assign = cast_constructor<Type, Types...>;

template <typename T, typename Type, typename... Types>
constexpr bool noexcept_cast_assign =
    (std::is_nothrow_destructible_v<Types> && ...) && std::is_nothrow_constructible_v<T, Type &&> && std::is_nothrow_assignable_v<T, Type &&>;
} // namespace var_traits

