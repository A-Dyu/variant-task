#pragma once
#include <algorithm>

template <typename... Types> struct variant;

constexpr size_t variant_npos = std::numeric_limits<size_t>::max();

template <typename T> struct in_place_type_t {};

template <size_t I> struct in_place_index_t {};

template <typename T> inline constexpr in_place_type_t<T> in_place_type;

template <size_t I> inline constexpr in_place_index_t<I> in_place_index;

template <typename T> struct is_in_place_type { static constexpr bool value = false; };

template <typename T> struct is_in_place_type<in_place_type_t<T>> { static constexpr bool value = true; };

template <typename T> struct is_in_place_index { static constexpr bool value = false; };

template <size_t N> struct is_in_place_index<in_place_index_t<N>> { static constexpr bool value = true; };

struct bad_variant_access : public std::exception {};

template <size_t N, typename... Rest> struct nth_type { using type = void; };

template <size_t N, typename T, typename... Rest> struct nth_type<N, T, Rest...> {
  using rest = nth_type<N - 1, Rest...>;
  using type = typename rest::type;
  template <typename Type>
  static constexpr size_t type_index = (std::is_same_v<Type, T> ? N : rest::template type_index<Type>);
};

template <typename T, typename... Rest> struct nth_type<0, T, Rest...> {
  using type = T;

  template <typename Type> static constexpr size_t type_index = (std::is_same_v<Type, T> ? 0 : variant_npos);
};

template <typename... Types> using all_types = nth_type<sizeof...(Types) - 1, Types...>;

template <size_t N, typename... Types> using nth_type_t = typename nth_type<N, Types...>::type;

template <typename T, typename... Types>
constexpr size_t type_index = sizeof...(Types) - all_types<Types...>::template type_index<T> - 1;

template <typename... Types> using first_type = nth_type_t<0, Types...>;

template <typename T, typename... Types> constexpr bool only_once = (static_cast<size_t>(std::is_same_v<T, Types>) + ...) == 1;

template <typename Type, typename... Types> struct overload_set { static void get_overload(void); };

template<typename T_i> struct Arr { T_i arr[1]; };

template <typename Type, typename T, typename... Types>
struct overload_set<Type, T, Types...> : overload_set<Type, Types...> {
  using overload_set<Type, Types...>::get_overload;

  template <typename _T = T,
            std::enable_if_t<(!std::is_same_v<std::decay_t<_T>, bool> || std::is_same_v<std::decay_t<Type>, bool>),
                             bool> = true, typename = std::void_t<decltype(Arr<_T>{{std::declval<Type>()}})>>
  static T get_overload(T);
};

template <size_t, typename> struct variant_alternative;

template <size_t N, typename... Types>
struct variant_alternative<N, variant<Types...>> {
  using type = nth_type_t<N, Types...>;
};

template <size_t N, typename Var> using variant_alternative_t = typename variant_alternative<N, Var>::type;

template <size_t N, typename Var> struct variant_alternative<N, const Var> {
  using type = const variant_alternative_t<N, Var>;
};

template <typename...> struct variant_size;

template <typename... Types>
struct variant_size<variant<Types...>> : std::integral_constant<std::size_t, sizeof...(Types)> {};

template <typename Var> struct variant_size<const Var> : variant_size<Var> {};

template <typename Var> inline constexpr size_t variant_size_v = variant_size<Var>::value;
