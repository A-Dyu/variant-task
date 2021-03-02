#pragma once
#include "pack_union.h"
#include "var_utils.h"
#include <utility>

template <typename Union, size_t N, typename Type> constexpr void destroy_storage(Union &&storage) {
  get(in_place_index<N>, storage).~Type();
}

template <typename T, typename Storage, typename Source, typename... Types>
void construct_storage(Storage &storage, Source &&source) {
  new (&get(in_place_index<type_index<T, Types...>>, storage))
      T(get(in_place_index<type_index<T, Types...>>, std::forward<Source>(source)));
}

template <typename Storage, typename Source, size_t N> void same_alternative_move_assign(Storage &storage, Source &&source) {
  get(in_place_index<N>, storage) = std::move(get(in_place_index<N>, std::forward<Source>(source)));
}

template <size_t N, typename T, typename Union, typename... Args> void emplace(Union &storage, Args &&... args) {
  new (const_cast<std::remove_const_t<T> *>(&get(in_place_index<N>, storage))) T(std::forward<Args>(args)...);
}

template <size_t N, typename Union> void same_alternative_swap(Union &a, Union &b) {
  using std::swap;
  swap(get(in_place_index<N>, a), get(in_place_index<N>, b));
}

template <size_t I, typename... _Types>
constexpr variant_alternative_t<I, variant<_Types...>> &get(variant<_Types...> &var);

template <size_t I, typename... _Types>
constexpr variant_alternative_t<I, variant<_Types...>> const &get(variant<_Types...> const &var);

template <size_t I, typename... _Types>
constexpr variant_alternative_t<I, variant<_Types...>> &&get(variant<_Types...> &&var);

template <typename T, size_t... Sizes> struct recursive_array {
  constexpr T const &get() const noexcept { return arr; }

  T arr;
};

template <typename T, size_t Size, size_t... Sizes> struct recursive_array<T, Size, Sizes...> {
  template <typename... Indexes> constexpr T const &get(size_t index, Indexes... indexes) const {
    return arr[index].get(indexes...);
  }

  recursive_array<T, Sizes...> arr[Size];
};

template <typename Array, typename Tuple, typename Sequence> struct visit_table_generator;

template <typename R, typename Visitor, size_t... Sizes, typename... Vars, size_t... Indexes>
struct visit_table_generator<recursive_array<R (*)(Visitor, Vars...), Sizes...>, std::tuple<Vars...>,
                             std::index_sequence<Indexes...>> {
  using array_t = recursive_array<R (*)(Visitor &&, Vars &&...), Sizes...>;

  using var = std::decay_t<nth_type_t<sizeof...(Indexes), Vars...>>;

  template <size_t Index, typename Element> static constexpr void generate_element(Element &element) {
    element = ::visit_table_generator<std::decay_t<Element>, std::tuple<Vars...>,
                                      std::index_sequence<Indexes..., Index>>::generate();
  }

  template <size_t... Index_seq>
  static constexpr void generate_array(array_t &visit_table, std::index_sequence<Index_seq...>) {
    (generate_element<Index_seq>(visit_table.arr[Index_seq]), ...);
  }

  static constexpr array_t generate() {
    array_t visit_table{};
    generate_array(visit_table, std::make_index_sequence<variant_size_v<var>>());
    return visit_table;
  }
};

template <typename R, typename Visitor, typename... Vars, size_t... Indexes>
struct visit_table_generator<recursive_array<R (*)(Visitor, Vars...)>, std::tuple<Vars...>,
                             std::index_sequence<Indexes...>> {
  using function = recursive_array<R (*)(Visitor &&, Vars &&...)>;

  static constexpr decltype(auto) visit_invoke(Visitor &&visitor, Vars &&... vars) {
    return std::forward<Visitor>(visitor)(get<Indexes>(std::forward<Vars>(vars))...);
  }

  static constexpr auto generate() { return function{&visit_invoke}; }
};

template <typename R, typename Visitor, typename... Vars> struct visit_invoke {
  using function = R (*)(Visitor &&, Vars &&...);

  using array_t = recursive_array<function, variant_size_v<std::decay_t<Vars>>...>;

  static constexpr auto table = visit_table_generator<array_t, std::tuple<Vars...>, std::index_sequence<>>::generate();
};
