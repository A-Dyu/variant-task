#pragma once
#include "pack_union.h"
#include "var_base.h"
#include "var_tables.h"
#include "var_utils.h"
#include <algorithm>
#include <type_traits>
#include <utility>

template <typename... Types>
struct variant : private variant_base<Types...>, default_construct_enable_t<var_traits::default_constructor<Types...>> {
  using base = variant_base<Types...>;
  using def_enable = default_construct_enable_t<var_traits::default_constructor<Types...>>;
  using base::base;

  constexpr variant() = default;

  constexpr variant(variant const &) = default;

  constexpr variant &operator=(variant const &) = default;

  constexpr variant(variant &&) = default;

  constexpr variant &operator=(variant &&) = default;

  template <size_t N, typename... Args,
          std::enable_if_t<N<sizeof...(Types) && std::is_constructible_v<nth_type_t<N, Types...>, Args...>, bool> = true>
  constexpr variant(in_place_index_t<N>, Args &&... args) :
          base(in_place_index_t<N>(), std::forward<Args>(args)...), def_enable(default_enable_init()) {}

  template <typename Type, std::enable_if_t<var_traits::cast_constructor<Type, Types...>, bool> = true,
            typename T = decltype(overload_set<Type, Types...>::get_overload(std::declval<Type &&>()))>
  constexpr variant(Type &&val) noexcept(var_traits::noexcept_cast_constructor<T, Type>)
      : base(in_place_index_t<type_index<T, Types...>>(), std::forward<Type>(val)), def_enable(default_enable_init()) {}

  template <typename T, typename... Args,
            std::enable_if_t<only_once<T, Types...> && std::is_constructible_v<T, Args...>, bool> = true>
  constexpr explicit variant(in_place_type_t<T>, Args &&... args) noexcept(std::is_nothrow_constructible_v<T, Args...>)
      : base(in_place_index_t<type_index<T, Types...>>(), std::forward<Args>(args)...),
        def_enable(default_enable_init()) {}

  template <typename Type, std::enable_if_t<var_traits::cast_assign<Type, Types...>, bool> = true,
            typename T = decltype(overload_set<Type, Types...>::get_overload(std::declval<Type &&>())),
            std::enable_if_t<std::is_assignable_v<T, Type>, bool> = true>
  variant &operator=(Type &&val) noexcept(var_traits::noexcept_cast_assign<T, Type, Types...>) {
    if (index() == type_index<T, Types...>) {
      u_get<type_index<T, Types...>>(*this) = std::forward<Type>(val);
      return *this;
    }
    if constexpr (std::is_nothrow_constructible_v<T, Type> || !std::is_trivially_move_constructible_v<T>) {
      emplace<T>(std::forward<Type>(val));
    } else {
      *this = variant(std::forward<Type>(val));
    }
    return *this;
  }

  constexpr size_t index() const noexcept { return base::index; }

  constexpr bool valueless_by_exception() const noexcept { return base::index == variant_npos; }

  template <typename T, typename... Args, typename = std::enable_if<std::is_constructible_v<T, Args...>>,
            typename = std::enable_if<only_once<T, Types...>>>
  T &emplace(Args &&... args) {
    base::destroy();
    ::emplace<type_index<T, Types...>, T, pack_union<Types...>, Args...>(base::storage, std::forward<Args>(args)...);
    base::index = type_index<T, Types...>;
    return u_get<type_index<T, Types...>>(*this);
  }

  template <size_t I, typename... Args,
            typename = std::enable_if<I<sizeof...(Types)>, typename = std::enable_if<std::is_constructible_v<
                                                               variant_alternative_t<I, variant>, Args...>>>
                variant_alternative_t<I, variant> &emplace(Args &&... args) {
    base::destroy();
    ::emplace<I, variant_alternative_t<I, variant>, pack_union<Types...>, Args...>(base::storage,
                                                                                   std::forward<Args>(args)...);
    base::index = I;
    return u_get<I>(*this);
  };

  void swap(variant &other) noexcept((std::is_trivially_move_constructible_v<Types> && ...) &&
                                     (std::is_nothrow_swappable_v<Types> && ...)) {
    if (valueless_by_exception() && other.valueless_by_exception()) {
      return;
    }
    if (valueless_by_exception()) {
      *this = std::move(other);
      other.destroy();
      return;
    }
    if (other.valueless_by_exception()) {
      other = std::move(*this);
      base::destroy();
      return;
    }
    if (index() == other.index()) {
      static std::array<void (*)(pack_union<Types...> &, pack_union<Types...> &), sizeof...(Types)> swap_table = {
          &same_alternative_swap<type_index<Types, Types...>, pack_union<Types...>>...};
      swap_table[index()](base::storage, other.storage);
    } else {
      variant safe(std::move(other));
      other = std::move(*this);
      *this = std::move(safe);
    }
  }

private:
  template <size_t N, typename Var> friend constexpr decltype(auto) u_get(Var &&);
};

template <size_t I, typename... _Types>
constexpr variant_alternative_t<I, variant<_Types...>> &get(variant<_Types...> &var) {
  if (var.index() != I) {
    throw bad_variant_access();
  }
  return u_get<I>(var);
}

template <size_t I, typename... _Types>
constexpr variant_alternative_t<I, variant<_Types...>> const &get(variant<_Types...> const &var) {
  if (var.index() != I) {
    throw bad_variant_access();
  }
  return u_get<I>(var);
}

template <size_t I, typename... _Types>
constexpr variant_alternative_t<I, variant<_Types...>> &&get(variant<_Types...> &&var) {
    if (var.index() != I) {
        throw bad_variant_access();
    }
    return std::move(u_get<I>(std::move(var)));
}

template <size_t I, typename... _Types>
constexpr variant_alternative_t<I, variant<_Types...>> const &&get(variant<_Types...> const &&var) {
    if (var.index() != I) {
        throw bad_variant_access();
    }
    return std::move(u_get<I>(std::move(var)));
}

template <typename T, typename... _Types> constexpr T &get(variant<_Types...> &var) {
  if (var.index() != type_index<T, _Types...>) {
    throw bad_variant_access();
  }
  return u_get<type_index<T, _Types...>>(var);
}

template <typename T, typename... _Types> constexpr T const &get(variant<_Types...> const &var) {
  if (var.index() != type_index<T, _Types...>) {
    throw bad_variant_access();
  }
  return u_get<type_index<T, _Types...>>(var);
}

template <typename T, typename... _Types> constexpr T &&get(variant<_Types...> &&var) {
  if (var.index() != type_index<T, _Types...>) {
    throw bad_variant_access();
  }
  return std::move(u_get<type_index<T, _Types...>>(std::move(var)));
}

template <typename T, typename... _Types> constexpr T const &&get(variant<_Types...> const &&var) {
    if (var.index() != type_index<T, _Types...>) {
        throw bad_variant_access();
    }
    return std::move(u_get<type_index<T, _Types...>>(std::move(var)));
}

template <size_t I, typename... _Types>
constexpr variant_alternative_t<I, variant<_Types...>> *get_if(variant<_Types...> *var) noexcept {
  if (var && var->index() == I) {
    return &u_get<I>(*var);
  } else {
    return nullptr;
  }
}

template <size_t I, typename... _Types>
constexpr variant_alternative_t<I, variant<_Types...>> const *get_if(variant<_Types...> const *var) noexcept {
  if (var && var->index() == I) {
    return &u_get<I>(*var);
  } else {
    return nullptr;
  }
}

template <typename T, typename... _Types> constexpr T *get_if(variant<_Types...> *var) noexcept {
  return get_if<type_index<T, _Types...>, _Types...>(var);
}

template <typename T, typename... _Types> constexpr T const *get_if(variant<_Types...> const *var) noexcept {
  return get_if<type_index<T, _Types...>, _Types...>(var);
}

template <typename Visitor, typename... Vars> constexpr decltype(auto) visit(Visitor &&visitor, Vars &&... vars) {
  if ((vars.valueless_by_exception() || ...)) {
    throw bad_variant_access();
  }
  using R = decltype(std::forward<Visitor>(visitor)(get<0>(std::forward<Vars>(vars))...));
  constexpr auto &visit_table = visit_invoke<R, Visitor &&, Vars &&...>::table;
  auto function = visit_table.get(vars.index()...);
  return (*function)(std::forward<Visitor>(visitor), std::forward<Vars>(vars)...);
}

template <typename R, typename Visitor, typename... Vars> constexpr R visit(Visitor &&visitor, Vars &&... vars) {
  if ((vars.valueless_by_exception() || ...)) {
    throw bad_variant_access();
  }
  constexpr auto &visit_table = visit_invoke<R, Visitor &&, Vars &&...>::table;
  auto function = visit_table.get(vars.index()...);
  return (*function)(std::forward<Visitor>(visitor), std::forward<Vars>(vars)...);
}

template <typename T, typename... _Types> constexpr bool holds_alternative(variant<_Types...> const &var) noexcept {
  return var.index() == type_index<T, _Types...>;
}
