#pragma once

#include "var_utils.h"

template <typename T, bool Is_trivial_destructible = std::is_trivially_destructible_v<T>> struct in_pack_storage_t;

template <typename T> struct in_pack_storage_t<T, true> {
  template <typename... Args> constexpr in_pack_storage_t(Args &&... args) : val(std::forward<Args>(args)...) {}

  constexpr T &get() & noexcept { return val; }

  constexpr T const &get() const &noexcept { return val; }

  constexpr T &&get() && noexcept { return std::move(val); }

  constexpr const T &&get() const &&noexcept { return std::move(val); }

private:
  T val;
};

template <typename T> struct in_pack_storage_t<T, false> {
  template <typename... Args> in_pack_storage_t(Args &&... args) { new (&storage) T(std::forward<Args>(args)...); }

  T &get() & noexcept { return *reinterpret_cast<T *>(&storage); }

  T const &get() const &noexcept { return *reinterpret_cast<T const *>(&storage); }

  T &&get() && noexcept { return std::move(*reinterpret_cast<T *>(&storage)); }

  const T &&get() const &&noexcept { return std::move(*reinterpret_cast<T const *>(&storage)); }

private:
  std::aligned_storage_t<sizeof(T), alignof(T)> storage;
};

template <typename... Rest> union pack_union {};

template <typename T, typename... Rest> union pack_union<T, Rest...> {
  constexpr pack_union() : rest() {}

  template <typename... Args>
  constexpr pack_union(in_place_index_t<0>, Args &&... args) : val(std::forward<Args>(args)...) {}

  template <size_t N, typename... Args>
  constexpr pack_union(in_place_index_t<N>, Args &&... args)
      : rest(in_place_index_t<N - 1>(), std::forward<Args>(args)...) {}

  in_pack_storage_t<T> val;
  pack_union<Rest...> rest;
};

template <typename Union> constexpr decltype(auto) get(in_place_index_t<0>, Union &&storage) {
  return std::forward<Union>(storage).val.get();
}

template <size_t N, typename Union> constexpr decltype(auto) get(in_place_index_t<N>, Union &&storage) {
  return ::get(in_place_index<N - 1>, std::forward<Union>(storage).rest);
}

template <size_t N, typename Var> constexpr decltype(auto) u_get(Var &&var) {
  return get(in_place_index<N>, std::forward<Var>(var).storage);
}
