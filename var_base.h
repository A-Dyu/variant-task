#pragma once
#include <array>
#include "pack_union.h"
#include "var_tables.h"
#include "var_traits.h"
#include "var_utils.h"
#include <algorithm>
#include <type_traits>
#include <utility>

template <bool Trait_v, typename... Types> struct storage_destruct_t {
  constexpr storage_destruct_t() noexcept : storage(), index(variant_npos) {}

  template <size_t N, typename... Args,
          std::enable_if_t<N<sizeof...(Types) && std::is_constructible_v<nth_type_t<N, Types...>, Args...>, bool> = true>
  constexpr storage_destruct_t(in_place_index_t<N>, Args &&... args) :
      storage(in_place_index_t<N>(), std::forward<Args>(args)...), index(N) {}

  constexpr storage_destruct_t(storage_destruct_t const &) noexcept(var_traits::noexcept_copy_constructor<Types...>) =
      default;

  constexpr storage_destruct_t &
  operator=(storage_destruct_t const &) noexcept(var_traits::noexcept_copy_assign<Types...>) = default;

  constexpr storage_destruct_t(storage_destruct_t &&) noexcept(var_traits::noexcept_move_constructor<Types...>) =
      default;

  constexpr storage_destruct_t &
  operator=(storage_destruct_t &&) noexcept(var_traits::noexcept_move_assign<Types...>) = default;

  ~storage_destruct_t() { destroy(); }

  void destroy() {
    if (index != variant_npos) {
      static std::array<void (*)(pack_union<Types...> &), sizeof...(Types)> destroy_table = {
          &destroy_storage<pack_union<Types...> &, type_index<Types, Types...>, Types>...};
      destroy_table.at(index)(storage);
    }
    index = variant_npos;
  }

  pack_union<Types...> storage;
  size_t index;
};

template <typename... Types> struct storage_destruct_t<true, Types...> {
  constexpr storage_destruct_t() noexcept : storage(), index(variant_npos) {}

  template <size_t N, typename... Args,
          std::enable_if_t<N<sizeof...(Types) && std::is_constructible_v<nth_type_t<N, Types...>, Args...>, bool> = true>
  constexpr storage_destruct_t(in_place_index_t<N>, Args &&... args) :
  storage(in_place_index_t<N>(), std::forward<Args>(args)...), index(N) {}

  constexpr storage_destruct_t(storage_destruct_t const &) = default;

  constexpr storage_destruct_t &operator=(storage_destruct_t const &) = default;

  constexpr storage_destruct_t(storage_destruct_t &&) = default;

  constexpr storage_destruct_t &operator=(storage_destruct_t &&) = default;

  ~storage_destruct_t() = default;

  constexpr void destroy() { index = variant_npos; }

  pack_union<Types...> storage;
  size_t index;
};

template <bool Trait_v, typename... Types>
struct storage_copy_construct_t : storage_destruct_t<var_traits::trivial_destructor<Types...>, Types...> {
  using base = storage_destruct_t<var_traits::trivial_destructor<Types...>, Types...>;
  using base::base;

  constexpr storage_copy_construct_t() noexcept(var_traits::noexcept_default_constructor<Types...>) = default;

  storage_copy_construct_t(storage_copy_construct_t const &other)
  noexcept(var_traits::noexcept_copy_constructor<Types...>) {
    if (other.index != variant_npos) {
      static std::array<void (*)(pack_union<Types...>&, pack_union<Types...> const&), sizeof...(Types)> construct_table = {
          &construct_storage<Types, pack_union<Types...>, pack_union<Types...> const&, Types...>...};
      construct_table[other.index](base::storage, other.storage);
      base::index = other.index;
    }
  }

  constexpr storage_copy_construct_t &operator=(storage_copy_construct_t const &) = default;

  constexpr storage_copy_construct_t(storage_copy_construct_t &&) = default;

  constexpr storage_copy_construct_t &operator=(storage_copy_construct_t &&) = default;
};

template <typename... Types>
struct storage_copy_construct_t<true, Types...>
    : storage_destruct_t<var_traits::trivial_destructor<Types...>, Types...> {
  using base = storage_destruct_t<var_traits::trivial_destructor<Types...>, Types...>;
  using base::base;

  constexpr storage_copy_construct_t() = default;

  constexpr storage_copy_construct_t(storage_copy_construct_t const &) = default;

  constexpr storage_copy_construct_t &
  operator=(storage_copy_construct_t const &) = default;

  constexpr storage_copy_construct_t(storage_copy_construct_t &&) = default;

  constexpr storage_copy_construct_t &operator=(storage_copy_construct_t &&) = default;
};

template <bool Trait_v, typename... Types>
struct storage_copy_assign_t : storage_copy_construct_t<var_traits::trivial_copy_constructor<Types...>, Types...> {
  using base = storage_copy_construct_t<var_traits::trivial_copy_constructor<Types...>, Types...>;
  using base::base;

  constexpr storage_copy_assign_t() = default;

  constexpr storage_copy_assign_t(storage_copy_assign_t const &) = default;

  storage_copy_assign_t &
  operator=(storage_copy_assign_t const &other) noexcept(var_traits::noexcept_copy_assign<Types...>) {
    if (base::index != variant_npos || other.index != variant_npos) {
      if (base::index == other.index) {
        static std::array<void (*)(pack_union<Types...> &, pack_union<Types...> const &), sizeof...(Types)> assign_table = {
            &same_alternative_move_assign<pack_union<Types...> &, pack_union<Types...> const &,
                                          type_index<Types, Types...>>...};
        assign_table[other.index](base::storage, other.storage);
        return *this;
      }
      base::destroy();
      if (other.index != variant_npos) {
        static std::array<void (*)(pack_union<Types...> &, pack_union<Types...> const &), sizeof...(Types)> construct_table = {
            &construct_storage<Types, pack_union<Types...>, pack_union<Types...> const &, Types...>...};
        construct_table[other.index](base::storage, other.storage);
        base::index = other.index;
      }
    }
    return *this;
  }

  constexpr storage_copy_assign_t(storage_copy_assign_t &&) =default;

  constexpr storage_copy_assign_t &operator=(storage_copy_assign_t &&) = default;
};

template <typename... Types>
struct storage_copy_assign_t<true, Types...>
    : storage_copy_construct_t<var_traits::trivial_copy_constructor<Types...>, Types...> {
  using base = storage_copy_construct_t<var_traits::trivial_copy_constructor<Types...>, Types...>;
  using base::base;

  constexpr storage_copy_assign_t() = default;

  constexpr storage_copy_assign_t(storage_copy_assign_t const &) = default;

  constexpr storage_copy_assign_t &
  operator=(storage_copy_assign_t const &) = default;

  constexpr storage_copy_assign_t(storage_copy_assign_t &&) = default;

  constexpr storage_copy_assign_t &operator=(storage_copy_assign_t &&) = default;
};

template <bool Trait_v, typename... Types>
struct storage_move_construct_t : storage_copy_assign_t<var_traits::trivial_copy_assign<Types...>, Types...> {
  using base = storage_copy_assign_t<var_traits::trivial_copy_assign<Types...>, Types...>;
  using base::base;

  constexpr storage_move_construct_t() = default;

  constexpr storage_move_construct_t(storage_move_construct_t const &) = default;

  constexpr storage_move_construct_t &operator=(storage_move_construct_t const &) = default;

  storage_move_construct_t(storage_move_construct_t &&other) noexcept(var_traits::noexcept_move_constructor<Types...>) {
    if (other.index != variant_npos) {
      static std::array<void (*)(pack_union<Types...> &, pack_union<Types...> &&), sizeof...(Types)> move_table = {
          &construct_storage<Types, pack_union<Types...>, pack_union<Types...>, Types...>...};
      move_table[other.index](base::storage, std::move(other.storage));
      base::index = other.index;
    }
  }

  constexpr storage_move_construct_t &operator=(storage_move_construct_t &&) = default;
};

template <typename... Types>
struct storage_move_construct_t<true, Types...>
    : storage_copy_assign_t<var_traits::trivial_copy_assign<Types...>, Types...> {
  using base = storage_copy_assign_t<var_traits::trivial_copy_assign<Types...>, Types...>;
  using base::base;

  constexpr storage_move_construct_t() = default;

  constexpr storage_move_construct_t(storage_move_construct_t const &) = default;

  constexpr storage_move_construct_t &
  operator=(storage_move_construct_t const &) = default;

  constexpr storage_move_construct_t(storage_move_construct_t &&) = default;

  constexpr storage_move_construct_t &operator=(storage_move_construct_t &&) = default;
};

template <bool Trait_v, typename... Types>
struct storage_move_assign_t : storage_move_construct_t<var_traits::trivial_move_constructor<Types...>, Types...> {
  using base = storage_move_construct_t<var_traits::trivial_move_constructor<Types...>, Types...>;
  using base::base;

  constexpr storage_move_assign_t() = default;

  constexpr storage_move_assign_t(storage_move_assign_t const &) = default;

  constexpr storage_move_assign_t &operator=(storage_move_assign_t const &) = default;

  constexpr storage_move_assign_t(storage_move_assign_t &&) = default;

  storage_move_assign_t &operator=(storage_move_assign_t &&other) noexcept(var_traits::noexcept_move_assign<Types...>) {
    if (base::index != variant_npos || other.index != variant_npos) {
      if (base::index == other.index) {
        static std::array<void (*)(pack_union<Types...> &, pack_union<Types...> &&), sizeof...(Types)> assign_table = {
            &same_alternative_move_assign<pack_union<Types...> &, pack_union<Types...> &&, type_index<Types, Types...>>...};
        assign_table[other.index](base::storage, std::move(other.storage));
        return *this;
      }
      base::destroy();
      if (other.index != variant_npos) {
        static std::array<void (*)(pack_union<Types...> &, pack_union<Types...> &&), sizeof...(Types)> move_table = {
            &construct_storage<Types, pack_union<Types...> &, pack_union<Types...> &&, Types...>...};
        move_table[other.index](base::storage, std::move(other.storage));
        base::index = other.index;
      }
    }
    return *this;
  }
};

template <typename... Types>
struct storage_move_assign_t<true, Types...>
    : storage_move_construct_t<var_traits::trivial_move_constructor<Types...>, Types...> {
  using base = storage_move_construct_t<var_traits::trivial_move_constructor<Types...>, Types...>;
  using base::base;

  constexpr storage_move_assign_t() = default;

  constexpr storage_move_assign_t(storage_move_assign_t const &) = default;

  constexpr storage_move_assign_t &operator=(storage_move_assign_t const &) = default;

  constexpr storage_move_assign_t(storage_move_assign_t &&) = default;

  constexpr storage_move_assign_t &operator=(storage_move_assign_t &&) = default;
};

struct default_enable_init {};

template <bool Enable, typename... Types> struct default_construct_enable_t {
  constexpr default_construct_enable_t() noexcept = delete;
  constexpr default_construct_enable_t(default_construct_enable_t const &) noexcept = default;
  constexpr default_construct_enable_t &operator=(default_construct_enable_t const &) noexcept = default;
  constexpr default_construct_enable_t(default_construct_enable_t &&) noexcept = default;
  constexpr default_construct_enable_t &operator=(default_construct_enable_t &&) noexcept = default;
  constexpr default_construct_enable_t(default_enable_init) noexcept {}
};

template <typename... Types> struct default_construct_enable_t<true, Types...> {
  constexpr default_construct_enable_t() noexcept = default;
  constexpr default_construct_enable_t(default_construct_enable_t const &) noexcept = default;
  constexpr default_construct_enable_t &operator=(default_construct_enable_t const &) noexcept = default;
  constexpr default_construct_enable_t(default_construct_enable_t &&) noexcept = default;
  constexpr default_construct_enable_t &operator=(default_construct_enable_t &&) noexcept = default;
  constexpr default_construct_enable_t(default_enable_init) noexcept {}
};

template <bool Enable, typename... Types> struct copy_construct_enable_t {
  constexpr copy_construct_enable_t() noexcept = default;
  constexpr copy_construct_enable_t(copy_construct_enable_t const &) noexcept = delete;
  constexpr copy_construct_enable_t &operator=(copy_construct_enable_t const &) noexcept = default;
  constexpr copy_construct_enable_t(copy_construct_enable_t &&) noexcept = default;
  constexpr copy_construct_enable_t &operator=(copy_construct_enable_t &&) noexcept = default;
};

template <typename... Types> struct copy_construct_enable_t<true, Types...> {
  constexpr copy_construct_enable_t() noexcept = default;
  constexpr copy_construct_enable_t(copy_construct_enable_t const &) noexcept = default;
  constexpr copy_construct_enable_t &operator=(copy_construct_enable_t const &) noexcept = default;
  constexpr copy_construct_enable_t(copy_construct_enable_t &&) noexcept = default;
  constexpr copy_construct_enable_t &operator=(copy_construct_enable_t &&) noexcept = default;
};

template <bool Enable, typename... Types> struct move_construct_enable_t {
  constexpr move_construct_enable_t() noexcept = default;
  constexpr move_construct_enable_t(move_construct_enable_t const &) noexcept = default;
  constexpr move_construct_enable_t &operator=(move_construct_enable_t const &) noexcept = default;
  constexpr move_construct_enable_t(move_construct_enable_t &&) noexcept = delete;
  constexpr move_construct_enable_t &operator=(move_construct_enable_t &&) noexcept = default;
};

template <typename... Types> struct move_construct_enable_t<true, Types...> {
  constexpr move_construct_enable_t() noexcept = default;
  constexpr move_construct_enable_t(move_construct_enable_t const &) noexcept = default;
  constexpr move_construct_enable_t &operator=(move_construct_enable_t const &) noexcept = default;
  constexpr move_construct_enable_t(move_construct_enable_t &&) noexcept = default;
  constexpr move_construct_enable_t &operator=(move_construct_enable_t &&) noexcept = default;
};

template <bool Enable, typename... Types> struct copy_assign_enable_t {
  constexpr copy_assign_enable_t() noexcept = default;
  constexpr copy_assign_enable_t(copy_assign_enable_t const &) noexcept = default;
  constexpr copy_assign_enable_t &operator=(copy_assign_enable_t const &) noexcept = delete;
  constexpr copy_assign_enable_t(copy_assign_enable_t &&) noexcept = default;
  constexpr copy_assign_enable_t &operator=(copy_assign_enable_t &&) noexcept = default;
};

template <typename... Types> struct copy_assign_enable_t<true, Types...> {
  constexpr copy_assign_enable_t() noexcept = default;
  constexpr copy_assign_enable_t(copy_assign_enable_t const &) noexcept = default;
  constexpr copy_assign_enable_t &operator=(copy_assign_enable_t const &) noexcept = default;
  constexpr copy_assign_enable_t(copy_assign_enable_t &&) noexcept = default;
  constexpr copy_assign_enable_t &operator=(copy_assign_enable_t &&) noexcept = default;
};

template <bool Enable, typename... Types> struct move_assign_enable_t {
  constexpr move_assign_enable_t() noexcept = default;
  constexpr move_assign_enable_t(move_assign_enable_t const &) noexcept = default;
  constexpr move_assign_enable_t &operator=(move_assign_enable_t const &) noexcept = default;
  constexpr move_assign_enable_t(move_assign_enable_t &&) noexcept = default;
  constexpr move_assign_enable_t &operator=(move_assign_enable_t &&) noexcept = delete;
};

template <typename... Types> struct move_assign_enable_t<true, Types...> {
  constexpr move_assign_enable_t() noexcept = default;
  constexpr move_assign_enable_t(move_assign_enable_t const &) noexcept = default;
  constexpr move_assign_enable_t &operator=(move_assign_enable_t const &) noexcept = default;
  constexpr move_assign_enable_t(move_assign_enable_t &&) noexcept = default;
  constexpr move_assign_enable_t &operator=(move_assign_enable_t &&) noexcept = default;
};

template <typename... Types>
struct variant_base : storage_move_assign_t<var_traits::trivial_move_assign<Types...>, Types...>,
                      copy_construct_enable_t<var_traits::copy_constructor<Types...>, Types...>,
                      copy_assign_enable_t<var_traits::copy_assign<Types...>, Types...>,
                      move_construct_enable_t<var_traits::move_constructor<Types...>, Types...>,
                      move_assign_enable_t<var_traits::move_assign<Types...>, Types...> {
  using base = storage_move_assign_t<var_traits::trivial_move_assign<Types...>, Types...>;

  using base::base;

  constexpr variant_base() noexcept(var_traits::noexcept_default_constructor<Types...>) : base(in_place_index<0>) {}
  constexpr variant_base(variant_base const &) = default;
  constexpr variant_base &operator=(variant_base const &) = default;
  constexpr variant_base(variant_base &&) = default;
  constexpr variant_base &operator=(variant_base &&) = default;
};
