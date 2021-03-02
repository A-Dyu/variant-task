#pragma once

#include "gtest/gtest.h"

template <class... Ts> struct overload : Ts... { using Ts::operator()...; };
template <class... Ts> overload(Ts...) -> overload<Ts...>;

struct dummy_t {};

struct no_default_t {
  no_default_t() = delete;
};

struct throwing_default_t {
  throwing_default_t() { throw std::exception(); }
};

struct throwing_move_operator_t {
  static size_t swap_called;
  throwing_move_operator_t() = default;
  throwing_move_operator_t(throwing_move_operator_t &&) noexcept(false) { throw std::exception(); }
  throwing_move_operator_t &operator=(throwing_move_operator_t &&) = default;
};

inline size_t throwing_move_operator_t::swap_called = 0;

void swap(throwing_move_operator_t &, throwing_move_operator_t &) {
  throwing_move_operator_t::swap_called += 1;
}

struct no_copy_t {
  no_copy_t(const no_copy_t &) = delete;
};

struct no_move_t {
  no_move_t(no_move_t &&) = delete;
};

struct non_trivial_copy_t {
  explicit non_trivial_copy_t(int x) noexcept : x{x} {}
  non_trivial_copy_t(const non_trivial_copy_t &other) noexcept : x{other.x + 1} {}

  int x;
};

struct non_trivial_copy_assignment_t {
  explicit non_trivial_copy_assignment_t(int x) noexcept : x{x} {}
  non_trivial_copy_assignment_t &operator=(const non_trivial_copy_assignment_t &other) {
    if (this != &other) {
      x = other.x + 5;
    }
    return *this;
  };

  int x;
};

struct non_trivial_int_wrapper_t {
  non_trivial_int_wrapper_t(int x) : x{x} {}
  non_trivial_int_wrapper_t &operator=(int i) {
    x = i + 1;
    return *this;
  }
  int x;
};

struct no_move_assignment_t {
  no_move_assignment_t &operator=(no_move_assignment_t &&) = delete;
};

struct no_copy_assignment_t {
  no_copy_assignment_t &operator=(const no_copy_assignment_t &) = delete;
};

struct throwing_move_assignment_t {
  throwing_move_assignment_t(throwing_move_assignment_t &&) = default;
  throwing_move_assignment_t &operator=(throwing_move_assignment_t &&) noexcept(false) { throw std::exception(); }
};

struct throwing_copy_operator_t {
  throwing_copy_operator_t() = default;
  throwing_copy_operator_t(throwing_copy_operator_t const &) = default;
  throwing_copy_operator_t &operator=(throwing_copy_operator_t const &) noexcept(false) { throw std::exception(); }
};

struct only_movable {
  static inline size_t move_assignment_called = 0;

  constexpr only_movable() = default;

  constexpr only_movable(only_movable &&other) noexcept {
    assert(other.coin && "Move of moved value?");
    coin = true;
    other.coin = false;
  }

  constexpr only_movable &operator=(only_movable &&other) noexcept {
    if (this != &other) {
      move_assignment_called += 1;
      assert(other.coin && "Move of moved value?");
      coin = true;
      other.coin = false;
    }
    return *this;
  }

  [[nodiscard]] constexpr bool has_coin() const noexcept { return coin; }

  only_movable(only_movable const &) = delete;
  only_movable &operator=(only_movable const &) = delete;

private:
  bool coin{true};
};

struct yac_coin {
  constexpr operator int() noexcept { return 42; }
};

struct coin_wrapper {
  constexpr coin_wrapper() noexcept = default;

  constexpr coin_wrapper(coin_wrapper &&other) noexcept {
    assert(other.coin && "Move of moved value?");
    coin = other.coin;
    other.coin = 0;
  }

  constexpr coin_wrapper &operator=(coin_wrapper &&other) noexcept {
    if (this != &other) {
      assert((other.coin != 0) && "Move of moved value?");
      coin = other.coin;
      other.coin = 0;
    }
    return *this;
  }

  [[nodiscard]] constexpr auto has_coins() const noexcept { return coin; }

  constexpr explicit coin_wrapper(yac_coin) noexcept : coin{17} {}

  constexpr coin_wrapper(coin_wrapper const &other) noexcept : coin(other.coin + 1) {}

  constexpr coin_wrapper &operator=(coin_wrapper const &other) noexcept {
    if (this != &other) {
      coin = other.coin + 1;
    }
    return *this;
  }

private:
  int coin{1};
};

struct sqr_sum_visitor {
  template <typename ... Args>
  constexpr long operator()(Args ... args) const noexcept {
    return ((args * args) + ...);
  }
};

struct strange_visitor {
    strange_visitor() = default;
    strange_visitor(strange_visitor const&) = delete;
    strange_visitor(strange_visitor&&) = default;

    constexpr int operator()(int value) & {
        return value;
    }
    constexpr int operator()(int value) && {
        return value + 1;
    }
    constexpr int operator()(int value) const & {
        return value + 2;
    }
    constexpr int operator()(int value) const && {
        return value + 3;
    }
};

