/*
 *  concepts.cppm – written by Colin Ford
 *    see github.com/colinrford/polynomial_nttp for AGPL-3.0 License, and
 *                                              for more info
 *  experimental.concepts is a c++ module
 *    as the name suggests, the contents found here are strictly experimental,
 *    and while the project will compile and run just fine, these concepts are
 *    still very much in development
 *
 *  these concepts are part of another library I am working on, and are
 *    subject to reappear within that library under a different license,
 *    whenever said library is released
 */

module;

import std;

export module lam.concepts;

/*
 *  all concepts will be declared with _c to distinguish them from types, etc
 *    at this time I am also appending _weak in many situations as concepts
 *    are limited in what they can check. With time it would be nice to
 *    work toward removing the _weak appendage.
 */
namespace lam::concepts::experimental::internals
{
  template<typename T>
  concept has_additive_identity_c_weak =
  requires {
    { T(0) } -> std::same_as<T>;
  } || requires {
    { T::zero() } -> std::same_as<T>;
  };
  template<typename T>
  concept has_multiplicative_identity_c_weak =
  requires {
    { T(1) } -> std::same_as<T>;
  } || requires {
    { T::one() } -> std::same_as<T>;
  };
  /*
   *  operator overloaded weak syntactical 'requirements' modeling group
   *  elements (does not check values)
   */
  template<typename G>
  concept additive_group_element_c_weak = has_additive_identity_c_weak<G> and
  requires(G g, G h, G k)
  {
    { -g } -> std::same_as<decltype(g)>;
    { g + h } -> std::same_as<decltype(h + g)>;
    { g - h } -> std::same_as<decltype(h - g)>;
    { (g + h) + k } -> std::same_as<decltype(g + (h + k))>;
  };

  /*
   *  has_multiplicative_inverse_c_weak: checks for existence of multiplicative inverse
   *  For a true multiplicative group, you only need:
   *    - Identity element (1 or one())
   *    - Inverse element (g⁻¹)
   *    - Closure under multiplication
   *  Division is derived: g / h = g * inv(h)
   */
  template<typename G>
  concept has_multiplicative_inverse_c_weak =
    requires(G g) {
      { inv(g) } -> std::same_as<G>;
    } or requires(G g) {
      { g.inverse() } -> std::same_as<G>;
    } or requires(G g) {
      { G(1) / g } -> std::same_as<G>;
    } or requires(G g) {
      { G::one() / g } -> std::same_as<G>;
    };

  template<typename G>
  concept multiplicative_group_element_c_weak =
    has_multiplicative_identity_c_weak<G> and
    has_multiplicative_inverse_c_weak<G> and
    requires(G g, G h, G k)
    {
      { g * h } -> std::same_as<decltype(h * g)>;
      { (g * h) * k } -> std::same_as<decltype(g * (h * k))>;
    };

  template<typename G>
  concept group_element_c_weak = additive_group_element_c_weak<G>
                              or multiplicative_group_element_c_weak<G>;

  /*
   *  operator overloaded weak syntactical 'requirements' modeling group
   *  elements (does not check values)
   */
  template<typename R>
  concept ring_element_c_weak = additive_group_element_c_weak<R>
                             and has_multiplicative_identity_c_weak<R>
                             and requires(R r, R s, R t)
  {
    { r * s } -> std::same_as<decltype(s * r)>;
    { (r * s) * t } -> std::same_as<decltype(r * (s * t))>;
    { r * (s + t) } -> std::same_as<decltype(r * s + r * t)>;
    { (r + s) * t } -> std::same_as<decltype(r * t + s * t)>;
  };

  /*
   *  has_division_c_weak: checks for existence of division operation
   *  For fields, division (except by zero) is always defined.
   *  Division is derived from inverse: g / h = g * inv(h)
   *  But for practical use, we may want explicit division syntax.
   */
  template<typename K>
  concept has_division_c_weak =
    requires(K a, K b) {
      { a / b } -> std::same_as<K>;
    } or requires(K a, K b) {
      { div(a, b) } -> std::same_as<K>;
    } or requires(K a, K b) {
      { a.divide(b) } -> std::same_as<K>;
    };

  /*
   *  operator overloaded weak syntactical 'requirements' modeling field
   *  elements (does not check values)
   */
  template<typename K>
  concept field_element_c_weak = ring_element_c_weak<K>
                              and multiplicative_group_element_c_weak<K>
                              and has_division_c_weak<K>;
} // end namespace lam::concepts::experimental::internals
namespace lam::concepts::experimental
{
  export template<typename G>
  concept additive_group_element_c_weak = internals::additive_group_element_c_weak<G>;
  export template<typename G>
  concept multiplicative_group_element_c_weak = internals::multiplicative_group_element_c_weak<G>;
  export template<typename G>
  concept group_element_c_weak = internals::group_element_c_weak<G>;
  export template<typename R>
  concept ring_element_c_weak = internals::ring_element_c_weak<R>;
  export template<typename K>
  concept field_element_c_weak = internals::field_element_c_weak<K>;
} // end namespace lam::concepts::experimental


// ==========================================
// Linear Algebra Concepts (Moved from linearalgebra)
// ==========================================

namespace lam::concepts::experimental::internals
{
  /*
   *  left_module_element_c_weak
   *  Models a left module M over a scalar ring R.
   *  Scalar multiplication: r * m -> M
   */
  template<typename M, typename R>
  concept left_module_element_c_weak = lam::concepts::experimental::additive_group_element_c_weak<M> and
                                       lam::concepts::experimental::ring_element_c_weak<R> and requires(M m, R r) {
                                         { r * m } -> std::same_as<M>;
                                       };

  /*
   *  right_module_element_c_weak
   *  Models a right module M over a scalar ring R.
   *  Scalar multiplication: m * r -> M
   */
  template<typename M, typename R>
  concept right_module_element_c_weak = lam::concepts::experimental::additive_group_element_c_weak<M> and
                                        lam::concepts::experimental::ring_element_c_weak<R> and requires(M m, R r) {
                                          { m * r } -> std::same_as<M>;
                                        };

  /*
   *  module_element_c_weak
   *  Models a module M that supports both left and right scalar multiplication.
   *  (Often effectively a bimodule where the left and right actions are compatible,
   *   or a module over a commutative ring where left and right are the same).
   */
  template<typename M, typename R>
  concept module_element_c_weak = left_module_element_c_weak<M, R> and right_module_element_c_weak<M, R>;

  /*
   *  vectorspace_element_c_weak
   *  Models a vector space V over a scalar field K.
   *  Requirements:
   *  1. It is a module over K (supporting both left/right mult for this library's conventions).
   *  2. K is a field.
   */
  template<typename V, typename K>
  concept vectorspace_element_c_weak =
    module_element_c_weak<V, K> and lam::concepts::experimental::field_element_c_weak<K>;

  /*
   *  vector_c_weak
   *  Models a concrete vector container (like lam::linalg::vector).
   */
  template<typename V>
  concept vector_c_weak =
    requires {
      typename V::scalar_type;
      typename V::size_type;
    } and module_element_c_weak<V, typename V::scalar_type> and
    requires(V v, V::size_type i) {
      { v.size() } -> std::same_as<typename V::size_type>;
      { v[i] } -> std::convertible_to<typename V::scalar_type>;
    } and
    std::same_as<std::remove_cvref_t<decltype(std::declval<V>()[std::declval<typename V::size_type>()])>,
                 typename V::scalar_type>;

  /**
   * Models a linear transformation T: V -> W.
   * A linear transformation is a map between two vector spaces that preserves
   * vector addition and scalar multiplication.
   */
  template<typename F, typename V, typename W>
  concept linear_transformation_c_weak =
    vectorspace_element_c_weak<V, typename V::scalar_type> &&
    vectorspace_element_c_weak<W, typename W::scalar_type> && requires(F f, V v) {
      { f(v) } -> std::convertible_to<W>;
    };

  /**
   * Models a matrix (finitary linear transformation).
   * Supports element access, row/column dimensions, and row/column iterators.
   */
  template<typename M, typename Scalar>
  concept matrix_c_weak =
    vectorspace_element_c_weak<M, Scalar>
    && requires(M m, typename M::size_type i) {
         typename M::size_type;
         { m.rows() } -> std::integral;
         { m.cols() } -> std::integral;
         { m[i, i] } -> std::convertible_to<Scalar>;
         { m.row(i) };
         { m.col(i) };
         // { m.rows_range() }; // Optional?
         // { m.cols_range() }; // Optional?
       };

  /**
   * Models a representation of a group G on a vector space V.
   * A representation is a homomorphism rho: G -> GL(V).
   * For each g in G, rho(g) is an invertible linear transformation (automorphism) of V.
   */
  template<typename Rho, typename G, typename V>
  concept representation_c_weak = requires(Rho rho, G g, V v) {
    { rho(g) } -> linear_transformation_c_weak<V, V>;
    { rho(g)(v) } -> std::same_as<V>;
  };

  /**
   * Models a square matrix (rows == cols).
   * Structural check: type must have is_square() method.
   */
  template<typename M, typename Scalar>
  concept square_matrix_c_weak = matrix_c_weak<M, Scalar> && requires(M m) {
    { m.is_square() } -> std::same_as<bool>;
  };

  /**
   * Models an invertible matrix type.
   * Structural check: type must have det() and inverse() methods.
   * Runtime singularity check still required.
   */
  template<typename M, typename Scalar>
  concept invertible_c_weak = square_matrix_c_weak<M, Scalar> && requires(M m) {
    { m.det() } -> std::convertible_to<Scalar>;
    { m.inverse() } -> matrix_c_weak<Scalar>;
  };

} // namespace lam::concepts::experimental::internals

namespace lam::concepts::experimental
{
  export template<typename M, typename R>
  concept left_module_element_c_weak = internals::left_module_element_c_weak<M, R>;

  export template<typename M, typename R>
  concept right_module_element_c_weak = internals::right_module_element_c_weak<M, R>;

  export template<typename M, typename R>
  concept module_element_c_weak = internals::module_element_c_weak<M, R>;

  export template<typename V, typename K>
  concept vectorspace_element_c_weak = internals::vectorspace_element_c_weak<V, K>;

  export template<typename V>
  concept vector_c_weak = internals::vector_c_weak<V>;

  export template<typename F, typename V, typename W>
  concept linear_transformation_c_weak = internals::linear_transformation_c_weak<F, V, W>;

  export template<typename M, typename Scalar>
  concept matrix_c_weak = internals::matrix_c_weak<M, Scalar>;

  export template<typename M, typename Scalar>
  concept square_matrix_c_weak = internals::square_matrix_c_weak<M, Scalar>;

  export template<typename M, typename Scalar>
  concept invertible_c_weak = internals::invertible_c_weak<M, Scalar>;

  export template<typename Rho, typename G, typename V>
  concept representation_c_weak = internals::representation_c_weak<Rho, G, V>;
} // namespace lam::concepts::experimental
