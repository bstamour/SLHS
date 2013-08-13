#ifndef BST_NUMERICS_HXX_
#define BST_NUMERICS_HXX_

#include <numeric>
#include <cmath>
#include <functional>

namespace Numerics
{
   template <typename Vector1, typename Vector2>
   auto euclidean_distance(Vector1&& v1, Vector2&& v2)
   {
      auto dist = std::inner_product(begin(v1), end(v1), begin(v2),
                                     0.0D,
                                     [](std::size_t p, std::size_t q)
                                     {
                                        return (q - p) * (q - p);
                                     },
                                     std::plus<std::size_t>{});
      return std::sqrt(dist);
   }

} // namespace Numerics

#endif
