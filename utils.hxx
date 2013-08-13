#ifndef BST_UTILS_HXX_
#define BST_UTILS_HXX_

#include <string>

// TODO: Remove this and use suffix s once GCC supports that from C++14.
inline auto operator "" _s (char const* str, unsigned long)
{
   return std::string{str};
}

#endif
