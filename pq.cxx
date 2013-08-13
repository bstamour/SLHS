#include <pqxx/pqxx>

#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <sstream>
#include <unordered_map>
#include <cmath>

// TODO: Remove this and use suffix s once GCC supports that from C++14.
inline auto operator "" _s (char const* str, unsigned long)
{
   return std::string{str};
}

//------------------------------------------------------------------------------
// The innovation data structure and related functions.
//------------------------------------------------------------------------------

namespace Innovations
{
   struct Innovation
   {
      std::string name;
      std::string description;
      std::string continuum_impact;
      std::string system_impact;
      std::string keywords;
   };

   std::ostream& operator << (std::ostream& os, Innovation const& inno)
   {
      os << "name:       " << inno.name             << '\n'
         << "desc:       " << inno.description      << '\n'
         << "con impact: " << inno.continuum_impact << '\n'
         << "sys impact: " << inno.system_impact    << '\n'
         << "keywords:   " << inno.keywords
         ;
      return os;
   }

   auto fetch_innovations()
   {
      auto connection_string =
         "user='web' password='KloodWewph' host='172.18.0.36' "
         "port='5437' dbname='innovation_map'"_s;

      pqxx::connection conn{connection_string};
      pqxx::work txn{conn};

      auto res = txn.exec("SELECT * FROM innovations");

      auto innos = std::vector<Innovation>{};
      innos.reserve(res.size());
      for (auto const& row: res)
      {
         innos.emplace_back(Innovation{
            row["name"].c_str(),
            row["description"].c_str(),
            row["impact_on_continuum_of_care"].c_str(),
            row["impact_at_system_level"].c_str(),
            row["keywords"].c_str()
         });
      }

      return innos;
   }

} // namespace Innovations

//------------------------------------------------------------------------------
// For number-crunching code.
//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------
// NLP-related code.
//------------------------------------------------------------------------------

namespace NLP
{
   // Next steps:
   //
   // Right now a simple euclidean metric is used to compute distances between
   // innovations. We need to extend this to real concept mining. Ideally the
   // program flow will look something like this:
   //
   // 1. Grab the innovations from the database.
   // 2. Mine concepts over this corpus. Keep in mind the geometric nature of
   //    conceptual spaces.
   // 3. For each pair of innovations, find their distance based on the
   //    conceptual space geometry computed in step 2. It may be a fuzzy distance
   //    (see Subjective Logic) or it may be crisp. Not sure yet.

   template <typename String>
   auto words(String const& str)
   {
      std::stringstream ss{str};

      auto wds = std::vector<String>{};
      auto temp = String{};

      while (ss >> temp)
         wds.emplace_back(temp);

      wds.shrink_to_fit();
      return wds;
   }

   template <typename Dictionary, typename Wordlist>
   auto make_frequency_vector(Dictionary const& dict, Wordlist const& words)
   {
      auto freqs = std::vector<std::size_t>{};
      freqs.reserve(dict.size());

      for (auto const& w : dict)
         freqs.emplace_back(std::count(begin(words), end(words), w));

      return freqs;
   }

   template <typename WordList>
   auto make_dictionary(WordList words1, WordList words2)
   {
      std::sort(begin(words1), end(words1));
      std::sort(begin(words2), end(words2));

      auto dictionary = decltype(words1){};
      std::set_union(begin(words1), end(words1), begin(words2), end(words2),
                     std::back_inserter(dictionary));
      dictionary.shrink_to_fit();
      return dictionary;
   }

   // Given two strings, compute the euclidean distance between the word frequency
   // vectors.
   template <typename String>
   auto euclidean_distance(String const& string1, String const& string2)
   {
      auto words1 = words(string1);
      auto words2 = words(string2);
      auto dictionary = make_dictionary(words1, words2);

      auto vector1 = make_frequency_vector(dictionary, words1);
      auto vector2 = make_frequency_vector(dictionary, words2);

      return Numerics::euclidean_distance(vector1, vector2);
   }

} // namespace NLP

auto innovation_distance(Innovations::Innovation const& inno1,
                         Innovations::Innovation const& inno2)
{
   return NLP::euclidean_distance(inno1.description, inno2.description);
}

int main()
try
{
   using namespace Innovations;
   auto innovations = fetch_innovations();

   for (auto const& inno1 : innovations)
      for (auto const& inno2 : innovations)
         std::cout << innovation_distance(inno1, inno2) << std::endl;
}
catch (std::exception const& e)
{
   std::cout << "ERROR: " << e.what() << std::endl;
   return 1;
}
