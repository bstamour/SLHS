#ifndef BST_NLP_HXX_
#define BST_NLP_HXX_

#include "numerics.hxx"

#include <sstream>
#include <vector>
#include <algorithm>
#include <iterator>

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

#endif
