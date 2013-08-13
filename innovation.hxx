#ifndef BST_INNOVATION_HXX_
#define BST_INNOVATION_HXX_

#include "utils.hxx"
#include "nlp.hxx"

#include <pqxx/pqxx>

#include <string>
#include <iosfwd>
#include <vector>

namespace Innovation
{
   struct Innovation
   {
      std::string name;
      std::string description;
      std::string continuum_impact;
      std::string system_impact;
      std::string keywords;
   };

   inline std::ostream& operator << (std::ostream& os, Innovation const& inno)
   {
      os << inno.name;
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

   inline auto euclidean_distance(Innovation const& inno1, Innovation const& inno2)
   {
      return NLP::euclidean_distance(inno1.description, inno2.description);
   }

} // namespace Innovations

#endif
