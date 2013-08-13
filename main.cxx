#include "innovation.hxx"
#include "nlp.hxx"

#include <iostream>
#include <exception>

auto innovation_distance(Innovation::Innovation const& inno1,
                         Innovation::Innovation const& inno2)
{
   return NLP::euclidean_distance(inno1.description, inno2.description);
}

int main()
try
{
   using namespace Innovation;
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
