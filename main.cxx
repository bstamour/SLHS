#include "innovation.hxx"

#include <boost/program_options.hpp>

#include <iostream>
#include <exception>
#include <functional>
#include <cstdlib>

enum class distance_algorithm { Default, Euclidean };

struct Options_parser
{
   Options_parser() = default;
   Options_parser(Options_parser const&) = default;
   Options_parser(Options_parser&&) = default;

   Options_parser& operator = (Options_parser const&) = default;
   Options_parser& operator = (Options_parser&&) = default;

   ~Options_parser() = default;

   explicit Options_parser(int argc, char *argv[])
   {
      using namespace boost::program_options;
      options_description desc{"Allowed options"};

      //
      // TODO: Add additional program options here.
      //

      desc.add_options()
         ("help", "show help message")
         ("distance-algorithm", value<int>(), "set distance algorithm")
         ;

      variables_map vm;
      store(parse_command_line(argc, argv, desc), vm);
      notify(vm);

      if (vm.count("help"))
      {
         std::cout << desc << std::endl;
         std::exit(1);
      }

      if (vm.count("distance-algorithm"))
         distance = static_cast<distance_algorithm>(vm["distance-algorithm"].as<int>());
   }

   distance_algorithm distance = distance_algorithm::Default;
};

auto innovation_distance(Options_parser const& opts,
                         Innovation::Innovation const& inno1,
                         Innovation::Innovation const& inno2)
{
   switch (opts.distance)
   {
      case distance_algorithm::Euclidean:
         return euclidean_distance(inno1, inno2);
         break;
      default:
         return euclidean_distance(inno1, inno2);
   }
}

int main(int argc, char *argv[])
try
{
   auto opts = Options_parser{argc, argv};
   auto innovations = Innovation::fetch_innovations();

   using namespace std::placeholders;
   auto distance = std::bind(innovation_distance, opts, _1, _2);

   for (auto const& inno1 : innovations)
      for (auto const& inno2 : innovations)
         std::cout << distance(inno1, inno2) << std::endl;
}
catch (std::exception const& e)
{
   std::cout << "ERROR: " << e.what() << std::endl;
   return 1;
}
