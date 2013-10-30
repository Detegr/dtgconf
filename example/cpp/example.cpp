#include "../../bindings/cpp/config.hpp"
#include <iostream>
#include <time.h>
#include <sstream>

using namespace DtgConf;

static void PrintItem(const ConfigItem& item)
{
	std::cout << "\t" << item.first << "=";
	if(item.second.length()) std::cout << item.second << std::endl;
	else std::cout << "NO VALUE" << std::endl;
}

int main()
{
	std::string confpath="../../example/example.conf";
	Config c(confpath);

	// Go through the whole config file
	std::vector<ConfigSection> sections=c.GetConfig();
	for(auto i=sections.begin(); i!=sections.end(); ++i)
	{
		std::cout << "[" << i->Name() << "]" << std::endl;
		auto items=i->Items();
		for(auto j=items.begin(); j!=items.end(); ++j)
		{
			PrintItem(*j);
		}
	}

	try {
		ConfigSection cs=c.GetSection("Second section");
		std::cout << "Keys from " << cs.Name() << ":" << std::endl;;
		auto items=cs.Items();
		for(auto i=items.begin(); i!=items.end(); ++i)
		{
			std::cout << "\t" << i->first << std::endl;
		}
	}
	catch(const DtgConfException& e) { std::cout << "Second section not found." << std::endl; }

	// If a section is known where the key exists, it can be specified to make the search more efficient
	ConfigItem item=c.GetItem("Key2");
	PrintItem(item);

	// Add/update random value to the config
	srand(time(NULL));
	int rnd=rand();
	std::stringstream ss;
	ss << rnd;
	std::string rndstr;
	ss >> rndstr;
	c.Add("First section", "Random value from example", rndstr);
	c.Save();
}
