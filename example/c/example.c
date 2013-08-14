#include "../../src/config.h"
#include <assert.h>
#include <stdlib.h>
#include <time.h>

#define CONF_PATH "../example.conf"

void error(const char* msg)
{
	fprintf(stderr, msg);
	exit(1);
}

int main()
{
	struct config conf;
	if(config_load(&conf, CONF_PATH))
	{
		error("Failed to load example.conf\n");
	}

	// Go through the whole config file
	for(unsigned i=0; i<conf.sectioncount; ++i)
	{
		printf("[%s]\n", conf.sections[i]->name);
		for(unsigned j=0; j<conf.sections[i]->itemcount; ++j)
		{
			char* key=conf.sections[i]->items[j]->key;
			char* val=conf.sections[i]->items[j]->val;
			printf("\t%s=%s\n", key, val?val:"NO VALUE");
		}
	}

	struct configsection* sect=config_find_section(&conf, "Second section");
	if(!sect) error("Second section not found\n");
	else
	{
		printf("Keys from %s:\n", sect->name);
		for(unsigned i=0; i<sect->itemcount; ++i)
		{
			printf("\t%s\n", sect->items[i]->key);
		}
	}

	// If a section is known where the key exists, it can be specified to make the search more efficient
	struct configitem* item=config_find_item(&conf, "Key2", NULL);
	printf("Item:\n\t%s=%s\n", item->key, item->val);

	// Add/update random value to the config
	srand(time(NULL));
	int rnd=rand();
	char rndstr[12]; // Ugly, but will do for this example
	snprintf(rndstr, 12, "%d", rnd);
	config_add(&conf, "First section", "Random value from example", rndstr);

	config_save(&conf, CONF_PATH);
}
