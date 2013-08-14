#include "config.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

static int binarysearch(const void* arr, const void* key, size_t elemsize, unsigned int max, int(*cmp)(const void*, const void*));
static int item_compare(const void* a, const void* b);
static int section_compare(const void* a, const void* b);
static int section_sort(const void* a, const void* b);

static void item_add(struct configsection* section, const char* key, const char* val);
static int item_find(struct configsection* haystack, const char* needle);
static void item_free(struct configitem* item);

static void section_new(struct config* conf, const char* name);
static void section_free(struct configsection* sect);

static int item_compare(const void* a, const void* b)
{
	return strncmp((const char*)a,(*(struct configitem**)b)->key,ITEM_MAXLEN);
}
static int item_sort(const void* a, const void* b)
{
	return strncmp((*(struct configitem**)a)->key,(*(struct configitem**)b)->key,ITEM_MAXLEN);
}

static int section_compare(const void* a, const void* b)
{
	return strncmp((const char*)a,(*(struct configsection**)b)->name,ITEM_MAXLEN);
}

static int section_sort(const void* a, const void* b)
{
	return strncmp((*(struct configsection**)a)->name,(*(struct configsection**)b)->name,ITEM_MAXLEN);
}

static void item_free(struct configitem* item)
{
	free(item->key);
	if(item->val) free(item->val);
}

static void section_free(struct configsection* sect)
{
	free(sect->name);
	for(unsigned int i=0; i<sect->itemcount; ++i)
	{
		item_free(sect->items[i]);
		free(sect->items[i]);
	}
	free(sect->items);
}

void config_free(struct config* conf)
{
	for(unsigned int i=0; i<conf->sectioncount; ++i)
	{
		section_free(conf->sections[i]);
		free(conf->sections[i]);
	}
	free(conf->sections);
}

static int item_find(struct configsection* haystack, const char* needle)
{
	if(haystack->itemcount)
	{
		return binarysearch(haystack->items, needle, sizeof(struct configitem*), haystack->itemcount-1, item_compare);
	}
	return -1;
}

void config_init(struct config* conf)
{
	conf->sectioncount = 0;
	conf->size = 0;
	conf->sections=NULL;
}

static void item_add(struct configsection* section, const char* key, const char* val)
{
	/* Allocate more space if needed */
	if(section->itemcount >= section->size)
	{
		section->size = section->size?section->size*2:8;
		struct configitem** newitems = (struct configitem**)realloc(section->items, section->size * sizeof(struct configitem*));
		assert(newitems != NULL);
		section->items=newitems;
	}
	/* Assign new item */
	struct configitem* newitem = (struct configitem*)malloc(sizeof(struct configitem));
	newitem->key = strndup(key, ITEM_MAXLEN);
	if(val) newitem->val = strndup(val, ITEM_MAXLEN);
	else newitem->val = NULL;
	section->items[section->itemcount++] = newitem;

	/* TODO: Do not sort the list every time. Just add new item to correct place instead */
	qsort(section->items, section->itemcount, sizeof(struct configitem*), item_sort);
}

void config_add(struct config* conf, const char* section, const char* key, const char* val)
{
	struct configsection* sect;
	if((sect=config_find_section(conf, section)))
	{
		struct configitem* item;
		if((item=config_find_item(conf, key, section)))
		{
			if(item->val) free(item->val);
			if(val) item->val=strndup(val, ITEM_MAXLEN);
			else item->val=NULL;
		}
		else item_add(sect, key, val);
	}
	else
	{
		section_new(conf, section);
		config_add(conf, section, key, val);
	}
}

static void section_new(struct config* conf, const char* name)
{
	if(conf->sectioncount >= conf->size)
	{
		conf->size = conf->size?conf->size*2:8;
		struct configsection** newsection = (struct configsection**)realloc(conf->sections, conf->size * sizeof(struct configsection*));
		assert(newsection != NULL);
		conf->sections=newsection;
	}
	/* Assign new item */
	struct configsection* newsection = (struct configsection*)malloc(sizeof(struct configsection));
	newsection->name = strndup(name, ITEM_MAXLEN);
	newsection->itemcount = 0;
	newsection->size = 0;
	newsection->items = NULL;
	conf->sections[conf->sectioncount++] = newsection;

	qsort(conf->sections, conf->sectioncount, sizeof(struct configsection*), section_sort);
}

static int binarysearch(const void* arr, const void* key, size_t elemsize, unsigned int max, int(*cmp)(const void*, const void*))
{
	unsigned int min=0;
	while(min<max)
	{
		unsigned int mid = (min+max)>>1;
		assert(mid<max);
		if(cmp(key,(unsigned char*)arr+(mid*elemsize)) > 0) min=mid+1;
		else max=mid;
	}
	if((max==min) && (cmp(key,(unsigned char*)arr+(min*elemsize)) == 0)) return min;
	else return -1;
}

struct configsection* config_find_section(struct config* haystack, const char* needle)
{
	if(haystack->sectioncount)
	{
		int i = binarysearch(haystack->sections, needle, sizeof(struct configsection*), haystack->sectioncount-1, section_compare);
		if(i>=0) return haystack->sections[i];
	}
	return NULL;
}

struct configitem* config_find_item(struct config* haystack, const char* needle, const char* section)
{
	if(section)
	{
		struct configsection* sect;
		if((sect=config_find_section(haystack, section)))
		{
			int item=item_find(sect, needle);
			if(item != -1) return sect->items[item];
		}
	}
	else
	{
		for(unsigned int i=0; i<haystack->sectioncount; ++i)
		{
			int item=item_find(haystack->sections[i], needle);
			if(item != -1) return haystack->sections[i]->items[item];
		}
	}
	return NULL;
}

void config_flush(struct config* conf, FILE* stream)
{
	for(unsigned int i=0; i<conf->sectioncount; ++i)
	{
		fprintf(stream, "[%s]\n", conf->sections[i]->name);
		for(unsigned int j=0; j<conf->sections[i]->itemcount; ++j)
		{
			struct configitem* item = conf->sections[i]->items[j];
			if(item->val)
			{
				fprintf(stream, "\t%s=%s\n", item->key, item->val);
			}
			else fprintf(stream, "\t%s\n", item->key);
		}
	}
}

int config_load(struct config* conf, const char* filename)
{
	config_init(conf);
	FILE* f=fopen(filename, "r");
	if(!f) return -1;
	int r=1;
	char* line=NULL;
	size_t s=0;
	char header[ITEM_MAXLEN+3];
	char left[ITEM_MAXLEN+1];
	char right[ITEM_MAXLEN+1];
	while((r = getline(&line, &s, f)) != -1)
	{
		int h=sscanf(line, "[%255[^\n]", header);
		header[ITEM_MAXLEN]=0;
		if(!h)
		{
			h=sscanf(line, "%*[ \t]%255[^=]=%255[^\n]", left, right);
			left[ITEM_MAXLEN]=0;
			right[ITEM_MAXLEN]=0;
			if(h==1)
			{
				h=sscanf(line, "%*[ \t]%255[^\n]", left);
				if(h==1) config_add(conf, header, left, NULL);
				else return -1;
			}
			else if(h==2) config_add(conf, header, left, right);
			else return -1;
		}
		else
		{
			size_t hlen = strnlen(header, ITEM_MAXLEN)-1;
			if(header[hlen]==']') header[hlen]=0;
			else return -1;
		}
	}
	free(line);
	fclose(f);
	return 0;
}

int config_save(struct config* conf, const char* filename)
{
	FILE* f=fopen(filename, "w");
	if(!f) return -1;
	config_flush(conf, f);
	fclose(f);
	return 0;
}
