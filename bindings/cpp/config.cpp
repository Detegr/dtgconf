#include "config.hpp"

namespace DtgConf
{
	static inline ConfigItem CConfigItemToConfigItem(struct configitem* ci)
	{
		const char* vp=ci->val;
		return std::make_pair(
				std::string(ci->key),
				vp ? std::string(vp) : "");
	}

	ConfigSection::ConfigSection(struct configsection* cs) : m_ConfigSection(cs)
	{
		if(m_ConfigSection->name)
		{
			m_Name=std::string(m_ConfigSection->name);
		}
	}

	std::vector<ConfigItem> ConfigSection::Items() const
	{
		std::vector<ConfigItem> ret;
		for(unsigned int i=0; i<m_ConfigSection->itemcount; ++i)
		{
			ret.push_back(CConfigItemToConfigItem(m_ConfigSection->items[i]));
		}
		return ret;
	}

	Config::Config() : m_Initialized(false)
	{
		config_init(&m_Config);
		m_Initialized=true;
	}

	Config::Config(const std::string& path) : m_Initialized(false)
	{
		Load(path);
	}

	Config::~Config()
	{
		if(m_Initialized)
		{
			config_free(&m_Config);
			m_Initialized=false;
		}
	}

	void Config::Add(const std::string& section, const std::string& key)
	{
		if(!m_Initialized) throw DtgConfException("Config file not initialized before adding an item");
		config_add(&m_Config, section.c_str(), key.c_str(), NULL);
	}

	void Config::Add(const std::string& section, const std::string& key, const std::string& val)
	{
		if(!m_Initialized) throw DtgConfException("Config file not initialized before adding an item");
		config_add(&m_Config, section.c_str(), key.c_str(), val.c_str());
	}

	void Config::Load(const std::string& path)
	{
		if(m_Initialized) config_free(&m_Config);
		if(config_load(&m_Config, path.c_str()))
		{
			throw DtgConfException("Failed to load config. Either file not found or config file malformed");
		}
		m_Initialized=true;
		m_Path=path;
	}

	void Config::Save()
	{
		if(!m_Initialized) throw DtgConfException("Config file not initialized before saving");
		config_save(&m_Config, m_Path.c_str());
	}

	void Config::SaveAs(const std::string& path)
	{
		if(!m_Initialized) throw DtgConfException("Config file not initialized before saving");
		config_save(&m_Config, path.c_str());
	}
	
	std::vector<ConfigSection> Config::GetConfig() const
	{
		std::vector<ConfigSection> ret;
		for(unsigned int i=0; i<m_Config.sectioncount; ++i)
		{
			ret.push_back(ConfigSection(m_Config.sections[i]));
		}
		return ret;
	}

	ConfigItem Config::GetItem(const std::string& needle)
	{
		struct configitem* ci;
		ci=config_find_item(&m_Config, needle.c_str(), NULL);
		if(!ci) throw DtgConfException("Item " + needle + " not found.");
		return CConfigItemToConfigItem(ci);
	}
	ConfigItem Config::GetItem(const std::string& needle, const std::string section)
	{
		struct configitem* ci=config_find_item(&m_Config, needle.c_str(), section.c_str());
		if(!ci) throw DtgConfException("Item " + section + " not found.");
		return CConfigItemToConfigItem(ci);
	}
	ConfigSection Config::GetSection(const std::string& needle)
	{
		struct configsection* cs=config_find_section(&m_Config, needle.c_str());
		if(!cs) throw DtgConfException("Section " + needle + " not found.");
		return ConfigSection(cs);
	}
}
