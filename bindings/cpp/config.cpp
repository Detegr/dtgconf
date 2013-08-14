#include "config.hpp"

DtgConf::DtgConf() : m_Initialized(false)
{
	config_init(&m_Config);
	m_Initialized=true;
}

DtgConf::DtgConf(const std::string& path) : m_Initialized(false)
{
	Load(path);
}

DtgConf::~DtgConf()
{
	if(m_Initialized)
	{
		config_free(&m_Config);
		m_Initialized=false;
	}
}

void DtgConf::Add(const std::string& section, const std::string& key)
{
	if(!m_Initialized) throw DtgConfException("Config file not initialized before adding an item");
	config_add(&m_Config, section.c_str(), key.c_str(), NULL);
}

void DtgConf::Add(const std::string& section, const std::string& key, const std::string& val)
{
	if(!m_Initialized) throw DtgConfException("Config file not initialized before adding an item");
	config_add(&m_Config, section.c_str(), key.c_str(), val.c_str());
}

void DtgConf::Load(const std::string& path)
{
	if(m_Initialized) config_free(&m_Config);
	if(config_load(&m_Config, path.c_str()))
	{
		throw DtgConfException("Failed to load config. Either file not found or config file malformed");
	}
	m_Initialized=true;
	m_Path=path;
}

void DtgConf::Save()
{
	if(!m_Initialized) throw DtgConfException("Config file not initialized before saving");
	config_save(&m_Config, m_Path.c_str());
}

void DtgConf::SaveAs(const std::string& path)
{
	if(!m_Initialized) throw DtgConfException("Config file not initialized before saving");
	config_save(&m_Config, m_Path.c_str());
}
