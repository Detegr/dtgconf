#pragma once

#include "../../src/config.h"

#include <string>
#include <stdexcept>
#include <vector>

namespace DtgConf
{
	typedef std::pair<std::string, std::string> ConfigItem;
	class ConfigSection
	{
		private:
			ConfigSection() {}
			std::string m_Name;
			struct configsection* m_ConfigSection;
		public:
			ConfigSection(struct configsection* cs);
			const std::string& Name() const {return m_Name;}
			std::vector<ConfigItem> Items() const;
			struct configsection* GetRawSection() const {return m_ConfigSection;}
	};

	class Config
	{
		private:
			bool m_Initialized;
			struct config m_Config;
			std::string m_Path;
		public:
			Config();
			Config(const std::string& path);
			~Config();

			void Add(const std::string& section, const std::string& key);
			void Add(const std::string& section, const std::string& key, const std::string& val);
			void Load(const std::string& path);
			void Save();
			void SaveAs(const std::string& path);

			std::vector<ConfigSection> GetConfig() const;
			ConfigItem GetItem(const std::string& needle);
			ConfigItem GetItem(const std::string& needle, const std::string section);
			ConfigSection GetSection(const std::string& needle);
	};

	struct DtgConfException : public std::runtime_error
	{
		DtgConfException(const std::string& msg) : std::runtime_error(msg) {};
	};
}
