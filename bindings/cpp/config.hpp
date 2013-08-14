#pragma once

#include "../../src/config.h"

#include <string>
#include <stdexcept>

struct DtgConfigItem
{
	private:
		std::string m_Key;
		std::string m_Value;
	public:
		DtgConfigItem(struct configitem* i);
		const std::string& Key() const { return m_Key; }
		const std::string& Val() const { return m_Value; }
};

class DtgConf
{
	private:
		bool m_Initialized;
		struct config m_Config;
		std::string m_Path;
	public:
		DtgConf();
		DtgConf(const std::string& path);
		~DtgConf();

		void Add(const std::string& section, const std::string& key);
		void Add(const std::string& section, const std::string& key, const std::string& val);
		void Load(const std::string& path);
		void Save();
		void SaveAs(const std::string& path);
};

struct DtgConfException : public std::runtime_error
{
	DtgConfException(const std::string& msg) : std::runtime_error(msg) {};
};
