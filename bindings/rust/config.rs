#![crate_name = "dtgconf"]
#![crate_type = "lib"]
#[allow(non_camel_case_types)]
#[allow(dead_code)]

extern crate libc;
use std::ptr;
use std::c_str;
use std::result::Result;

struct CConfigItem
{
	key: *const libc::c_char,
	val: *const libc::c_char
}

#[allow(dead_code)]
struct CConfigSection
{
	name: *const libc::c_char,
	itemcount: libc::c_uint,
	size: libc::c_uint,
	items: *const *const CConfigItem
}

pub struct ConfigItem
{
	key: String,
	val: Option<String>
}

impl ConfigItem
{
	pub fn get_key<'a>(&'a self) -> &'a str
	{
		self.key.as_slice()
	}
	pub fn get_val<'a>(&'a self) -> Option<&'a str>
	{
		self.val.as_ref().and_then(|v| {Some(v.as_slice())})
	}
}

pub struct ConfigSection
{
	name: String,
	items: Vec<ConfigItem>
}
impl ConfigSection
{
	pub fn get_name<'a>(&'a self) -> &'a str
	{
		self.name.as_slice()
	}
	pub fn get_items<'a>(&'a self) -> &'a [ConfigItem]
	{
		self.items.as_slice()
	}
}

#[allow(dead_code)]
struct CConfig
{
	sectioncount: libc::c_uint,
	size: libc::c_uint,
	sections: *mut *mut CConfigSection
}

#[link(name = "dtgconf")]
extern
{
	fn config_init(conf: *mut CConfig);
	fn config_free(conf: *mut CConfig);
	fn config_load(conf: *const CConfig, filename: *const libc::c_char) -> libc::c_int;
	fn config_save(conf: *const CConfig, filename: *const libc::c_char);
	fn config_find_section(conf: *const CConfig, section: *const libc::c_char) -> *const CConfigSection;
	fn config_find_item(conf: *const CConfig, needle: *const libc::c_char, haystack: *const libc::c_char) -> *const CConfigItem;
	fn config_add(conf: *mut CConfig, section: *const libc::c_char, key: *const libc::c_char, val: *const libc::c_char);
}

pub struct Config {
	conf: CConfig
}

impl Config {
	pub fn new() -> Config
	{
		let mut conf = Config {
			conf: CConfig {
				sectioncount:0,
				size:0,
				sections:ptr::mut_null()
			}
		};
		unsafe {
			config_init(&mut conf.conf);
		}
		conf
	}
	pub fn load(path: &Path) -> Result<Config, &'static str>
	{
		unsafe {
			path.as_str()
				.map(|pstr| { pstr.with_c_str(|s|
				{
					let conf=Config::new();
					let p : *const CConfig = &conf.conf;
					match config_load(p,s) {
						0 => Ok(conf),
						_ => Err("Could not load config file. No such file or invalid config file.")
					}
				})}).unwrap()
		}
	}
	pub fn find_section(&self, section: &str) -> Option<ConfigSection>
	{
		unsafe {
			let p : *const CConfig = &self.conf;
			let sec=section.with_c_str(|s| {
				config_find_section(p, s)
			});
			if sec.is_null() {None}
			else
			{
				let mut items=vec![];
				for i in range(0, (*sec).itemcount)
				{
					let item : *const *const CConfigItem = (*sec).items.offset(i as int);
					let ck = c_str::CString::new((**item).key, false);
					items.push(ConfigItem {
						key: ck.as_str().unwrap().to_string(),
						val: match (**item).val.is_null() {
							true => None,
							false => match c_str::CString::new((**item).val, false).as_str() {
								Some(v) => Some(v.to_string()),
								None => None
							}
						}
					});
				}
				let cname = c_str::CString::new((*sec).name, false);
				return Some(ConfigSection {name: cname.as_str().unwrap().to_string(), items: items});
			}
		}
	}
	pub fn find_item(&self, needle: &str, haystack: Option<&str>) -> Option<ConfigItem>
	{
		unsafe {
			let p : *const CConfig = &self.conf;
			return needle.with_c_str(|n| {
				let ci=match haystack {
					Some(hs) => hs.with_c_str(|h| {
							config_find_item(p,n,h)
					}),
					None => config_find_item(p,n,ptr::null())
				};
				if ci.is_null() {None}
				else
				{
					let ck=c_str::CString::new((*ci).key, false);
					let cv=c_str::CString::new((*ci).val, false);
					return ck.as_str().and_then(|k| {
						let both=cv.as_str().and_then(|v| {
							Some(ConfigItem {key: k.to_string(), val: Some(v.to_string())})
						});
						match both {
							Some(ci) => Some(ci),
							None => Some(ConfigItem {key: k.to_string(), val: None})
						}
					});
				}
			});
		}
	}
	pub fn add(&mut self, section: &str, key: &str, val: Option<String>)
	{
		unsafe {
			let p : *mut CConfig = &mut self.conf;
			section.with_c_str(|s| {
				key.with_c_str(|k| {
					match val {
						Some(ref val) => {
							val.with_c_str(|v| {
								config_add(p, s, k, v)
							});
						},
						None => {
							config_add(p, s, k, ptr::null())
						}
					}
				});
			});
		}
	}
	pub fn save(&self, to_file: &Path)
	{
		unsafe {
			let p : *const CConfig = &self.conf;
			to_file.with_c_str(|f| {
				config_save(p,f)
			});
		}
	}
	fn free(&mut self)
	{
		unsafe {
			let p : *mut CConfig = &mut self.conf;
			config_free(p);
		}
	}
}
impl Drop for Config
{
	fn drop(&mut self)
	{
		self.free();
	}
}
