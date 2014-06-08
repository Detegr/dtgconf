#![crate_id = "dtgconf#0.2"]
#![crate_type = "lib"]
#[allow(non_camel_case_types)]
#[allow(dead_code)]

pub mod config
{
	extern crate libc;
	use std::ptr;
	use std::c_str;

	struct c_configitem
	{
		key: *libc::c_char,
		val: *libc::c_char
	}

	struct c_configsection
	{
		name: *libc::c_char,
		itemcount: libc::c_uint,
		size: libc::c_uint,
		items: **c_configitem
	}

	pub struct configitem
	{
		key: String,
		val: Option<String>
	}

	impl configitem
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

	pub struct configsection
	{
		name: String,
		items: Vec<configitem>
	}
	impl configsection
	{
		pub fn get_name<'a>(&'a self) -> &'a str
		{
			self.name.as_slice()
		}
		pub fn get_items<'a>(&'a self) -> &'a [configitem]
		{
			self.items.as_slice()
		}
	}

	struct c_config
	{
		sectioncount: libc::c_uint,
		size: libc::c_uint,
		sections: **c_configsection
	}

	#[link(name = "dtgconf")]
	extern
	{
		fn config_init(conf: *c_config);
		fn config_free(conf: *c_config);
		fn config_load(conf: *c_config, filename: *libc::c_char) -> libc::c_int;
		fn config_save(conf: *c_config, filename: *libc::c_char);
		fn config_find_section(conf: *c_config, section: *libc::c_char) -> *c_configsection;
		fn config_find_item(conf: *c_config, needle: *libc::c_char, haystack: *libc::c_char) -> *c_configitem;
		fn config_add(conf: *c_config, section: *libc::c_char, key: *libc::c_char, val: *libc::c_char);
	}

	pub struct cfg {
		conf: c_config
	}

	impl cfg {
		pub fn new() -> cfg
		{
			cfg{conf: c_config {sectioncount:0, size:0, sections:ptr::null()}}
		}
		pub fn load(path: &str) -> Option<cfg>
		{
			let conf=cfg::new();
			unsafe {
				let p : *c_config = &conf.conf;
				let ok=path.with_c_str(|s| {
					match config_load(p,s) {
						0 => true,
						_ => false
					}
				});
				match ok {
					true  => Some(conf),
					false => None
				}
			}
		}
		pub fn find_section(&self, section: &str) -> Option<configsection>
		{
			unsafe {
				let p : *c_config = &self.conf;
				let sec=section.with_c_str(|s| {
					config_find_section(p, s)
				});
				if sec.is_null() {None}
				else
				{
					let mut items=vec![];
					for i in range(0, (*sec).itemcount)
					{
						let item : **c_configitem = (*sec).items.offset(i as int);
						let ck = c_str::CString::new((**item).key, false);
						let cv = c_str::CString::new((**item).val, false);
						items.push(configitem {
							key: ck.as_str().unwrap().to_string(),
							val: match cv.is_null() {
								true => None,
								false => match cv.as_str() {
									Some(v) => Some(v.to_string()),
									None => None
								}
							}
						});
					}
					let cname = c_str::CString::new((*sec).name, false);
					return Some(configsection {name: cname.as_str().unwrap().to_string(), items: items});
				}
			}
		}
		pub fn find_item(&self, needle: &str, haystack: Option<&str>) -> Option<configitem>
		{
			unsafe {
				let p : *c_config = &self.conf;
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
								Some(configitem {key: k.to_string(), val: Some(v.to_string())})
							});
							match both {
								Some(ci) => Some(ci),
								None => Some(configitem {key: k.to_string(), val: None})
							}
						});
					}
				});
			}
		}
		pub fn add(&self, section: &str, key: &str, val: Option<String>)
		{
			unsafe {
				let p : *c_config = &self.conf;
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
		pub fn save(&self, to_file: &str)
		{
			unsafe {
				let p : *c_config = &self.conf;
				to_file.with_c_str(|f| {
					config_save(p,f)
				});
			}
		}
		fn free(&self)
		{
			unsafe {
				let p : *c_config = &self.conf;
				config_free(p);
			}
		}
	}
	impl Drop for cfg
	{
		fn drop(&mut self)
		{
			self.free();
		}
	}
}
