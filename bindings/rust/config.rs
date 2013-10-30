#[link(name = "dtgconf", vers = "0.1")];
#[crate_type = "lib"];

pub mod config
{
	use std::libc;
	use std::ptr;
	use std::vec;
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

	struct configitem
	{
		key: ~str,
		val: Option<~str>
	}

	struct configsection
	{
		name: ~str,
		items: ~[configitem]
	}

	struct c_config
	{
		sectioncount: libc::c_uint,
		size: libc::c_uint,
		sections: **c_configsection
	}

	#[link_args = "-ldtgconf"]
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

	struct cfg {
		conf: c_config
	}

	impl cfg {
		pub fn new() -> cfg
		{
			cfg{conf: c_config {sectioncount:0, size:0, sections:ptr::null()}}
		}
		#[fixed_stack_segment]
		pub fn load(path: &str) -> Option<~cfg>
		{
			unsafe {
				let conf=cfg::new();
				let p=ptr::to_unsafe_ptr(&conf.conf);
				let ok=do path.with_c_str |s| {
					match config_load(p,s) {
						0 => true,
						_ => false
					}
				};
				match ok {
					true  => Some(~conf),
					false => None
				}
			}
		}
		#[fixed_stack_segment]
		pub fn find_section(&self, section: &str) -> Option<configsection>
		{
			unsafe {
				let p=ptr::to_unsafe_ptr(&self.conf);
				let sec=do section.with_c_str |s| {
					config_find_section(p, s)
				};
				if(sec.is_null()) {
					None
				}
				else
				{
					let ivec=vec::from_buf((*sec).items, (*sec).itemcount as uint);
					let items=vec::flat_map(ivec, |i:&*c_configitem| {
						let ck = c_str::CString::new((**i).key, false);
						let cv = c_str::CString::new((**i).val, false);
						match cv.as_str() {
							Some(v) => {
								~[configitem {
									key: ck.as_str().unwrap().to_owned(),
									val: Some(v.to_owned())
								}]
							},
							None => {
								~[configitem {
									key: ck.as_str().unwrap().to_owned(),
									val: None
								}]
							}
						}
					});
					let cname = c_str::CString::new((*sec).name, false);
					return Some(configsection {name: cname.as_str().unwrap().to_owned(), items: items});
				}
			}
		}
		#[fixed_stack_segment]
		pub fn find_item(&self, needle: &str, haystack: Option<&str>) -> Option<configitem>
		{
			unsafe {
				let p=ptr::to_unsafe_ptr(&self.conf);
				return do needle.with_c_str() |n| {
					let ci=match haystack {
						Some(hs) => {
							do hs.with_c_str |h| {
								config_find_item(p,n,h)
							}
						}
						None => config_find_item(p,n,ptr::null())
					};
					if(ci.is_null()) {
						None
					}
					else
					{
						let ck=c_str::CString::new((*ci).key, false);
						let cv=c_str::CString::new((*ci).val, false);
						do ck.as_str().and_then |k| {
							let both=do cv.as_str().and_then |v| {
								Some(configitem {key: k.to_owned(), val: Some(v.to_owned())})
							};
							match both {
								Some(ci) => Some(ci),
								None => Some(configitem {key: k.to_owned(), val: None})
							}
						}
					}
				};
			}
		}
		#[fixed_stack_segment]
		pub fn add(&self, section: &str, key: &str, val: Option<~str>)
		{
			unsafe {
				let p=ptr::to_unsafe_ptr(&self.conf);
				do section.with_c_str |s| {
					do key.with_c_str |k| {
						match val {
							Some(ref val) => {
								do val.with_c_str |v| {
									config_add(p, s, k, v)
								}
							},
							None => {
								config_add(p, s, k, ptr::null())
							}
						}
					}
				}
			}
		}
		#[fixed_stack_segment]
		pub fn save(&self, to_file: &str)
		{
			unsafe {
				let p=ptr::to_unsafe_ptr(&self.conf);
				do to_file.with_c_str |f| {
					config_save(p,f)
				}
			}
		}
		#[fixed_stack_segment]
		fn free(&self)
		{
			unsafe {
				let p=ptr::to_unsafe_ptr(&self.conf);
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
