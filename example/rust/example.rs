extern mod dtgconf;
use dtgconf::config;
use std::rand;
use std::rand::Rng;
use std::int;
use std::str;

fn main()
{
	let conf_path="../example.conf";
	let conf=match config::cfg::load(conf_path) {
		Some(c) => c,
		None => {
			println("Failed to load example.conf");
			return
		}
	};
	match conf.find_section("Second section") {
		Some(sect) => {
			println!("Keys from {:s}:", sect.name);
			for i in sect.items.iter() {
				println!("\t{:s}", i.key);
			}
		}
		None => {
			println("Second section not found");
		}
	};

	// If a section is known where the key exists, it can be specified to make the search more efficient
	let item=conf.find_item("Key2", None).unwrap();
	println!("Item\n\t{:s}={:s}", item.key, item.val.unwrap());

	let mut rng=rand::rng();
	let randomstr=int::to_str_bytes(rng.gen(), 10, str::from_utf8);
	conf.add("First section", "Random value from example", Some(randomstr));
	conf.save(conf_path);
}
