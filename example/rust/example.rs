extern crate dtgconf;
use dtgconf::config;
use std::rand;
use std::rand::Rng;

fn main()
{
	let conf_path="../example.conf";
	let conf=match config::cfg::load(conf_path) {
		Some(c) => c,
		None => {
			println!("Failed to load example.conf");
			return
		}
	};
	match conf.find_section("Second section") {
		Some(sect) => {
			println!("Keys from {:s}:", sect.get_name());
			for i in sect.get_items().iter() {
				println!("\t{:s}", i.get_key());
			}
		}
		None => {
			println!("Second section not found");
		}
	};

	// If a section is known where the key exists, it can be specified to make the search more efficient
	let item=conf.find_item("Key2", None).unwrap();
	println!("Item\n\t{:s}={:s}", item.get_key(), item.get_val().unwrap());

	let mut rng=rand::task_rng();
	let randomstr = rng.gen::<int>().to_str();
	conf.add("First section", "Random value from example", Some(randomstr));
	conf.save(conf_path);
}
