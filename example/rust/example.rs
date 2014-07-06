extern crate dtgconf;
use dtgconf::Config;
use std::rand;
use std::rand::Rng;

fn main()
{
	let conf_path=Path::new("../example.conf");
	let mut conf=match Config::load(&conf_path) {
		Ok(c) => c,
		Err(_) => {
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
	conf.save(&conf_path);
}
