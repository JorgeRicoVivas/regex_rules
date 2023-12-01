use std::collections::VecDeque;
use std::ops::Range;
use std::str::FromStr;

use regex::{Captures, Regex};

macro_rules! impl_to_regex_rules_for {
    (
        signed $castable:ty; $($tail:tt)*
    )=>{
        impl ToRegexRules <$castable> for $castable {
            fn regex_rules() -> RegexRules<Self>{
                RegexRules {
                    regex: Regex::new(r"(?x) (?P<float> (?P<integer> (?P<sign>[-+]?) (?P<natural>[0-9]+)) (\.(?P<fraction>[0-9]+))?)").unwrap(),
                    transform: |captures,_|
                        {
                            let result = Self::from_str(captures.name("integer").unwrap().as_str());
                            if result.is_ok(){Some(result.unwrap())}else{None}
                        }
                }
            }
        }
        impl_to_regex_rules_for!($($tail)*);
    };
    (
        unsigned $castable:ty; $($tail:tt)*
    )=>{
        impl ToRegexRules <$castable> for $castable {
            fn regex_rules() -> RegexRules<Self>{
                RegexRules {
                    regex: Regex::new(r"(?x) (?P<float> (?P<integer> (?P<sign>[-+]?) (?P<natural>[0-9]+)) (\.(?P<fraction>[0-9]+))?)").unwrap(),
                    transform: |captures,_|
                        {
                            let result = Self::from_str(captures.name("natural").unwrap().as_str());
                            if result.is_ok(){Some(result.unwrap())}else{None}
                        }
                }
            }
        }
        impl_to_regex_rules_for!($($tail)*);
    };
    (
        float $castable:ty; $($tail:tt)*
    )=>{
        impl ToRegexRules <$castable> for $castable {
            fn regex_rules() -> RegexRules<Self>{
                RegexRules {
                    regex: Regex::new(r"(?x) (?P<float> (?P<integer> (?P<sign>[-+]?) (?P<natural>[0-9]+)) (\.(?P<fraction>[0-9]+))?)").unwrap(),
                    transform: |captures,_|
                        {
                            let result = Self::from_str(captures.name("float").unwrap().as_str());
                            if result.is_ok(){Some(result.unwrap())}else{None}
                        }
                }
            }
        }
        impl_to_regex_rules_for!($($tail)*);
    };
    (
        $($tail:tt)*
    ) => {

    }
}

impl_to_regex_rules_for! {
    unsigned u8; unsigned u16; unsigned u32; unsigned u64; unsigned u128; unsigned usize;
    signed i8; signed i16; signed i32; signed i64; signed i128; signed isize;
    float f32; float f64;
}

pub struct RegexData<'linelife> {
	pub original_full_line: &'linelife str
}

pub struct RegexRules<T> {
	pub regex: Regex,
	pub transform: fn(&Captures, RegexData) -> Option<T>,
}

impl<T> RegexRules<T> {
	pub fn regex(&self) -> &Regex {
		&self.regex
	}

	pub fn transform(&self) -> fn(&Captures<'_>, RegexData<'_>) -> Option<T> {
		self.transform
	}

	pub fn find(&self, line: &str) -> Option<T> {
		let rmatch = self.regex().captures(line);
		if rmatch.is_some() {
			(self.transform)(&rmatch.unwrap(), RegexData { original_full_line: line })
		} else {
			None
		}
	}

	pub fn find_all(&self, line: &str) -> Vec<T> {
		if self.regex.find(line).is_none() {
			return vec![];
		}
		self.regex.captures_iter(line)
			.map(|rmatch| {
				(self.transform)(&rmatch, RegexData { original_full_line: line })
			})
			.filter(|opt| opt.is_some())
			.map(|opt| opt.unwrap())
			.collect()
	}

	pub fn find_consume_all(&self, line: &mut String) -> Vec<T> {
		if self.regex.find(line).is_none() {
			return vec![];
		}
		let res_and_matches = self.regex.captures_iter(line)
			.map(|rmatch| {
				let value = (self.transform)(&rmatch, RegexData { original_full_line: line });
				(value, rmatch)
			})
			.filter(|(opt, _)| opt.is_some())
			.map(|(opt, rmatch)| (opt.unwrap(), rmatch))
			.collect::<Vec<(_, _)>>();
		let match_positions = res_and_matches
			.iter()
			.map(|(_, rmatch)| {
				rmatch.get(0).unwrap().range()
			})
			.collect::<Vec<_>>();
		let res = res_and_matches.into_iter().map(|(value, _)| value).collect();
		let positions = reduce_ranges(match_positions);
		*line = line.chars().enumerate()
			.filter(|(index, _)| !positions.iter().any(|range| range.contains(index)))
			.map(|(_, character)| character)
			.collect();
		res
	}
}

fn reduce_ranges<T:PartialOrd>(ranges: Vec<Range<T>>) -> Vec<Range<T>> {
	let mut ranges = ranges.into_iter().collect::<VecDeque<_>>();
	let mut res = VecDeque::new();
	while !ranges.is_empty() {
		let mut current = ranges.pop_front().unwrap();
		let mut rejected = VecDeque::new();
		while !ranges.is_empty() {
			let comparing_value = ranges.pop_front().unwrap();
			if contains_part_of(&current, &comparing_value) {
				let (current_start ,current_end) = (current.start, current.end);
				let (comparing_start ,comparing_end) = (comparing_value.start, comparing_value.end);
				let min_start = if current_start < comparing_start { current_start } else { comparing_start };
				let max_start = if current_end > comparing_end { current_end } else { comparing_end };
				current = min_start..max_start;
			} else {
				rejected.push_back(comparing_value);
			}
		}
		std::mem::swap(&mut ranges, &mut rejected);
		res.push_back(current);
	}
	res.into_iter().collect()
}

fn contains_part_of<T:PartialOrd>(original: &Range<T>, contained: &Range<T>) -> bool {
	//One element from this range will be contained by the other range if the highest value of
	//this is greater or equal to the lowest value of the other value, and the lowest value of
	//this range is lesser or equal to the others range lowest value
	//self.max >= other.min && self.min <= other.max
	original.end >= contained.start && original.start <= contained.end
}


#[test]
fn test(){
	let message_rules = u8::regex_rules();
	let mut texto = "Mi8 texto de pr1ue2ba".to_string();
	println!("Res: {:?}", message_rules.find_consume_all(&mut texto));
	println!("{}",texto);
}

pub trait ToRegexRules<T> {
	fn regex_rules() -> RegexRules<T> { RegexRules { regex: Regex::new("").unwrap(), transform: |_, _| None } }
	fn to_regex_rules(&self) -> RegexRules<T> { Self::regex_rules() }
}

impl ToRegexRules<bool> for bool {
	fn regex_rules() -> RegexRules<Self> {
		RegexRules {
			regex: Regex::new(r"(?x)(?i)(?P<val>TRUE|FALSE|1|0)").unwrap(),
			transform: |captures, _| {
				Some(["true", "1"].contains(&&*captures.name("val").unwrap().as_str().to_lowercase()))
			},
		}
	}
}

pub struct ListOfValuesRules {
	pub names: Vec<String>,
	pub separator: Option<String>,
}

pub struct ListOfValuesRes<T: ToRegexRules<T>> {
	list_name: String,
	values: Vec<T>,
}

impl<T: ToRegexRules<T>> ListOfValuesRes<T> {
	pub fn list_name(&self) -> &str {
		&self.list_name
	}
	pub fn values(&self) -> &Vec<T> {
		&self.values
	}
}

impl<T> ToRegexRules<ListOfValuesRes<T>> for ListOfValuesRules where T: ToRegexRules<T> {
	fn to_regex_rules(&self) -> RegexRules<ListOfValuesRes<T>> {
		RegexRules {
			regex: {
				let list_names = self.names.join("|");
				let values_regex = T::regex_rules().regex.to_string();
				let separator = if self.separator.is_some() { format!(r"[[:space:]]+ {} [[:space:]]+", self.separator.clone().unwrap_or("".to_string())) } else { "[[:space:]]+".to_string() };
				Regex::new(&*format!(r"(?x)(?i) (?P<list_name>{}) {} (?P<values> ({} [[:space:]]*,? [[:space:]]* )+)", list_names, separator, values_regex)).unwrap()
			},
			transform: |captures, _| {
				Some(ListOfValuesRes {
					list_name: captures.name("list_name").unwrap().as_str().to_string(),
					values: T::regex_rules().find_all(captures.name("values").unwrap().as_str()),
				})
			},
		}
	}
}

//noinspection ALL
//noinspection RsExternalLinter
fn example_of_list_of_values_regex() {
	let found: Vec<ListOfValuesRes<u8>> = ListOfValuesRules {
		names: vec!["Bayonetta[[:space:]]1", "Bayonetta[[:space:]]2"].into_iter().map(|st| st.to_string()).collect(),
		separator: Some("for".to_string()),
	}
		.to_regex_rules().find_all("Bayonetta 1 for 1,2 , 3, 4, Bayonetta 2 for 5,6,7, Bayonetta 3 for 8");
	found.into_iter().for_each(|list| println!("Name: {}, values: {:?}", list.list_name, list.values));
}
