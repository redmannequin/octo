use std::collections::hash_map::Entry;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Object {
    Null,
    Number(f64),
    Bool(bool),
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn get<S: AsRef<str>>(&self, name: S) -> Option<Object> {
        self.store.get(name.as_ref()).copied()
    }

    pub fn set<S: Into<String>>(&mut self, name: S, obj: Object) {
        self.store.insert(name.into(), obj);
    }

    pub fn set_existing<S: Into<String>>(&mut self, name: S, obj: Object) {
        match self.store.entry(name.into()) {
            Entry::Occupied(mut entry) => entry.insert(obj),
            _ => unimplemented!(),
        };
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Struct<'a> {
    name: &'a str,
}
