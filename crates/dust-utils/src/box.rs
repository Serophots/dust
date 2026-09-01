use std::{
    fs::File,
    io::{self, Read as _},
    path::Path,
};

use bumpalo::Bump;

pub type Box<'a, T> = std::boxed::Box<T, &'a Bump>;

pub mod box_serialize_with {
    use serde::Serialize as _;

    use super::Box;

    pub fn serialize<T, S>(value: &Box<'_, [T]>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
        T: serde::Serialize,
    {
        value.as_ref().serialize(serializer)
    }
}
