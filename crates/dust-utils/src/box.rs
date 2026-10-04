use bumpalo::Bump;

pub type Box<'a, T> = std::boxed::Box<T, &'a Bump>;
pub type Vec<'a, T> = std::vec::Vec<T, &'a Bump>;

pub mod boxed_slice_serialize_with {

    use serde::Serialize as _;

    use super::Box;

    pub fn serialize<T, S>(value: &Box<'_, [T]>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
        T: serde::Serialize,
    {
        let value: &[T] = value.as_ref();
        value.serialize(serializer)
    }
}

pub mod vec_serialize_with {
    use serde::Serialize as _;

    use super::Vec;

    pub fn serialize<T, S>(value: &Vec<'_, T>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
        T: serde::Serialize,
    {
        let value: &[T] = value.as_ref();
        value.serialize(serializer)
    }
}
