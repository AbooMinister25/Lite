//! This crate contains the logic for handling name resolution. This is the
//! process in which variables are resolved by finding what declaration it might
//! refer to in the context of the current scope.

#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::unnecessary_wraps)]
#![allow(clippy::missing_const_for_fn)]
#![allow(clippy::return_self_not_must_use)]
#![allow(clippy::module_name_repetitions)]

pub struct NameResolver {}
