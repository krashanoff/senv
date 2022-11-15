//! FFI for denv

#[repr(C)]
pub struct Assignment;

#[cfg(no_mangle)]
pub fn parse() -> bool {}
