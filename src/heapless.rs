use std::{fmt::Debug, ptr::null_mut};

use crate::interpreter::RuntimeError;

/// Fixed size, stack-based, FFI friendly string with heap fall-back
#[repr(C)]
pub struct Str<const N: usize> {
    len: usize,
    data: [u8; N],
    // TODO
    // fallback: *mut u8
}

impl<const N: usize> Str<N> {
    pub fn new() -> Self {
        Self {
            data: [0; N],
            len: 0,
            // fallback: null_mut()
        }
    }

    pub fn from_str(s: &str) -> Result<Self, RuntimeError> {
        let mut ret = Self::new();
        ret.set(s)?;
        Ok(ret)
    }

    pub fn set(&mut self, to: &str) -> Result<(), RuntimeError> {
        let bytes = to.as_bytes();
        let len = bytes.len();

        if len > N {
            return Err(RuntimeError {
                message: "String too long".to_string(),
                callstack: vec![],
                range: None,
            });
        }

        self.data[..len].copy_from_slice(&bytes[..len]);
        self.len = len;

        Ok(())
    }

    pub fn push(&mut self, byte: u8) {
        if self.len < N {
            self.data[self.len] = byte;
            self.len += 1;
        }
    }

    pub fn as_str(&self) -> &str {
        std::str::from_utf8(&self.data[..self.len]).unwrap()
    }
}

impl<const N: usize> Debug for Str<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
