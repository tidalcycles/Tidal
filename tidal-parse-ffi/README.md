# tidal-parse-ffi

`tidal-parse-ffi` is a Haskell library providing a Foreign Function Interface (FFI) for parsing Tidal patterns and exporting them to JSON format. It is designed to be used in conjunction with Rust and other languages via FFI.

## Features

- Exposes Haskell Tidal parsing functions through FFI.
- Converts parsed patterns into JSON format.
- Supports Rust integration via `libc`.

## Dependencies

- `tidal-parse`
- `base`
- `aeson`
- `bytestring`
- `containers`
- `tidal`

## Installation

Clone the repository and navigate to the `tidal-parse-ffi` directory:

```sh
cd tidal-parse-ffi
cabal build
```

## Usage

Include `tidal-parse-ffi` as a dependency in your Cabal project:

```cabal
build-depends: tidal-parse-ffi
```

### Haskell Example

```haskell
import Foreign.C.String (newCString)
main = do
  result <- eval_pattern_c "[bd sn]"
  putStrLn =<< peekCString result
```

## Rust Integration

To use this library in Rust:

1. Add the dependency to `Cargo.toml`:
   ```toml
   [dependencies]
   tidal-parse-ffi = { path = "../tidal-parse-ffi" }
   ```
2. Link the library in `build.rs`:
   ```rust
   println!("cargo:rustc-link-lib=static=tidalparseffi");
   ```

### Rust Example

```rust
use std::ffi::{CString, CStr};
use std::os::raw::c_char;

extern "C" {
    fn eval_pattern_c(input: *const c_char) -> *mut c_char;
}

fn main() {
    let input = CString::new("[bd sn]").expect("CString::new failed");
    unsafe {
        let result_ptr = eval_pattern_c(input.as_ptr());
        let result = CStr::from_ptr(result_ptr).to_string_lossy().into_owned();
        println!("Parsed Pattern: {}", result);
    }
}
```

The library provides a single exported function:

```haskell
foreign export ccall eval_pattern_c :: CString -> IO CString
```
