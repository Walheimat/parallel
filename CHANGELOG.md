# Changelog

## [0.3.1](https://github.com/Walheimat/parallel/compare/v0.3.0...v0.3.1) (2024-01-28)


### Features

* **ci:** add semantic-release ([cf04b50](https://github.com/Walheimat/parallel/commit/cf04b50d4a99664821bd1be03174138a72b66b68))

## [0.3.0]

### Added

- New macro `parallel-mirror` to create inverted functions. Currently
  this only works for boolean functions. It uses the original
  function's arity to create that new function.

## [v0.2.0]

### Added

- A README.
- Variable `parallel-naming-function` that defaults to
  `parallel--normalize` this is a new function that will not repeat a
  prefix common to both functions.
- Variable `parallel-custom-namespace` that can be set to a string
  used in your own function definitions (think `my/`). Allows
  normalization to work for library and custom functions.
- Key `:name` can now be passed to `parallel` to explicitly set a
  name.

### Changed

- Macro `parallel` is now a public version of internal macro
  `parallel--parallelize` to ensure the signature may remain the same
  while the underlying implementation changes.
- Custom variable for the separator was renamed from
  `parallel--separator` to `parallel-separator`.

## [v0.1.0]

Initial version as an extraction of my config package.
