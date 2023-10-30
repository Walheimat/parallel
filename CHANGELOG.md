# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
