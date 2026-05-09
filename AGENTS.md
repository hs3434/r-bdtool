# bdtool R Package

## Package Identity
- **Package name**: `bdtool` (defined in `DESCRIPTION`, not `mytool`)
- **RStudio project file**: `mytool.Rproj` (do not confuse with package name)
- **Build type**: Package (no vignettes)

## Build & Release
- Build: `devtools::build()` produces `bdtool_*.tar.gz`
- Install deps: `devtools::install_deps(dependencies = TRUE)`
- Roxygenize (regenerate NAMESPACE/DESCRIPTION): `devtools::document()`
- CI image: `rocker/devtools` with R 4.3.0

## Documentation
- Uses roxygen2 (`RoxygenNote: 7.3.2` in DESCRIPTION)
- Run `devtools::document()` to regenerate NAMESPACE from roxygen comments
- Do not edit NAMESPACE manually (it's generated)

## Testing
- Framework: testthat edition 3
- Run tests: `devtools::test()`
- Test files: `tests/testthat/*.R`

## Dependencies
- Imports: `filelock` (used in `file_lock()` function)

## CI/CD
- `rhub.yaml`: Cross-platform checks via R-hub (dispatched manually)
- `build.yaml`: Builds and attaches package tarball to GitHub releases on tag `v*`
