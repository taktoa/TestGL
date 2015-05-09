# TestGL

``` sh
# Update to latest version of Cabal.
cabal update
cabal install cabal-install

# Initialize a sandbox and install the package's dependencies.
make install

# For Nix users:
make nix-shell

# Configure & build the package.
make configure
make build

# Test package.
make test

# Run executable.
make run

# Start REPL.
make repl

# Generate documentation.
make haddock

# Analyze coverage.
make hpc
```
