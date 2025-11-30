# Revela Decompiler Release Guide

This document explains how to build and release the Revela decompiler using the CI/CD pipeline.

## Automated Builds

The repository includes a GitHub Actions workflow that automatically builds release versions of the decompiler for multiple platforms.

### Supported Platforms

The CI builds for the following platforms:
- **Linux** (x86_64)
- **Windows** (x86_64)
- **macOS Intel** (x86_64)
- **macOS Apple Silicon** (ARM64)

### Triggering a Release Build

There are two ways to trigger a release build:

#### Method 1: Create a Git Tag (Recommended)

```bash
# Create and push a tag
git tag decompiler-v1.0.0
git push origin decompiler-v1.0.0
```

The tag must follow the pattern `decompiler-v*` (e.g., `decompiler-v1.0.0`, `decompiler-v1.0.1`).

#### Method 2: Manual Workflow Dispatch

1. Go to the **Actions** tab in your GitHub repository
2. Select **Build Revela Decompiler Release** workflow
3. Click **Run workflow**
4. Optionally enter a tag name (e.g., `decompiler-v1.0.0`)
5. Click **Run workflow**

### Release Artifacts

After the workflow completes, the following artifacts will be generated:

- `move-decompiler-linux-x86_64.tar.gz` - Linux binary
- `move-decompiler-windows-x86_64.zip` - Windows binary
- `move-decompiler-macos-x86_64.tar.gz` - macOS Intel binary
- `move-decompiler-macos-arm64.tar.gz` - macOS Apple Silicon binary

These artifacts will be:
1. Available as workflow artifacts (for 90 days by default)
2. Attached to the GitHub release (if triggered by a tag or manual dispatch with a tag name)

## Local Development Builds

For local development and testing, use the standard Cargo commands:

```bash
# Debug build
cargo build --bin move-decompiler

# Release build
cargo build --release --bin move-decompiler

# Run directly
cargo run --bin move-decompiler -- -b <bytecode_file>
```

## Cross-Platform Build Notes

### Building for macOS ARM64 on x86_64

If you're on an Intel Mac and want to build for Apple Silicon:

```bash
rustup target add aarch64-apple-darwin
cargo build --release --bin move-decompiler --target aarch64-apple-darwin
```

### Building for Windows on Linux

Install cross-compilation tools:

```bash
# Install mingw-w64
sudo apt-get install mingw-w64

# Add Windows target
rustup target add x86_64-pc-windows-gnu

# Build
cargo build --release --bin move-decompiler --target x86_64-pc-windows-gnu
```

## Version Management

Version information is stored in `Cargo.toml`:

```toml
[package]
name = "move-decompiler"
version = "1.0.1-alpha"
```

Before creating a release, update the version in `Cargo.toml` and create a corresponding git tag.

## Troubleshooting

### Build Failures

If builds fail in CI:
1. Check the Actions tab for detailed logs
2. Verify that all dependencies are properly specified in `Cargo.toml`
3. Ensure the code builds locally with `cargo build --release --bin move-decompiler`

### Missing Artifacts

If artifacts are not attached to the release:
- Verify the tag name follows the pattern `decompiler-v*`
- Check that the workflow has the necessary GitHub permissions
- Ensure `GITHUB_TOKEN` has write permissions for releases

### Platform-Specific Issues

- **macOS**: Ensure Xcode command-line tools are installed
- **Windows**: PostgreSQL dependencies might require additional setup
- **Linux**: Should work out of the box with standard Rust toolchain

## Credits

Designed and implemented by Verichains.
