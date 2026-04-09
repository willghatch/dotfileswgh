# Testing

Run Wave lit tests with `WAVE_DIR=$PWD $WAVE_DIR/build_tools/wave-dev-setup.sh --test-wave-lit`.
Additional arguments for `lit` can be passed after `--`, e.g. `-- path/to/specific/test.py`.

Run Wave end-to-end tests with `WAVE_DIR=$PWD $WAVE_DIR/build_tools/wave-dev-setup.sh --test-wave-e2e`.
Additional arguments for `pytest` can be passed after `--`, e.g. `-- -k test_gemm` to select specific tests, `-- -x` to stop on first failure, or `-- path/to/test_file.py` to target a specific file.

The full suite is slow; use `--` passthrough args to target specific files and use `-k` to select tests during edit/test cycles.

WaveASM has separate tests: `--test-waveasm-lit` and `--test-waveasm-e2e` (same passthrough syntax).
Water has lit tests: `--test-water-lit`.

The test commands automatically rebuild native components (water, waveasm) before running.

Lit tests work on any hardware. End-to-end tests may require specific GPUs.
