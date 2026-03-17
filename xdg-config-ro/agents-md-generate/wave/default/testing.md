# Testing

Always set `WAVE_CACHE_ON=0` and `PYTHONPATH=<wave-repo-root>` when running tests.

Run Wave lit tests with `lit -v lit_tests` (or a specific test file path).
Run end-to-end tests with `pytest --run-e2e`.
The full suite is slow; target specific files and use `-k` to select tests during edit/test cycles.

WaveASM has separate tests: `wnb --test-waveasm-lit` and `wnb --test-waveasm-e2e -- --run-e2e`.

Lit tests work on any hardware. End-to-end tests may require specific GPUs.
