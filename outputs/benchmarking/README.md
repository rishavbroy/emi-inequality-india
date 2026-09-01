# Benchmarking outputs

This directory stores opt-in **benchmark-specific** outputs. Ordinary public builds preserve this directory; use `make clean-benchmarking` to clear it intentionally. Shared input universes, QA summaries, and tuning references belong to their canonical diagnostic targets rather than being copied into benchmarking. For example, the fuzzy benchmark persists threshold sensitivity only and reuses the fuzzy diagnostic candidate-pair coverage/reference artifacts when rendering its analysis note.
