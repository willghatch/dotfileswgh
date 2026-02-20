# No secrets in commits

Never commit API keys, passwords, tokens, certificates, or other secrets—even temporarily.
Use environment variables or a gitignored file (e.g., `.env`) for secrets needed during development.
If you encounter hardcoded secrets in existing code, flag them but do not copy them elsewhere.
