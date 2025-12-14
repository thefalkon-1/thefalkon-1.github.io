# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Pelican-based static website deployed to GitHub Pages. Pelican is a Python static site generator that converts content (Markdown/RST files) into HTML.

## Build Commands

```bash
# Generate the site (development)
make html

# Live development server with auto-regeneration
make devserver

# Serve the site locally (http://localhost:8000)
make serve [PORT=8000]

# Generate using production settings
make publish

# Deploy to GitHub Pages
make github
```

Alternative using invoke tasks (tasks.py):
```bash
invoke build          # Build local version
invoke livereload     # Auto-reload browser on changes
invoke gh_pages       # Publish to GitHub Pages
```

## Project Structure

- `content/` - Markdown/RST content files (articles, pages)
- `output/` - Generated HTML output (gitignored)
- `themes/svbhack/` - Current theme (responsive, svbtle-like)
- `pelicanconf.py` - Development configuration
- `publishconf.py` - Production configuration (extends pelicanconf.py)

## Configuration

Key settings in `pelicanconf.py`:
- `THEME` - Points to `themes/svbhack`
- `PATH` - Content directory
- `SITEURL` - Base URL (empty for dev, set for production)

Theme supports: Google Analytics (`GOOGLE_ANALYTICS`), Matomo, custom logo (`USER_LOGO_URL`), tagline (`TAGLINE`), Disqus comments.

## Development

Requires Python with Pelican installed (uses `.venv/` virtual environment). Activate with:
```bash
source .venv/bin/activate
```
