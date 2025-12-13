Title: Building Static Sites with Pelican
Date: 2024-12-08
Category: Technology
Tags: python, web, pelican
Summary: Why I chose Pelican for this blog and how the setup process went.

When I decided to start this blog, I evaluated several static site generators. I landed on Pelican, and here's why.

## Why Static?

Static sites are fast, secure, and cheap to host. There's no database to manage, no server to patch, and sites can be hosted for free on GitHub Pages or similar services.

## Why Pelican?

A few reasons:

1. **Python-based** - I'm comfortable with Python, so customizing themes and plugins feels natural
2. **Markdown support** - Writing in Markdown is a joy
3. **Jinja2 templates** - The templating system is flexible and powerful
4. **Active community** - Good documentation and plenty of themes/plugins

## The Setup

Getting started was straightforward:

```bash
# Create virtual environment
python -m venv .venv
source .venv/bin/activate

# Install Pelican with Markdown support
pip install pelican markdown

# Create a new site
pelican-quickstart

# Write content and build
make html
make serve
```

## Customization

I spent most of my time customizing the theme to get the minimalist look I wanted. Pelican's theming system is flexible—you can override individual templates or create a theme from scratch.

The result is what you see now: a clean, fast-loading blog that lets the content shine.

## Deployment

GitHub Pages makes deployment trivial. With a simple `make github` command, the site is built and pushed to the `main` branch, where GitHub serves it automatically.

If you're considering starting a blog, I'd recommend giving Pelican a try.
