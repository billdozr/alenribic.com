# Alenribic.com - Personal Website

Source code of my personal website http://alenribic.com/

This is a personal website and blog built with [Hakyll](https://jaspervdj.be/hakyll/), a static site generator written in Haskell.

## Recent Upgrade (2025-11-23)

The site has been successfully upgraded from Hakyll 3.x/4.x (arrow-based API) to Hakyll 4.16.x (modern applicative API).

## Prerequisites

- GHC 9.6.7
- Cabal 3.12.1.0
- Hakyll 4.16.x

If you don't have these installed, you can use GHCup:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
export PATH="$HOME/.ghcup/bin:$PATH"
```

## Building the Site

### First Time Build

```bash
# Update Cabal package index
cabal update

# Build the site generator
cabal build

# Generate the static site
cabal run site rebuild
```

### Subsequent Builds

```bash
# Clean and rebuild everything
cabal run site rebuild

# Or just rebuild changed files
cabal run site build
```

## Development Workflow

### Preview the Site Locally

```bash
# Start the preview server (usually at http://localhost:8000)
cabal run site watch
```

This will:
- Build the site
- Start a local web server
- Watch for changes and rebuild automatically

### Cleaning Up

```bash
# Remove generated files
cabal run site clean

# Clean Cabal build artifacts
cabal clean
```

## Project Structure

```
.
├── site.hs                 # Main site generator configuration
├── alenribic-site.cabal   # Cabal project file
├── posts/                  # Blog posts in Markdown
├── templates/              # HTML templates
│   ├── default.html       # Base layout
│   ├── index.html         # Homepage
│   ├── post.html          # Individual post
│   └── posts.html         # Post listing
├── css/                    # Stylesheets
├── images/                 # Static images
├── presentations/          # Presentation files
├── letters/                # PDF documents
├── _site/                  # Generated static site (gitignored)
└── _cache/                 # Hakyll cache (gitignored)
```

## Adding New Posts

1. Create a new Markdown file in `posts/` with the format: `YYYY-MM-DD-slug.markdown`

2. Add YAML frontmatter:
```markdown
---
title: "Your Post Title"
author: Alen Ribic
date: Month DD, YYYY
tags: tag1, tag2
description: A brief description of your post
---

Your post content here...
```

**Important:** If your title contains colons (`:`) or hyphens in certain positions, wrap it in quotes to avoid YAML parsing errors.

3. Rebuild the site:
```bash
cabal run site rebuild
```

## Deployment

The `_site/` directory contains the complete static website ready to be deployed to any static hosting service (GitHub Pages, Netlify, Vercel, AWS S3, etc.).

Simply copy the contents of `_site/` to your hosting provider.

## Configuration

Edit `site.hs` to:
- Modify site generation rules
- Change RSS feed settings
- Update routing logic
- Customize template application

After changes to `site.hs`, rebuild:
```bash
cabal build
cabal run site rebuild
```

## Migration Notes

### Changes from Old Version

1. **Arrow-based → Applicative API**: The site has been migrated from the deprecated arrow-based API (`>>>`, `arr`, etc.) to the modern applicative-style Hakyll API
2. **Template Syntax**: Updated templates to use `$for(posts)$...$endfor$` instead of direct `$posts$` substitution
3. **Build System**: Now using modern Cabal with explicit `.cabal` file
4. **YAML Fixes**: Fixed post titles with special characters by adding quotes

### Files Modified

- `site.hs` - Complete rewrite using modern Hakyll API
- `templates/posts.html` - Updated template syntax
- `templates/index.html` - Updated template syntax
- `posts/2012-07-08-haskell-an-attempt-at-tic-tac-toe.markdown` - Fixed YAML
- `posts/2012-08-01-hs-task-an-in-code-task-management-utility.markdown` - Fixed YAML

## RSS Feed

The site automatically generates an RSS feed at `/rss.xml` with the 10 most recent posts.

## Tags

Posts are automatically tagged, and tag pages are generated at `/tags/[tagname].html`.
A tag cloud is displayed on the homepage.

## Troubleshooting

### Build Fails

```bash
# Clean everything and rebuild
cabal clean
cabal run site clean
cabal build
cabal run site rebuild
```

### YAML Parse Errors

If you see YAML parsing errors when adding posts, ensure:
- Titles with colons are wrapped in quotes: `title: "My Title: Subtitle"`
- The frontmatter has proper spacing (space after colons)
- There are no tabs in the frontmatter

## License

© 2014 Alen Ribic

## Links

- Author: [Alen Ribic](http://alenribic.com)
- Twitter: [@alenribic](https://twitter.com/alenribic)
- GitHub: [billdozr](https://github.com/billdozr)
