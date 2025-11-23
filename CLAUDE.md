# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a personal website built with Hakyll, a static site generator written in Haskell. The site includes blog posts, presentations, letters (PDFs), a bookshelf page, and automatically generates an RSS feed and tag-based navigation.

## Build System

### Compilation and Building

Hakyll sites are built by compiling the `site.hs` file into an executable, then running that executable to generate the static site.

**Standard build workflow:**
```bash
# Compile the site generator
ghc --make site.hs

# Build the site (generates _site/ directory)
./site build

# Preview with local server
./site preview  # Usually runs on http://localhost:8000

# Clean generated files
./site clean

# Rebuild from scratch
./site rebuild
```

**Build artifacts:**
- `site` - The compiled Hakyll executable (gitignored)
- `_site/` - Generated static site output (gitignored)
- `_cache/` - Hakyll's internal cache (gitignored)
- `*.o`, `*.hi` - Haskell compilation artifacts (gitignored)

## Architecture

### Site Generator (`site.hs`)

The `site.hs` file defines the entire site generation logic using Hakyll's DSL. It uses arrow-based composition (`>>>`) to chain compilation steps.

**Key routing rules:**
- **Images** (`images/*`) - Copied directly
- **CSS** (`css/*`) - Compressed with `compressCssCompiler`
- **Posts** (`posts/*`) - Markdown files with YAML frontmatter, converted to HTML with date formatting, tag rendering, and template application
- **Post listings** - Automatically generated from all posts, sorted chronologically
- **Tags** - Extracted from post metadata, generates tag cloud and per-tag post listings
- **Index page** - Shows recent 5 posts, tag cloud
- **Presentations** (`presentations/**`) - Copied recursively
- **Letters** (`letters/**`) - Copied recursively (contains PDFs)
- **RSS feed** (`rss.xml`) - Generated from all posts using `feedConfiguration`

### Content Structure

**Posts** (`posts/` directory):
- Markdown files with format: `YYYY-MM-DD-slug.markdown`
- Required frontmatter: `title`, `author`, `date`, `tags`, `description`
- Posts are chronologically sorted, with newest first on listings

**Templates** (`templates/` directory):
- `default.html` - Base layout with navigation, footer, analytics
- `index.html` - Homepage layout with about section, recent posts, tag cloud
- `post.html` - Individual post layout
- `posts.html` - Post listing page
- `postitem.html` - Template for rendering individual post items in lists

**Content pages:**
- `index.markdown` - Homepage content (currently placeholder text)
- `bookshelf.markdown` - Bookshelf page
- `404.html` - Custom 404 page

### Compiler Pipeline

Posts go through this compilation pipeline:
1. Parse markdown → HTML
2. Render date field in format "Month Day, Year"
3. Render tags with links to tag pages
4. Apply post template
5. Copy body to description field
6. Apply default template (wraps in site layout)
7. Relativize URLs

### Tag System

- Tags are extracted from all posts
- Tag cloud is generated with font sizes 100-120
- Each tag gets its own listing page at `tags/<tagname>.html`
- Uses `tagIdentifier` function to create consistent tag URLs

### RSS Feed

Feed configuration in `feedConfiguration`:
- Title: "Alen Ribic"
- Author: Alen Ribic (alen.ribic@gmail.com)
- Root: http://alenribic.com
- Generated from all posts at `rss.xml`

## Development Workflow

1. **Content changes** (posts, pages, templates):
   - Modify the markdown/HTML files
   - Run `./site rebuild` to regenerate
   - Check `_site/` for output

2. **Site logic changes** (`site.hs`):
   - Edit `site.hs`
   - Recompile: `ghc --make site.hs`
   - Rebuild site: `./site rebuild`

3. **Static assets** (images, CSS, presentations, letters):
   - Add files to respective directories
   - Ensure routing rules in `site.hs` match
   - Files are copied directly or compressed (CSS)

## Important Notes

- This is an older Hakyll version (uses deprecated arrow-based API with `>>>`, `arr`, etc.)
- Modern Hakyll uses applicative-style composition, but don't refactor unless requested
- The site uses Google Analytics (UA-33873202-1)
- Navigation includes: Home, Posts, Bookshelf
- Footer displays "© MMXIV Alen Ribic λ" (Roman numerals for 2014)
