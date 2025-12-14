THEME = "themes/julian"
AUTHOR = 'Dan Falkenheim'
SITENAME = 'Dan Falkenheim'
SITEURL = ""

PATH = "content"

TIMEZONE = 'America/New_York'

DEFAULT_LANG = 'en'

# Tagline displayed below site title
TAGLINE = "thoughts on technology and life"

# Feed generation is usually not desired when developing
FEED_ALL_ATOM = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None
AUTHOR_FEED_ATOM = None
AUTHOR_FEED_RSS = None

# Navigation menu items
MENUITEMS = (
    ("archives", "/archives.html"),
)

# Social links
SOCIAL = (
    ("GitHub", "https://github.com/thefalkon-1"),
)

# Hide categories and authors from menu
DISPLAY_CATEGORIES_ON_MENU = False
DISPLAY_PAGES_ON_MENU = True

DEFAULT_PAGINATION = 10

# Article URL structure
ARTICLE_URL = '{slug}/'
ARTICLE_SAVE_AS = '{slug}/index.html'
PAGE_URL = '{slug}/'
PAGE_SAVE_AS = '{slug}/index.html'

# Uncomment following line if you want document-relative URLs when developing
# RELATIVE_URLS = True
