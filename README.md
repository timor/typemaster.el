# Description #
This package provides a simple yet effective typing trainer.  It finds out which
keys or key combinations the user has difficulty with, and presents these more
often.  Different training sets are supplied, and the functions to generate
those are also present.

# Training Sets #
The following training sets are present:
- english text
- german text
- python code

## Generating Training Sets ##
The functions for that are present in [typemaster-extract.el](typemaster-extract.el).
Generating a Training set generally works like this:
1. Generate/Provide some files with example input
2. Read in those files, possibly removing/substituting characters, all the while
   adding character information to an index
3. Save that index to a (gzipped) file

Some helpers exist for step 1, especially fetching stuff from wikipedia.
