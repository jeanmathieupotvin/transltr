# Tokens and tags --------------------------------------------------------------


# This constant defines a subtoken of R's "COMMENT" token. It marks a
# line in a source code as a comment that only relates to the package.
# It has a regular expression to ease the parsing process.
#
# ^        : matches the beginning of a string;
# #~       : matches #~ literally;
# [ \t\n]* : matches space(s), tab(s), and new lines ([ \t\n])
#            zero or multiple times (*);
.TRANSLTR_COMMENT <- structure("#~", regex = "^#~[ \t\n]*")


# Code parsing and analysis ----------------------------------------------------


# These character strings are used to mark text to translate.
# They each have a regular expression used to identify them.
#
# ^      : matches the beginning of a string;
# $      : matches the end of a string;
# \"?    : matches a possibly missing double string character;
# [ \t]* : matches space(s) and tab(s) ([ \t]) zero or multiple times (*);
# \\{\\{ : matches {{ literally;
# \\}\\} : matches }} literally.
.TRANSLTR_L_TAG <- structure("{{", regex = "^\"?[ \t]*\\{\\{[ \t]*")
.TRANSLTR_R_TAG <- structure("}}", regex = "[ \t]*\\}\\}[ \t]*\"?$")
