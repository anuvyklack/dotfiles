# This file is part of ranger, the console file manager.
# License: GNU GPL version 3, see the file "AUTHORS" for details.

from __future__ import (absolute_import, division, print_function)

from ranger.colorschemes.default import Default
from ranger.gui.color import (
    black, blue, cyan, green, magenta, red, white, yellow, default,
    normal, bold, reverse, dim, BRIGHT,
    default_colors,
)


# class CustomDefault(Default):
class Scheme(Default):
    progress_bar_color = blue

    def use(self, context):  # pylint: disable=too-many-branches,too-many-statements
        fg, bg, attr = Default.use(self, context)

        # if context.in_browser and context.selected:
        #     if context.container:
        #         bg = white

        if context.in_titlebar:
            if context.hostname:
                fg = red if context.bad else green
            elif context.directory:
                fg = blue
            elif context.tab:
                if context.good:
                    bg = green
                    fg = black
            elif context.link:
                fg = cyan
            attr |= bold

        return fg, bg, attr
