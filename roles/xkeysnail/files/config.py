import re
from xkeysnail.transform import *

# [Multipurpose modmap] Give a key two meanings. A normal key when pressed and
# released, and a modifier key when held down with another key. See Xcape,
# Carabiner and caps2esc for ideas and concept.
define_multipurpose_modmap(
    # Enter is enter when pressed and released. Control when held down.
    {Key.SPACE: [Key.SPACE, Key.RIGHT_CTRL]}
)

# define_multipurpose_modmap(
#     # Capslock is escape when pressed and released. Control when held down.
#     {Key.CAPSLOCK: [Key.ESC, Key.LEFT_CTRL]}
# )

define_keymap(None, {
    K("CAPSLOCK"): K("ESC")
})
