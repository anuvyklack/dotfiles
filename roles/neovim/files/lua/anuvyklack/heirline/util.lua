local util = {}

util.icons = {
   powerline = {
      -- 
      vertical_bar_thin  = '│',
      vertical_bar       = '┃',
      block              = '█',
      ----------------------------------------------
      left  = '', left_filled  = '',
      right = '', right_filled = '',
      ----------------------------------------------
      slant_left    = '', slant_left_thin    = '',
      slant_right   = '', slant_right_thin   = '',
      ----------------------------------------------
      slant_left_2  = '', slant_left_2_thin  = '',
      slant_right_2 = '', slant_right_2_thin = '',
      ----------------------------------------------
      left_rounded  = '', left_rounded_thin  = '',
      right_rounded = '', right_rounded_thin = '',
      ----------------------------------------------
      trapezoid_left  = '', trapezoid_right = '',
      ----------------------------------------------
      line_number   = '', column_number = '',
   },
   padlock      = '',
   circle_small = '●', -- ●
   circle       = '', -- 
   circle_plus  = '', -- 
   dot_circle_o = '', -- 
   circle_o     = '⭘', -- ⭘
}

util.mode = setmetatable({
   n        = 'normal' ,
   no       = 'op',
   nov      = 'op',
   noV      = 'op',
   ["no"] = 'op',
   niI      = 'normal',
   niR      = 'normal',
   niV      = 'normal',
   nt       = 'normal',
   v        = "visual",
   V        = "visual_lines",
   [""]   = "visual_block",
   s        = "select",
   S        = "select",
   [""]   = "block",
   i        = "insert",
   ic       = "insert",
   ix       = "insert",
   R        = "replace",
   Rc       = "replace",
   Rv       = "v_replace",
   Rx       = "replace",
   c        = "command",
   cv       = "command",
   ce       = "command",
   r        = "enter",
   rm       = "more",
   ["r?"]   = "confirm",
   ["!"]    = "shell",
   t        = "terminal",
   ["null"] = "none",
}, {
   __call = function (self, raw_mode)
      return self[raw_mode]
   end
})

util.mode_lable = {
   normal       = "NORMAL",
   op           = "OP",
   visual       = "VISUAL",
   visual_lines = "VISUAL LINES",
   visual_block = "VISUAL BLOCK",
   select       = "SELECT",
   block        = "BLOCK",
   insert       = "INSERT",
   replace      = "REPLACE",
   v_replace    = "V-REPLACE",
   command      = "COMMAND",
   enter        = "ENTER",
   more         = "MORE",
   confirm      = "CONFIRM",
   shell        = "SHELL",
   terminal     = "TERMINAL",
   none         = "NONE"
}

return util
