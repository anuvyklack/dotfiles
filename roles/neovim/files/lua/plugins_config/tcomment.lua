local available, whick_key = pcall(require, "which-key")
if not available then return end

whick_key.register({
    ['gc'] = { name = "Comments" },
    ['gcc'] = "Current line"
})
