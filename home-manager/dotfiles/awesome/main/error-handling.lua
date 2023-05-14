-- Notification library
local naughty = require("naughty")
local device = RC.vars.device

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
  naughty.notify({
    preset = naughty.config.presets.critical,
    title = "Oops, there were errors during startup!",
    text = awesome.startup_errors
  })
end

-- Handle runtime errors after startup
do
  local in_error = false
  awesome.connect_signal("debug::error", function(err)
    -- Make sure we don't go into an endless error loop
    if in_error then return end
    in_error = true

    naughty.notify({
      preset = naughty.config.presets.critical,
      title = "Oops, an error happened!",
      text = tostring(err)
    })
    in_error = false
  end)
end

-- notify if environment variables aren't set correctly - Alan
if device == "" then
  naughty.notify({
    preset = naughty.config.presets.critical,
    title = "Error in dotfiles; entering fallback config",
    text = "Hostname must contain 'laptop' or 'desktop' and not both for my dotfiles to work correctly. Please refer to the README."
  })
end
