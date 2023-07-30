-- Standard awesome library
local awful     = require("awful")
-- Theme handling library
local beautiful = require("beautiful")
local vars      = RC.vars

local _M        = {}

-- reading
-- https://awesomewm.org/apidoc/libraries/awful.rules.html

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

function _M.get(clientkeys, clientbuttons)
  local rules = {

    -- All clients will match this rule.
    {
      rule = {},
      properties = {
        border_width = beautiful.border_width,
        border_color = beautiful.border_normal,
        focus        = awful.client.focus.filter,
        raise        = true,
        keys         = clientkeys,
        buttons      = clientbuttons,
        screen       = awful.screen.preferred,
        placement    = awful.placement.no_overlap + awful.placement.no_offscreen
      }
    },

    -- Floating clients.
    {
      rule_any = {
        role = {
          "AlarmWindow",   -- Thunderbird's calendar.
          "ConfigManager", -- Thunderbird's about:config.
          "pop-up",        -- e.g. Google Chrome's (detached) Developer Tools.
        }
      },
      properties = {
        { floating = true },
      }
    },
    { rule_any = { class = { vars.browser, vars.browser2 } }, properties = { tag = awful.screen.focused().tags[1] }, },
    { rule_any = { class = { "libreoffice-writer" } },        properties = { tag = awful.screen.focused().tags[3] }, },
    { rule_any = { class = { "calibre", "Zotero" } },         properties = { tag = awful.screen.focused().tags[4] }, },
    { rule_any = { class = { "thunderbird" } },               properties = { tag = awful.screen.focused().tags[5] }, },
    { rule_any = { class = { "discord", "slack" } },          properties = { tag = awful.screen.focused().tags[6] }, },
    { rule_any = { class = { vars.filemanager } },            properties = { tag = awful.screen.focused().tags[7] }, },
    { rule_any = { class = { "vlc", "Spotify" } },            properties = { tag = awful.screen.focused().tags[8] }, },
    { rule_any = { class = { "XIVLauncher" } },               properties = { tag = awful.screen.focused().tags[9] }, },

    -- Add titlebars to normal clients and dialogs
    {
      rule_any = {
        type = { "dialog" },
      },
      properties = {
        titlebars_enabled = true
      }
    },

  }

  return rules
end

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

return setmetatable({}, { __call = function(_, ...) return _M.get(...) end })
