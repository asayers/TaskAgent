A simple CLI to-do list application. Designed to consume and produce files compatible with [TaskAgent][1]. Tested on Linux; should be fairly portable, but I'll make no promises. BSD 3-clause licenced.

Configuration and Installation
------------------------------

You can specify the location of your list files by changing `defaultListDirectory` in `todo.hs`. If, say, your dedication to to-do lists is such that your lists are stored in /lists, just set `defaultListDirectory = Just "/lists"`, and you're good to go.

If you don't specify a location, we try our best to guess. The default is ~/Dropbox/Apps/TaskAgent, which is where they should be under a normal Dropox/TaskAgent setup.

Once you're happy with with this, just run `make` to generate a binary. `make install` will copy it to ~/bin, but feel free to put it where you like.

Usage
-----

- `todo` to list to-dos
- `todo new` to add a new one
- `todo n` to mark to-do number *n* as done

To-Do-ception
-------------

- Support multiple lists (at the moment, we only use the list titled "todo").


[1]: http://macrecon.com/app/TaskAgent/
