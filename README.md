# Rameau

Rameau is a console client for Mpd (Music Player Daemon). It is build in OCaml
with Notty and OCaml-libmpdclient.

[Jean-Philippe Rameau](https://en.wikipedia.org/wiki/Jean-Philippe_Rameau).


<a href="https://raw.github.com/cedlemo/rameau/master/screenshot.png"><img src="https://raw.github.com/cedlemo/rameau/master/screenshot.png" alt="Rameau Preview"></a>

## Installation

Rameau is not finished. For now just the Queue view is working. You can play,
stop, pause and select a song the already filled current playlist.

```
  git clone git@github.com:cedlemo/OCaml-libmpdclient.git
  opam pin add libmpdclient libmpdclient
  git clone git@github.com:cedlemo/rameau.git
  opam pin add rameau rameau
  rameau
```

## Controls in the Queue view.

 * j/k -> next / previous song in the queue list
 * Enter -> play the selected song
 * s -> stop playing
 * p -> toggle pause
 * "+" -> increase by one the volume
 * "-" -> decrease by one the volume
 * "q" or "Escape" or "Ctrl + C" quit rameau

# Unicode character list
https://en.wikipedia.org/wiki/List_of_Unicode_characters
