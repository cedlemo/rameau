# Rameau

Rameau is a console client for Mpd (Music Player Daemon). It is build in OCaml
with Notty and OCaml-libmpdclient.

[Jean-Philippe Rameau](https://en.wikipedia.org/wiki/Jean-Philippe_Rameau).

```
  git clone git@github.com:cedlemo/OCaml-libmpdclient.git
  opam pin add libmpdclient libmpdclient
  git clone git@github.com:cedlemo/rameau.git
  opam pin add rameau rameau
  rameau
```

# Controls in the Queue view.

 * j/k -> next / previous song in the queue list
 * Enter -> play the selected song
 * s -> stop playing
 * p -> toggle pause
