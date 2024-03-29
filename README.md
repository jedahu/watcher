# Watcher

[Full documentation](http://jedahu.github.com/watcher/uberdoc.html) with
[Marginalia](http://fogus.me/fun/marginalia/).

Usage:

    (use 'me.panzoo.watcher)

    (with-watch-paths ["/tmp/foo" "/var/log"]
      (fn [events]
        (doseq [e events]
          (prn e)))
      :recursive)

Events:

    {:kind        (or :create :delete :modify :unknown)
     :directory?  (or true false)
     :path        string}

Other functions:

    (def w (watcher ["/tmp" "/etc"] :recursive))

    (watch-path w "/var/log")

    (unwatch-path w "/etc")

    (with-watcher w (fn [events] ...))

`:recursive` is optional. If present `watch-path` will recursively add
subdirectories to the watcher, and `unwatch-path` will recursively remove them.
The paths given to `watcher` and `with-watch-paths` will be treated the same.

## Leiningen

    [me.panzoo/watcher "0.0.1"]
