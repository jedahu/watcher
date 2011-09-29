(ns me.panzoo.watcher
  "A clojureish wrapper round the Watch Service API as implemented in
  [jpathwatch](http://jpathwatch.wordpress.com)."
  (:use
    [clojure.string :only (join)]
    [clojure.core.incubator :only (dissoc-in)])
  (:import
    [java.io File FileReader BufferedReader]
    [name.pachler.nio.file
     WatchService WatchKey Path Paths FileSystems
     StandardWatchEventKind]
    [name.pachler.nio.file.ext ExtendedWatchEventModifier]))

(def ^:private ENTRY_CREATE StandardWatchEventKind/ENTRY_CREATE)
(def ^:private ENTRY_DELETE StandardWatchEventKind/ENTRY_DELETE)
(def ^:private ENTRY_MODIFY StandardWatchEventKind/ENTRY_MODIFY)

(def ^:private event-kinds
  (into-array [ENTRY_CREATE ENTRY_DELETE ENTRY_MODIFY]))

;; Does ACCURATE make a difference on Linux?
(def ^:private event-modifiers
  (into-array [ExtendedWatchEventModifier/ACCURATE]))

(defn- path-pieces [path]
  (vec (.split path File/separator)))

(defn- pieces-path [pieces]
  (join File/separator pieces))

(defn- watch-path-single [{:keys [service pathmap keytree]} path]
  (let [wkey (.register (Paths/get path) service
                        event-kinds event-modifiers)
        pieces (path-pieces path)]
    (swap! pathmap #(assoc % wkey pieces))
    (swap! keytree #(assoc-in % (conj pieces :watch-key) wkey))))

(defn- watch-path-recursive [w path]
  (watch-path-single w path)
  (doseq [p (file-seq (File. path))
          :when (.isDirectory p)]
    (watch-path-single w (.getPath p))))

(defn watch-path
  "Register `path` with `watcher`. If `watcher` is recursive and `path` is a
  directory, all subdirectories will be registered too."
  [watcher path]
  ((if (:recursive? watcher) watch-path-recursive watch-path-single)
     watcher path))

(defn- unwatch-path-single [{:keys [pathmap keytree]} path]
  (let [pieces (path-pieces path)
        wkey (get-in @keytree (conj pieces :watch-key))]
    (when wkey
      (.cancel wkey)
      (swap! pathmap #(dissoc % wkey))
      (swap! keytree #(dissoc-in % (conj pieces :watch-key))))))

(defn- unwatch-path-recursive [{:keys [pathmap keytree]} path]
  (let [pieces (path-pieces path)
        wkeys (for [x (tree-seq map? vals (get-in @keytree pieces))
                    :when (:key x)]
                (:key x))]
    (doseq [k wkeys]
      (.cancel k))
    (swap! pathmap #(apply dissoc % wkeys))
    (swap! keytree #(dissoc-in % pieces))))

(defn unwatch-path
  "Unregister `path` with `watcher`. If `watcher` is recursive and `path`
  is a directory, all subdirectories will be unregistered too."
  [watcher path]
  ((if (:recursive? watcher) unwatch-path-recursive unwatch-path-single)
     watcher path))

(defn watcher [paths & opts]
  "Create a watcher and register `paths` with it. `opts` is a sequence and
  `:recursive` is the only option to date; if it is present, registration and
  unregistration of directories is recursive."
  (let [w {:service (.newWatchService (FileSystems/getDefault))
           :recursive? (:recursive (set opts))
           :pathmap (atom nil)
           :keytree (atom nil)}]
    (doseq [p paths]
      (watch-path w p))
    w))

(defn- events [{:keys [service pathmap keymap] :as w}]
  (let [wkey (.take service)
        path (get @pathmap wkey)
        evts (.pollEvents wkey)]
    (.reset wkey)
    (let [evts* (map
                  #(let [fillmap
                         (fn [kind]
                           (let [p (.toString (.resolve (Paths/get (pieces-path path))
                                                        (.context %)))]
                             {:kind kind
                              :path p
                              :directory? (.isDirectory
                                            (File. p))}))
                         kind (.kind %)]
                     (cond
                       (= kind ENTRY_CREATE) (fillmap :create)
                       (= kind ENTRY_DELETE) (fillmap :delete)
                       (= kind ENTRY_MODIFY) (fillmap :modify)
                       :else {:kind :unknown}))
                  evts)]
      (doseq [e evts*
              :when (and (= :create (:kind e))
                         (:directory? e))]
        (watch-path w (:path e)))
      (doseq [e evts*
              :when (= :delete (:kind e))]
        (unwatch-path w (:path e)))
      evts*)))

(defn with-watcher
  "Wait for events from `watcher` and call `fun` with them.

  `fun`: a function with a single argument of a sequence of events.

  Events are maps. Path events have the following key-value pairs:

  `:kind` one of `:create` `:delete` `:modify`

  `:directory?` a boolean (always false when `:kind` is `:delete`)

  `:path` a string

  All other events are simply `{:kind :unknown}`."
  [watcher fun]
  (loop [evts (events watcher)]
    (when (seq evts)
      (fun evts))
    (recur (events watcher))))

(defn with-watch-paths
  "A convencience function wrapping `with-watcher`.
  
  Watch `paths` and call `fun` with any events. `opts` is simply passed to
  the `watcher` function.

  See also `watcher` and `with-watcher`."
  [paths fun & opts]
  (with-watcher (apply watcher paths opts) fun))
