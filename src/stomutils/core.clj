(ns stomutils.core
  (:require [lambdaisland.classpath :as cp]
            [clojure.reflect :as reflect]
            [clojure.string :as str])
  (:import [java.util Arrays]
           [java.lang.reflect Field]
           [net.minestom.server MinecraftServer]
           [net.minestom.server.instance.generator GenerationUnit]
           [net.minestom.server.entity Player Entity]
           [net.minestom.server.event GlobalEventHandler]
           [net.minestom.server.instance.generator Generator]
           [net.minestom.server.instance.block Block]
           [net.minestom.server.item ItemStack Material]
           [net.minestom.server.utils.chunk ChunkSupplier]
           [net.kyori.adventure.text TextComponent Component]
                                        ;pvp
           ;; [io.github.togar2.pvp MinestomPvP]
           ;; [io.github.togar2.pvp.feature CombatFeatures]
           )
  (:gen-class))

(set! *warn-on-reflection* true)

(defn munge-jstr [s]
  (-> (str s)
      (str/replace #".*\." "")
      (str/replace #"_" "-")
      (str/replace #"Event$" "")
      (str/replace #"([a-z])([A-Z])" (fn [[_ a A]]
                                       (str a "-" A)))
      (str/lower-case)))

(defn fn-with-accepting-type [mfn class]
  (-> (into [] mfn) (update-in [1 0] with-meta {:tag class}) seq))

(defn static-fields [^Class c]
  (into {}
        (for [f (filter #(bit-and java.lang.reflect.Modifier/STATIC java.lang.reflect.Modifier/PUBLIC (.getModifiers ^Field %))
                        ^Field (.getFields c))]
          [(keyword (munge-jstr (.getName ^Field f)))
           (.get (doto ^Field f (.setAccessible true)) nil)])))

(defmacro idiofy-static-enum-class [^Class c]
  (let [munged-name (munge-jstr c)
        getfn (symbol munged-name)
        keyreg-name (symbol (str "-" munged-name))
        protocol-name (symbol (str "Has-" (clojure.string/capitalize munged-name)))]

    `(do (def ~keyreg-name (static-fields ~c))
         (defprotocol ~protocol-name
           (~getfn ~(with-meta '[this] {:tag (symbol (reflect/typename c))})))
         (extend-protocol ~protocol-name
           ~c
           (~getfn [this#] this#)
           clojure.lang.Keyword
           (~getfn [this#] (get ~keyreg-name this#))))))


(idiofy-static-enum-class Block)
(idiofy-static-enum-class Material)
;(idiofy-static-enum-class CombatFeatures)
(defprotocol Has-Item-stack
  (item-stack ^ItemStack [this]))
(defprotocol Has-Text-component
  (text-component ^TextComponent [this]))

(extend-protocol Has-Text-component
  String
  (text-component [this] (Component/text this))) ; todo -- add markup parser

(extend-protocol Has-Block
  clojure.lang.IPersistentMap
  (block [x]
    (let [b (block (:type x))
          props (dissoc x :type)]
      (if (not (empty? props))
        (.withProperties ^Block b props)))))





(extend-protocol Has-Item-stack
  clojure.lang.Keyword
  (item-stack [x] (-> (ItemStack/builder (material x)) .build))
  ItemStack
  (item-stack [x] x)
  clojure.lang.IPersistentMap
  (item-stack [x]
    (let [stack (ItemStack/builder (material (:material x)))]
          (and (:amount x) (.amount stack (:amount x)))
          (and (:lore x) (.lore stack (Arrays/asList (to-array (map text-component (:lore x))))))
          (and (:max-stack-size x) (.maxStackSize stack (:max-stack-size x)))
          (and (:custom-name x) (.customName stack (:custom-name x)))
          (and (:glowing x) (.glowing stack (:glowing x)))
          (and (:custom-model-data x) (.customModelData stack (:custom-model-data x)))
          (.build stack))))




(def -server  (atom nil))
(def -instance-manager (atom nil))
(def -event-handler (atom nil))


(defn ^MinecraftServer server []
  @-server)
(defn ^net.minestom.server.instance.InstanceManager  instance-manager []
  @-instance-manager)
(defn ^GlobalEventHandler event-handler []
  @-event-handler)
(defn instance-container []
  (.createInstanceContainer ^net.minestom.server.instance.InstanceManager (instance-manager)))



(defn find-event-classes []
  (cp/find-resources #".*minestom.*Event\.class"))
(def event-classes
  (let [classes (find-event-classes)
        classes (if (empty? classes)
                  (find-event-classes)
                  classes)]
    (map #(-> % (str/replace #"/" ".") (str/replace #".class$" ""))
         classes)))
(def events
  (into {}
        (for [i event-classes]
          [(keyword (munge-jstr i)) (Class/forName i)])))
(defmacro listen! [node event mfn]
  (let [e (cond
            (keyword? event)
            (symbol (reflect/typename (get events event)))
            (symbol? event)
            event)
        fun (fn-with-accepting-type mfn e)]

    `(.addListener ~(with-meta node {:tag `GlobalEventHandler}) ~e
                   (reify java.util.function.Consumer
                     (accept [this# arg#] 
                       (~fun arg#))))))

(defn give-items [^Player p & items]
  (doseq [i items]
    (.addItemStack (.getInventory p) ^ItemStack (item-stack i))))

;; (defn enable-pvp! [event-handler coll]
;;   (.addChild ^GlobalEventHandler event-handler
;;              (.createNode (.build
;;                            (.addAll (io.github.togar2.pvp.feature.CombatFeatures/empty)
;;                                     (Arrays/asList (to-array (map #(combat-features %) coll))))))))


(defmacro set-generator [world genfn]
  `(.setGenerator ~(with-meta world {:tag `net.minestom.server.instance.InstanceContainer})
                  (reify Generator
                    (generate [this unit#]
                      (~(fn-with-accepting-type genfn `GenerationUnit) unit#)))))


(defn init []
  (reset! -server (MinecraftServer/init))
  ;(MinestomPvP/init)
  (reset! -instance-manager (MinecraftServer/getInstanceManager))
  (reset! -event-handler (MinecraftServer/getGlobalEventHandler)))


