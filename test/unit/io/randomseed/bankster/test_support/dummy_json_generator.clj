(ns io.randomseed.bankster.test-support.dummy-json-generator
  (:import (java.util ArrayList))
  (:gen-class
   :name io.randomseed.bankster.test_support.DummyJsonGenerator
   :init init
   :state state
   :constructors {[] []}
   :methods [[writeStartObject [] void]
             [writeStringField [String String] void]
             [writeEndObject [] void]
             [writeString [String] void]
             [calls [] java.util.List]]))

(defn -init []
  [[] (ArrayList.)])

(defn -calls [this]
  (.state this))

(defn -writeStartObject [this]
  (.add ^ArrayList (.state this) [:writeStartObject])
  nil)

(defn -writeStringField [this k v]
  (.add ^ArrayList (.state this) [:writeStringField k v])
  nil)

(defn -writeEndObject [this]
  (.add ^ArrayList (.state this) [:writeEndObject])
  nil)

(defn -writeString [this s]
  (.add ^ArrayList (.state this) [:writeString s])
  nil)

