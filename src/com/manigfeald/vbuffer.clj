(ns com.manigfeald.vbuffer
  (:import (javax.xml.bind DatatypeConverter)
           (java.nio ByteBuffer)))

(defprotocol Buffer
  (get-byte [buf pos])
  (get-bytes [buf pos bb]))

(deftype ValueBuffer [^bytes b ^:unsynchronized-mutable hash m]
  clojure.lang.IMeta
  (meta [_]
    m)
  clojure.lang.IObj
  (withMeta [_ met]
    (ValueBuffer. b hash met))
  clojure.lang.Counted
  (count [_]
    (alength b))
  clojure.lang.IHashEq
  (hasheq [this]
    (.hashCode this))
  Object
  (toString [_]
    (DatatypeConverter/printBase64Binary b))
  (equals [o1 o2]
    (boolean
     (when (instance? ValueBuffer o2)
       (when (= (count o2) (count o1))
         (let [^ValueBuffer o2 o2
               ^bytes a (.-b o2)
               c (count o2)]
           (loop [i 0]
             (if (> c i)
               (if (= (aget a i)
                      (aget b i))
                 (recur (inc i))
                 false)
               true)))))))
  (hashCode [_]
    (unchecked-int
     (if (== -1 hash)
       (let [c (count b)
             h (clojure.lang.Murmur3/mixCollHash
                (unchecked-int
                 (loop [i 0
                        h 1]
                   (if (> c i)
                     (let [a (inc i)
                           f (unchecked-multiply-int 31 h)
                           e (int i)
                           d (byte (aget b e))
                           b (unchecked-add-int f d)]
                       (recur a b))
                     h)))
                (count b))]
         (set! hash h)
         h)
       hash)))
  Buffer
  (get-byte [buf pos]
    (aget b (int pos)))
  (get-bytes [buf pos bb]
    (let [^java.nio.ByteBuffer bb bb
          c (min (count b) (.remaining bb))]
      (dotimes [i c]
        (.put bb (byte (get-byte buf (+ pos i)))))
      c)))

(defn rget-bytes [bb pos buf]
  (get-bytes buf pos bb))

(defn value-buffer
  "create a value buffer from a byte array"
  [bytes]
  (ValueBuffer. (aclone ^bytes bytes) -1 {}))

(defn to-biginteger
  "value buffer to biginteger"
  [buf]
  (let [bb (java.nio.ByteBuffer/allocate (count buf))]
    (get-bytes buf 0 bb)
    (BigInteger. 1 ^bytes (.array bb))))

(defn xor [buf-a buf-b]
  (let [s (max (count buf-a)
               (count buf-b))
        r (byte-array s)]
    (dotimes [i s]
      (let [x (byte (if (> (count buf-b) i)
                      (get-byte buf-b i)
                      0))
            y (byte (if (> (count buf-a) i)
                      (get-byte buf-a i)
                      0))]
        (aset r i (unchecked-byte (bit-xor x y)))))
    (value-buffer r)))

(defn buf-and [buf-a buf-b]
  (let [s (max (count buf-a)
               (count buf-b))
        r (byte-array s)]
    (dotimes [i s]
      (let [x (byte (if (> (count buf-b) i)
                      (get-byte buf-b i)
                      0))
            y (byte (if (> (count buf-a) i)
                      (get-byte buf-a i)
                      0))]
        (aset r i (unchecked-byte (bit-and x y)))))
    (value-buffer r)))

(defn buf-or [buf-a buf-b]
  (let [s (max (count buf-a)
               (count buf-b))
        r (byte-array s)]
    (dotimes [i s]
      (let [x (byte (if (> (count buf-b) i)
                      (get-byte buf-b i)
                      0))
            y (byte (if (> (count buf-a) i)
                      (get-byte buf-a i)
                      0))]
        (aset r i (unchecked-byte (bit-or x y)))))
    (value-buffer r)))

(defn get-int [buf pos]
  (let [a (bit-and (get-byte buf pos) 0xff)
        b (bit-and (get-byte buf (+ 1 pos)) 0xff)
        c (bit-and (get-byte buf (+ 2 pos)) 0xff)
        d (bit-and (get-byte buf (+ 3 pos)) 0xff)]
    (bit-or (bit-shift-left a (* 3 8))
            (bit-shift-left b (* 2 8))
            (bit-shift-left c (* 1 8))
            (bit-shift-left d (* 0 8)))))

(defn get-long [buf pos]
  (let [a (bit-and (get-int buf pos)
                   (unchecked-int 0xffffffff))
        b (bit-and (get-int buf (+ 4 pos))
                   (unchecked-int 0xffffffff))]
    (bit-or (bit-shift-left a (* 4 8))
            (bit-shift-left b (* 0 8)))))

(defn leading-zeros [buf]
  (loop [n 0
         i (dec (count buf))
         a 7]
    (if (neg? i)
      n
      (if (neg? a)
        (recur n (dec i) 7)
        (let [x (bit-and (bit-shift-left (get-byte buf i) a) 1)]
          (if (zero? x)
            (recur (inc n) i (dec a))
            n))))))

(defn test-bit [buf i]
  (if (> i (count buf))
    false
    (let [ii (quot i 8)
          iii (rem i 8)
          b (get-byte buf ii)]
      (not (zero? (bit-and (bit-shift-left b iii) 1))))))

(defn from-byte-buffer [bb]
  (let [b (byte-array (.remaining bb))]
    (.get bb b)
    (value-buffer b)))

(defn from-int [i]
  (value-buffer
   (.array
    (doto (ByteBuffer/allocate 4)
      (.putInt (unchecked-int i))))))

(defn from-long [l]
  (value-buffer
   (.array
    (doto (ByteBuffer/allocate 8)
      (.putInt (unchecked-long l))))))

(defn concat-buffers [& bufs]
  (let [s (apply + (map count bufs))
        buf (ByteBuffer/allocate s)]
    (doseq [b bufs]
      (.put buf (.-b b)))
    (value-buffer (.array buf))))


