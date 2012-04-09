(ns cloxo.instr
  (import [java.io File FileOutputStream]
          [java.nio ByteBuffer ByteOrder]))

(def valtype-encode-tbl
  { :A   0  :B 1    :C 2    :X 3    :Y 4    :Z 5    :I 6,   :J 7
    :A_r 8  :B_r 9  :C_r 10 :X_r 11 :Y_r 12 :Z_r 13 :I_r 14 :J_r 15
    :A_n 16 :B_n 17 :C_n 18 :X_n 19 :Y_n 20 :Z_n 21 :I_n 22 :J_n 23
    :POP 24 :PEEK 25 :PUSH 26 :SP 27
    :PC 28
    :O 29
    :NEXT_REF 30 :NEXT_LIT 31 })

(defn valtype-encode [k] (or (valtype-encode-tbl k) k))

(def op-encode
  { :EXT 0
    :SET 1 :ADD 2 :SUB 3
    :MUL 4 :DIV 5 :MOD 6
    :SHL 7 :SHR 8
    :AND 9 :BOR 10 :XOR 11
    :IFE 12 :IFN 13 :IFG 14 :IFB 15 })

(def ext-op-encode 
  { :JSR 1 })
;; Value 
; {:val value keyword, :word <word arg>, :label <Label ref for word arg>}
; If word and label are both present, word is an -offset- to add to the label
(defrecord Instruction [opcode a b])

(defn assemble-bracketed-value 
  ([reg] {:val (keyword (str (name reg) "_r"))})
  ([p & args]
    (let [[lbl] (filter keyword? args)
          [wrd] (filter number? args)
          [reg] (filter symbol? args)]
      (merge
        {:val (keyword (str (name reg) "_n"))}
        (if wrd {:word wrd} {})
        (if lbl {:label lbl} {}))
    )))

(defn assemble_value [vr]
  (cond
    (vector? vr) ; Bracketed
      (apply assemble-bracketed-value vr)
    (symbol? vr) ; No next word
      {:val (-> vr name keyword)}
    (number? vr) ; Literal number
      (cond
        (> vr 31) ; nextword
          {:val :NEXT_LIT :word vr}
        true
          {:val (+ 32 vr)})
    (keyword? vr) ; Label
      {:val :NEXT_LIT :label vr}))

(defn assemble-ir [ir]
  ;(println (meta ir))
  (cond
    (= 2 (count ir)) ;Extended op
      (let [[op a] ir]
        (with-meta
          (->Instruction :EXT
                         {:val (ext-op-encode (keyword op)) :ext_opcode (keyword op)}
                         (assemble_value a)) (meta ir))) 
    (= 3 (count ir)) ;Basic op
      (let [[op a b] ir]
        (with-meta
          (->Instruction (keyword op)
                       (assemble_value a)
                       (assemble_value b)) (meta ir)))))

(defn emit-val [vr]
  (let [mdata (if (:label vr) {:label_ref (:label vr)} {})]
    (if (or (:word vr) (:label vr))
      [(with-meta [(or (:word vr) 0)] mdata)] [])))

(defn emit-ir [ir]
  (concat
    [(with-meta 
            [(bit-or (op-encode (:opcode ir))
             (bit-shift-left (valtype-encode (:val (:a ir))) 4)
             (bit-shift-left (valtype-encode (:val (:b ir))) 10))] (meta ir))]
    (emit-val (:a ir)) (emit-val (:b ir))))

(defn label-map [base assembled]
  (reduce conj {} (filter (complement nil?)
          (map (fn [word addr]
                 (let [lbl (:label (meta word))]
                   (if lbl [lbl addr])))
               assembled (range base (+ base (count assembled)))))))

;; TODO combine this above
(defn label-map-exported [base assembled]
  (reduce conj {} (filter (complement nil?)
          (map (fn [word addr]
                 (let [lbl (:label (meta word))
                       xprt (:export (meta word))]
                   (if (and lbl xprt) [lbl addr])))
               assembled (range base (+ base (count assembled)))))))

;; Perhaps have this flag labels as resolved
;; so we can make multiple passes over as we compile
;; different units?
(defn label-fill [labelmap assembled]
  "Adds label offsets to label references in assembled output"
  (map (fn [word]
         (with-meta
           (let [ref (:label_ref (meta word))
                 offs (labelmap ref)
                 [num] word]
             (if offs [(bit-and 0xffff (+ num offs))] [num])) 
           (meta word))) assembled))

(defn pre-assemble [prog]
  (mapcat 
       (comp emit-ir assemble-ir) prog))

(defn assemble 
  ([prog base extmap]
    (let [prasm (pre-assemble prog)
          lmap  (merge extmap (label-map base prasm))]
      (with-meta
        (apply concat (label-fill lmap prasm))
        {:label-map (label-map-exported base prasm)})))
  ([prog base] (assemble prog base {}))
  ([prog] (assemble prog 0)))

(defn add-blob [name data addr appl]
  (merge-with merge appl
    {:label-map {name addr}}
    {:data-map {addr data}}))

(defn add-code [pgm addr appl]
  (let [asm (assemble pgm addr (:label-map appl))]
    (merge-with merge appl
      {:label-map (:label-map (meta asm))}
      {:data-map {addr asm}})))

(defn add-label [lbl addr appl]
  (merge-with merge appl
    {:label-map {lbl addr}}))

(defn save-app [filename appl]
  (let [dm (:data-map appl)
        last (apply max (keys dm))
        size (+ last (count (dm last)))
        bb (ByteBuffer/allocate (* 2 size))]
        (.createNewFile (File. filename))
        (.order bb ByteOrder/LITTLE_ENDIAN)
        (doseq [[addr dta] dm]
          (.position bb (* 2 addr))
          (doseq [val dta]
            (.putChar bb (char val))))
        (.rewind bb)
        (with-open [chl (.getChannel (FileOutputStream. (File. filename)))]
          (.write chl bb))))


