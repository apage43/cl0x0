(ns cloxo.testapp
  (use [cloxo.instr :only (add-label add-blob add-code save-app)]))

(def dprintstr
  ;; We only add to the global namespace when we need to
  '[^{:label :dprintstr} ^:export
    (SET B SP)
    (SET SP A)
    ^{:label :loop}
    (SET [+ :screen I] POP)
    (ADD I 1)
    (IFN PEEK 0)
      (SET PC :loop)
    (SET SP B)
    (SET PC POP)])

(def appmain
  '[(SET A :hello)
    (JSR :dprintstr)
    (SET A :thebest)
    (JSR :dprintstr)
    (SUB PC 1)])

; Shorthand for compose/partial
(def & comp)
(def p partial)

(def mkapp
  (& (p add-code appmain 0x0)
     (p add-code dprintstr 0x10)
     (p add-label :screen 0x8000)
     (p add-blob :hello (seq "Hello World! \0") 0x20)
     (p add-blob :thebest (seq "You're the best!\0") 0x30)))
  
(save-app "/Users/apage43/out.dcpu16" (mkapp {}))
