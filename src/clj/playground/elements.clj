(ns clojure-playground.elements
  (:require [clojure.string :as st]))

(def elements "Am Ar As At Au B Ba Be Bh Bi Bk Br C Ca Cd Ce Cf Cl Cm Co Cn Cr Cs Cu Db Ds Dy Er Es Eu F Fe Fm Fr Ga Gd Ge H He Hf Hg Ho Hs I In Ir K Kr La Li Lr Lu Md Mg Mn Mo Mt N Na Nb Nd Ne Ni No Np O Os P Pa Pb Pd Pm Po Pr Pt Pu Ra Rb Re Rf Rg Rh Rn Ru S Sb Sc Se Sg Si Sm Sn Sr Ta Tb Tc Te Th Ti Tl Tm U V W Xe Y Yb Zn Zr")


(defn spellel "Spell target using elements" [elements target]
  (let [els  (set (map st/lower-case (st/split elements  #"\s+")))
        target     (st/lower-case target) ; ==> "oseti"
        s1   (map str (seq target)) ; ==> (o s e t i)
        s2   (map str (drop-last 1 (seq target)) (drop 1 (seq target))); ==> (os se et ti)
        pairs (map list (concat (range (count s1)) (range (count s2))) (concat s1 s2))
               ; ==> ((0 o) (1 s) (2 e) (3 t) (4 i) (0 os) (1 se) (2 et) (3 ti))
        locs  (reduce #(assoc %1 %2 ()) (sorted-map) (range (count s1)))
               ; ==> {0 (), 1 (), 2 (), 3 (), 4 ()}
        locs  (reduce #(if (els (second %2))
                         (update-in %1 [(first %2)] concat (list (second %2)))
                         %1)
                      locs pairs) ; ==> {0 (o os), 1 (s se), 2 (), 3 (ti), 4 (i)}
        cands  (vals locs)
        ]
    ;; At this point, cands is a list of all elements that conceivably go at posn i.
    (letfn [(traverse [acc cands unused]
              (if (seq cands) 
                (some identity (for [el (filter unused (first cands))]
                                 (traverse (conj acc el)
                                           (drop (count el) cands)
                                           (disj unused el))))
                (reverse acc)))]
      (traverse () cands els))))
