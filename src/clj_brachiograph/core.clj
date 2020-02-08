(ns clj-brachiograph.core)

(defn get-position [a l]
  "returns the position based on the angle a (rad) and the length l"
  (let [x (* (Math/cos a) l)
        y (* (Math/sin a) l)]
    [x y]))

(defn q2 [a1 a2 x y]
  (Math/acos (/ (+ (* x x) (* y y) (* a1 a1 -1) (* a2 a2 -1)) (* 2 a1 a2))))

(defn q1 [a1 a2 x y q2]
    (- (Math/atan (/ y x)) (Math/atan (/ (* a2 (Math/sin q2)) (+ a1 (* a2 (Math/cos q2)))))))

(defn get-angles [a1 a2 x y]
  (let [q2 (q2 a1 a2 x y)
        q1 (q1 a1 a2 x y q2)]
    [q1 q2]))

(defn- rad2deg [a]
  (/ (* 180 a) Math/PI))


(defn display-angle [a1 a2 x y]
  (let [angles (get-angles a1 a2 x y)
        pos1 (get-position (first angles) a1)
        pos2 (get-position (apply + angles) a2)]
    (println "q1 is " (rad2deg (first angles)) ", q2 is " (rad2deg (second angles)))
    (println "position 1 computes to " a1 " * sin(" (first angles) ") = " pos1)
    (println "position 2 computes to " a2 " * sin(" (second angles) " + " (first angles) ") = " pos2)
    (println "position computes to " (+ (first pos1) (first pos2)) " / "(+ (second pos1) (second pos2)))))

(defn line [a1 a2 x1 y1 x2 y2 steps]
  (let [dx (/ (- x2 x1) steps)
        dy (/ (- y2 y1) steps)]
    (for [s (range (inc steps))]
      (let [nx (+ x1 (* s dx))
            ny (+ y1 (* s dy))
            angles (get-angles a1 a2 nx ny)]
        (display-angle a1 a2 nx ny)))))

