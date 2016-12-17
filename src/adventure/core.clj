(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.set :as set])
  (:gen-class))

(def the-map
  {
   :entrance
    {
      :title "at the entrance"
      :desc "You managed to escape the mansion. Ahead you see a road. You can also go back. "
      :dir { :road :road :back :down}
      :check { :road "The road is made of cobblestone. You wonder where it leads."}
    }
   :road {
           :title "traveling down the road"
           :desc "You notice a park in the distance. "
           :dir { :park :park :back :entrance}
       }

   :park
    {
      :title "in the park"
      :desc "After arriving at the park, you decide to sit down and relax. "


    }

   :yellow {
   :torn 0
   :firstdesc "You don't remember how you got here. "
   :desc "The walls are covered with yellow wallpaper. There is a door. "
           :title "in the yellow room"
           :dir {
                  :door :corridor :corridor :corridor
                  }
           :check {
             :wallpaper "It seems like something is hidden under the wallpaper."

             :door "This is an old wooden door."}
           :contents #{}}
   :blue {
           :desc "This room is empty, apart from a stick lying on the floor. "
           :title "in the blue room"
           :contents #{:stick}
           :dir {
                  :door :corridor :corridor :corridor
            }
           :check {
                :stick "Just a stick. Perhaps you could use it."
            }
   }
   :black {
          :desc "This room contains a fireplace. There is a piece of cloth lying on the ground. "
          :title "in the black room"
          :check
            { :cloth "It's just a piece of cloth."
            }
          :contents #{:cloth}
          :dir {
                  :door :corridor :corridor :corridor
            }
    }
   :corridor {
    :desc "There are multiple doors leading to different rooms: yellow, red, blue and black. There are stairs leading downstairs. "
    :title "in the corridor"
     :dir {:yellow :yellow
           :stairs :down :down :down
           :red :red
           :blue :blue
           :black :black
           }
     :check {:stairs "The stairs lead into darkness. You would prefer not to go before you find a light source."
             :yellow "This door leads to the room you woke up in."
             :red "This door is stained red. You decided not to think about it too much."
             :blue "This door looks brand new."
             :black "This metal door has a lock on it. You can't enter without a key."
             }
     :contents #{}
            }
   :red {
          :title "in the red room"
          :desc "This room is filled with a horrible stench. In the middle there is a table. A door leads back to the corridor. "
          :check {
           :table "On top of the table is a butcher knife."
          }
          :dir {
                  :door :corridor :corridor :corridor
                 }
          :contents #{:knife}
        }
   :down {
           :firstdesc "You can't see anything. "
           :title "downstairs"
           :desc "The only thing you can see is some figure in the shadows. You can't quite make it out, but it looks like a large dog. "
           :check {
                  :stairs "The stairs lead back up."
                  :figure "Suddenly the beast begins rapidly approaching you! It appears to be a grue!"
                  :exit "In order to open the exit you need to enter the 6-digit password. (type go <password>)"
            }
           :dir {
                  :up :corridor :stairs :corridor
                  :1 :1 :2 :2 :3 :3 :4 :4 :5 :5
                  :6 :6
                  :562935 :entrance
                  }
           :grue :exists
         }
    :1 {
         :title "in the first room"
         :desc "This room is empty, apart from some numbers written on the wall: 35. "
         :check {
              :numbers "The numbers are 35."
          }
         :dir {
              :door :down
              :back :down
          }
    }
    :2 {
         :title "in the second room"
         :desc "This room is empty, apart from some numbers written on the wall: 24. "
         :check {
              :numbers "The numbers are 24."
          }
         :dir {
              :door :down
              :back :down
          }
    }
    :3 {
         :title "in the third room"
         :desc "This room has a bag on the floor. Some numbers written on the wall: 56. "
         :check {
              :numbers "The numbers are 56."
              :bag "Just a bag. Who knows what's inside?"
          }
         :dir {
              :door :down
              :back :down
          }
         :contents #{:bag}
    }
    :4 {
         :title "in the fourth room"
         :desc "This room is empty, apart from some numbers written on the wall: 73. "
         :check {
              :numbers "The numbers are 73."
          }
         :dir {
              :door :down
              :back :down
          }
    }
    :5 {
         :title "in the fifth room"
         :desc "This room is empty, apart from some numbers written on the wall: 29. "
         :check {
              :numbers "The numbers are 29."
          }
         :dir {
              :door :down
              :back :down
          }
    }
    :6 {
         :title "in the sixth room"
         :desc "This room is empty, but written on the wall is: 3, then 5, then 1. "
         :check {
              :wall "\"3, then 5, then 1\""
          }
         :dir {
              :door :down
              :back :down
          }
    }

   }
)

(def adventurer
  {:location :yellow
   :hp 5
   :inventory #{}
   :tick 0
   :seen #{}
   :combat :none
   :lit 0
   }
  )

(defn get-inv [player]
  (if (empty? (player :inventory))
    "Nothing in your inventory."
    (clojure.string/join ", " (mapv name (player :inventory)) )
  )
)

(defn get-st [player]
  (match (player :hp)
        5 "You feel fine."
        4 "You have a scratch."
        3 "You are wounded."
        2 "You are severely injured."
        1 "You are heavily bleeding."
        _ ""
  )
 )


(defn tock [player]
  (update-in player [:tick] inc)
  )

(defn do-combat [player]
  (if (= :none (player :combat))
    player

 (do (println (str "You are attacked by the " (name (player :combat))"! ") (get-st (update-in player [:hp] #(- % 1))))
   (update-in player [:hp] #(- % 1))
   )

    )
)

(defn status [player]
  (let [location (player :location)]


    (if (= :none (player :combat))
      (print (str "You are " (-> the-map location :title) ". ")))

    (if-not ((player :seen) location)
      (if (nil? (-> the-map location :firstdesc))
      (print (-> the-map location :desc))
      (print (-> the-map location :firstdesc))

      )
    )
    (do-combat (tock (update-in player [:seen] #(conj % location)))) )

)

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+"))
)

(defn go [dir cur-map player]
  (let [location (player :location)
        dest (->> cur-map location :dir dir)
        line (->> cur-map location :check dir)]

    (if-not (= :none (player :combat))
      (do (println "You can't go anywhere when in combat!") player)


    (if (nil? dest)
      (if (nil? line)
      (do (println "You can't go that way.")
          player)
        (do (println line) player)
        )

      (if (and (= dest :black) (nil? ((player :inventory) :key)))
        (do (println "There is a lock, you can't enter without a key.") player)
      (assoc-in player [:location] dest)
      )
      )
      )
    )

)

(defn check [this cur-map player]
   (let [location (player :location)
             line (->> cur-map location :check this)]
    (do


    (if (nil? line)
      (println "Nothing interesting.")
      (println line)
    )

      (if (and (= location :down) (= this :figure) (= :exists ((cur-map location) :grue)))
     (assoc-in player [:combat] :grue)
        player
      )
    )
     )

)

(defn pickup [cur-map player that]
  (let [location (player :location)]
  (if (nil? (->> cur-map location :contents that))
    (do (println "Can't take that.") player)
  (do
      (println (str "Picked up " (name that) "."))
      (update-in player [:inventory] #(conj % that))
    )
  )
  )
)


 (defn attack [cur-map player]
   (if (= :none (player :combat))
     (do (println "You are not in combat.") player)
     (do (println (str "You hit the " (name (player :combat)) " with your fist. It doesn't seem to notice.")) player)
   )
  )

 (defn use-item-pl [item target cur-map player]
   (println (str "Used " (name item) "."))
   (if (and (= item :knife) (= target :wallpaper) (= (player :location) :yellow))
     (do (println "You cut the wallpaper with the knife. A key falls out.") player)

       (if (and (= item :knife) (= target :grue) (= :grue (player :combat)))
         (do (println "You strike the beast with the knife. It whimpers and runs away into the darkness.") (assoc-in player [:combat] :none))


          (if (and (= item :torch) (= target :fireplace) (= (player :location) :black))
          (do (println "You light the torch from the fireplace.") (assoc-in player [:lit] 1))

           player
         )

         )
     )
)

 (defn use-item-map [item target cur-map player]


   (if (and (= item :knife) (= target :wallpaper) (= (player :location) :yellow) (= ((cur-map :yellow) :torn) 0))
    (assoc-in
     (update-in cur-map [:yellow] assoc
                :desc "The walls are covered with ripped yellow wallpaper. A key lies on the floor. There is a door. "

                :contents #{:key}
                :torn 0)
      [:yellow :check :wallpaper] "The wallpaper has been torn apart. ")


  (if (and (= item :knife) (= target :grue) (= (player :combat) :grue))
        (assoc-in
          (update-in cur-map [:down] assoc
                    :desc "Just darkness. "

                    :grue :none
                    )
          [:down :check :figure] nil)

  (if (and (= item :torch) (= target :fireplace) (= (player :location) :black))
    cur-map


   (do (println "Nothing happens.") cur-map  )
    )) )
 )


(defn get-commands []
  (str
       "List of commands you can use:\n"
       "look\n"
       "go <somewhere>\n"
       "check <something>\n"
       "take <something>\n"
       "attack - only usable in combat. this is only for attacking with your fists\n"
       "inventory - check inventory\n"
       "status - your current health status\n"
       "use <item> <target> - for using items in your inventory, including attacking with them\n"
       "hint - receive a clue about what you should do next\n"
       "combine <item1> <item2> - combine 2 items to make another item\n"
       "open <item> - this is not for doors, it's for stuff like bags\n"
       "...I ran out of ideas for commands at this point. 15 is a bit too many...\n"
       "run <somewhere> - same as go, but you arrive faster. allegedly.\n"
       "sleep - maybe you're tired.\n"
       "jump - sure, why not\n"
       "panic\n"
  )
)

(defn hint [cur-map player]

  (if (nil? ((player :inventory) :key))
  "It seems that there might be something hidden under the wallpaper in the yellow room.\nPerhaps if you had something to cut it with you could check?"
  (if (nil? ((player :inventory) :torch))
  "You are curious what you could find downstairs. If only you had something like a torch to light the way..."
  (if (nil? ((player :lit) :torch))
  "You have a torch, but for it to be useful you need to find a way to light it."
  "Using your torch you can navigate the darkness downstairs. Maybe you can find out the password to the exit."

  )

  )

  )
)

(defn modpl [player cur-map command]




  (match command
         [:help] (do (println (get-commands)) player)

         [:look] (do (let [location (player :location)] (println (-> cur-map location :desc)) player))

         [:go there] (go there cur-map player)

         [:run there] (go there cur-map player)

         [:check that] (check that cur-map player)


         [:take that] (pickup cur-map player that)

         [:hint] (do (println(hint cur-map player)) player)

         [:attack _] (attack cur-map player)
         [:attack] (attack cur-map player)


         [:inventory] (do (println
                      (get-inv player))
               player)
         [:status] (do (println
                      (get-st player))
               player)

         [:use item target] (if-not ((player :inventory) item) (do (println "You don't have that.") player)
                              (use-item-pl item target cur-map player))

         [:debug] (do (println (str player cur-map)) player)

         [:panic] (let [new-pl (update-in player [:hp] #(- % 1))]
                    (do
                      (println (str "You panic! In your panicked state, you accidentally hit youself. " (get-st new-pl)))
                      new-pl)
                    )

         [:sleep] (do (println
                      (if (= :none (player :combat))
                        "You take a nap. Then you wake up."
                        "You can't go to sleep! You're in combat!"
                        ))
               player)

         [:jump] (do (println
                      "You jump. What did you expect that to accomplish?")
               player)

         [:combine a b] (if (and (or (and (= a :stick) (= b :cloth)) (and (= b :stick) (= a :cloth)) ) ((player :inventory) :stick) ((player :inventory) :cloth))
                           (do (println "You have successfully made a torch!") (update-in player [:inventory] #(conj % :torch)))
                           player
                           )
         [:open this] (if (and ((player :inventory) this) (= this :bag))
                        (do (println "Inside the bag, you find a rock, a marble, a string, and some seeds. You wonder why so many useless items were put in this bag.")

                         (update-in player [:inventory] #(set/union % #{:marble :rock :string :seeds}))
                         )
                        (do (println "Nothing to open.") player))



         _ (do (println "I don't understand you.")
               player)

         )


  )

(defn modmap [player cur-map command]

  (match command

        [:go there] (if (and (= there :down) (= (player :lit) 1) ((player :inventory) :torch))
                     (assoc-in cur-map [:down :desc] "The torch allows you to see: there are 6 doors, labeled 1 through 6, along the wall. There is also an exit.")
                      cur-map
                      )

        [:check that] (if (and (= (player :location) :down) (= that :figure))
                    (assoc-in cur-map [:down :desc] "The grue is right in front of you!")
                      cur-map
                    )

        [:take that] (if (and (= (player :location) :red) (= that :knife))
                      (assoc-in cur-map [:red :check :table] "There is nothing on the table.")
                      (if (and (= (player :location) :yellow) (= that :key))
                      (assoc-in cur-map [:yellow :desc] "The walls are covered with ripped yellow wallpaper. There is a door. ")
                        (if (and (= (player :location) :blue) (= that :stick))
                          (assoc-in
                          (update-in cur-map [:blue] assoc
                                   :desc "This room is empty. "

                                     ) [:blue :check :stick] nil)

                          (if (and (= (player :location) :3) (= that :bag))
                            (assoc-in
                          (update-in cur-map [:3] assoc
                                     :desc "This room is empty, apart from some numbers written on the wall: 56. "
                                       ) [:3 :check :bag] nil)
                            (if (and (= (player :location) :black) (= that :cloth))
                              (assoc-in
                            (update-in cur-map [:black] assoc
                                       :desc "This room contains a fireplace. "
                                         ) [:black :check :cloth] nil)
                              cur-map
                            )
                          )
                        )
                      )
                    )

        [:use item target] (if-not ((player :inventory) item) cur-map
                              (use-item-map item target cur-map player))


         _ cur-map
  )
)


(defn -main
  [& args]

  (println "Welcome to the game! Use \"help\" to get a list of all the commands.")

  (loop [local-map the-map
         local-player adventurer]

    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (to-keywords (read-line))
          new-map (do (println "-----" )(modmap pl local-map command))
          new-pl (modpl pl local-map command)
          ]
      (if (<= (new-pl :hp) 0)

      (println "You have died. Game Over.")
      (if (= (new-pl :location) :park)
        (println (str "You sit down on a bench and relax.\nCongratulations! You have completed this game. It took you " (new-pl :tick) " commands."))

       (recur new-map new-pl)
      )
      )

      )
    )
  )
