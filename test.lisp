(defun test1 ()
  (clrhash *user-functors*)
  (<- (member ?item (?item . ?rest)))
  (<- (member ?item (?x . ?rest)) (member ?item ?rest))
  (<- (nextto ?x ?y ?list) (iright ?x ?y ?list))
  (<- (nextto ?x ?y ?list) (iright ?y ?x ?list))
  (<- (iright ?left ?right (?left ?right . ?rest)))
  (<- (iright ?left ?right (?x . ?rest))
      (iright ?left ?right ?rest))
  (<- (zebra ?h ?w ?z)
      (= ?h ((house norwegian ? ? ? ?)
	     ?
	     (house ? ? ? milk ?) ? ?))
      (member (house englishman ? ? ? red) ?h)
      (member (house spaniard dog ? ? ?) ?h)
      (member (house ? ? ? coffee green) ?h)
      (member (house ukranian ? ? tea ?) ?h)
      (iright (house ? ? ? ? ivory)
	      (house ? ? ? ? green) ?h)
      (member (house ? snails winston ? ?) ?h)
      (member (house ? ? kools ? yellow) ?h)
      (nextto (house ? ? chesterfield ? ?)
	      (house ? fox ? ? ?) ?h)
      (nextto (house ? ? kools ? ?)
	      (house ? horse ? ? ?) ?h)
      (member (house ? ? luckystrike orange-juice ?) ?h)
      (member (house japanese ? parliaments ? ?) ?h)
      (nextto (house norwegian ? ? ? ?)
	      (house ? ? ? ? blue) ?h)
      (member (house ?w ? ? water ?) ?h)
      (member (house ?z zebra ? ? ?) ?h))
  (prog1
      (time (select (?houses ?water-drinker ?zebra-owner) 
		    (zebra ?houses ?water-drinker ?zebra-owner)))
    (clrhash *user-functors*)))    

(defun triple-test ()
  (clrhash *user-functors*)
  (insert 
   ("Kevin" "loves" "Dustie")
   ("Kevin" "loves" "Echo")
   ("Dustie" "loves" "Kevin")
   ("Echo" "loves" "cat nip")
   ("Cher" "loves" "Sonny")
   ("Sonny" "loves" "Cher"))
  (prog1
      (select (?x ?y) (q- ?x "loves" ?y))
    (clrhash *user-functors*)
    (clear-triple-store)))
