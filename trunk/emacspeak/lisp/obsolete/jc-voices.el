;;{{{  Settings from Janet Cahn's thesis.

;;; the  following are taken from Janet Cahn's Masters thesis.
;;; I've modified them for the Dectalk Express.
;;; lo is g5 on express.
;;; b4 and b5 are nuked.
;;; Also get rid of absolute changes in speech rate.

(dtk-define-voice 'paul-angry
                  "[:np :dv as 90 ap 95 bf 29 hr 13 pr 250 sr 90 br 0 la 0 lx 0 qu 58 ri 100 sm 0 gh 73 gf 74 gv 65]")

(dtk-define-voice 'paul-disgusted
 "[:np   :dv as 50 ap 120 bf 18 hr 18 pr 145 sr 26 br 0 la 0 lx 0   qu 0 ri 85 sm 18 gh 74 gf 75 gv 63 b4 261 b5 332 ]")

(dtk-define-voice 'paul-glad
 "[:np   :dv as 39 ap 105 bf 10 hr 5 pr 250 sr 73 br 0 qu 0 ri 56 sm 48 gh 49 gf 67 gv 63  ]")

(dtk-define-voice 'paul-sad
 "[:np   :dv as 30 ap 120 bf 14 hr 16 pr 50 sr 78 br 72 la 0 lx 100  qu 100 ri 7 sm 94 gh 35 gf 65 gv 62  ]")

(dtk-define-voice 'paul-scared
                  "[:np   :dv as 20 ap 300 bf 0 hr 100 pr 250 sr 100 br 0 la 0 lx 0   qu 100 ri 100 sm 0 gh 70 gf 70 gv 65  ]")

(dtk-define-voice 'paul-surprised
                  "[:np   :dv as 60 ap 120 bf 9 hr 5 pr 220 sr 66 br 0 la 0 lx 0   qu 70 ri 49 sm 54 gh 70 gf 70 gv 64  ]")

;;}}}
;;{{{  Settings from Janet Cahn's thesis.

;;; the  following are taken from Janet Cahn's Masters thesis.
;;; I originally  modified them for the Dectalk Express.
;;; and later cloned them for Outloud.
;;; lo is g5 on express.
;;; Also get rid of absolute changes in speech rate.

(outloud-define-voice 'paul-angry
		      " `v1 `vv100 `vb55 `vf250 `vr30 `vy25 ")

(outloud-define-voice
 'paul-disgusted
 " `v1 `vf0 `vv80 `vb40 `vr10 ")

(outloud-define-voice
 'paul-glad
 " `v1 `vf100 `vb60 `vv100 `vh40 `vr20 ")

(outloud-define-voice
 'paul-sad
 " `v1 `vf0 `vr0 `vv75 `vh55 `vb48 ")

(outloud-define-voice 'paul-scared
		      " `v1 `vf100 `vv100 `vb60 `vh40 `vy70 ")

(outloud-define-voice 'paul-surprised
		      " `v1 `vf100 `vv100 `vh30 `vb70 ")

;;}}}
