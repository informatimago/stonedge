(pushnew #P"~/works/stonedge/lisp/" ql:*local-project-directories*
         :test (function equal))
(ql:quickload '(:com.informatimago.games.stonedge.generator
                :com.informatimago.games.stonedge.render
                :com.informatimago.games.stonedge.level-manager))

#-(and)
(setf ql:*local-project-directories* (remove-duplicates ql:*local-project-directories*
                                                        :test (function equal)))
