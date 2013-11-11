import AbstractCurry
import AbstractCurryGoodies
import ERD
import ERDGoodies
import GenerationHelper

-- "main"-function
generateViewsForEntity :: String -> [Entity] -> Entity -> [Relationship]
                       -> CurryProg
generateViewsForEntity erdname allEntities
                       (Entity ename attrlist) relationships =
 let noKeyAttrs  = filter (\a -> notKey a && notPKey a) attrlist
     noPKeyAttrs = filter notPKey attrlist
  in CurryProg
  (viewModuleName ename)
  ["WUI", "HTML", "Time", "Sort", "Spicey",
   erdname, erdname++"EntitiesToHtml"] -- imports
  [] -- typedecls
  -- functions
  [
   wuiSpec      erdname (Entity ename noKeyAttrs) relationships allEntities,
   tuple2Entity erdname (Entity ename noPKeyAttrs) relationships allEntities,
   entity2Tuple erdname (Entity ename noPKeyAttrs) relationships allEntities,
   wuiType      erdname (Entity ename noKeyAttrs) relationships allEntities,
   blankView    erdname (Entity ename noKeyAttrs) relationships allEntities,
   createView   erdname (Entity ename noKeyAttrs) relationships allEntities,
   editView     erdname (Entity ename noKeyAttrs) relationships allEntities,
   showView     erdname (Entity ename noKeyAttrs) relationships allEntities,
   leqEntity    erdname (Entity ename noKeyAttrs) relationships allEntities,
   listView     erdname (Entity ename noKeyAttrs) relationships allEntities
  ]  
  [] -- opdecls
  
  
type ViewGenerator = String -> Entity -> [Relationship] -> [Entity] -> CFuncDecl

wuiSpec :: ViewGenerator
wuiSpec erdname (Entity entityName attrlist) relationships allEntities =
  let
    manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
    manyToOneEntities  = manyToOne (Entity entityName attrlist) relationships
    argumentCount = length attrlist + length manyToOneEntities
                    + length manyToManyEntities
  in
    cmtfunc 
    ("The WUI specification for the entity type "++entityName++".\n"++
     if null (manyToOneEntities ++ manyToManyEntities)
     then ""
     else "It also includes fields for associated entities.")
    (viewModuleName entityName, "w"++entityName) 2 Public
    (foldr CFuncType
           (CTCons ("WUI", "WuiSpec")
               [entityInterface attrlist manyToOneEntities manyToManyEntities])
           (map (\e -> listType (ctvar e))
                (manyToOneEntities ++ manyToManyEntities))-- possible values
    )
    [
      CRule
      (map (\e -> CPVar (1, lowerFirst $ e ++ "List"))
           (manyToOneEntities ++ manyToManyEntities))
      [
        noGuard (
          applyF ("WUI", "withRendering") [         
            (if (argumentCount == 1) then
              head (attrWidgets attrlist)
            else
              applyF (combinator argumentCount) 
              (
                (attrWidgets attrlist) ++
                (map (\e -> applyF (wui "wSelect")
                              [constF (erdname, lowerFirst e++"ToShortView"),
                               CVar (1, lowerFirst $ e ++ "List")])
                     manyToOneEntities) ++
                (map (\e -> 
                  applyF (wui "wMultiCheckSelect")
                   [CLambda [CPVar (1, lowerFirst e)]
                      (list2ac [
                        applyF ("HTML", "htxt") [
                         applyF (erdname, lowerFirst e++"ToShortView")
                                [CVar (1, lowerFirst e)]
                         ]]),
                    CVar (1, lowerFirst $ e ++ "List")
                  ]) manyToManyEntities)
              )
            ),
            applyF ("Spicey", "renderLabels")
                   [constF (erdname++"EntitiesToHtml",
                            lowerFirst entityName++"LabelList")]
          ]
        )
      ]
      []
    ]
  where
    getFirstAttributeName myEntityName =
      gf (head (filter (\(Entity name _) -> name == myEntityName) allEntities))
    gf (Entity _ (_:((Attribute name _ _ _):_))) = name



tuple2Entity :: ViewGenerator
tuple2Entity erdname (Entity entityName attrlist) relationships allEntities =
  let
    manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
    manyToOneEntities = manyToOne (Entity entityName attrlist) relationships
  in
    cmtfunc 
    ("Transformation from data of a WUI form to entity type "++entityName++".")
    (viewModuleName entityName, "tuple2"++entityName) 2 Public
    (
     foldr CFuncType
      (if null manyToManyEntities
       then baseType (erdname, entityName)
       else tupleType ([ctvar entityName] ++
                       map (\name -> listType (ctvar name)) manyToManyEntities)
      )
      ([ctvar entityName] ++
       [entityInterface (filter notKey attrlist)
                        manyToOneEntities manyToManyEntities])
    )
    [
      CRule
      ( 
        [ CPVar (1, lowerFirst entityName ++ "ToUpdate"),
          tuplePattern
           (
            (map (\ ((Attribute name _ _ _), varId) ->
                       CPVar (varId,lowerFirst name)))
                 (zip (filter notKey attrlist) [1..]) ++
            (map (\ (name, varId) -> CPVar(varId,lowerFirst name))
                 (zip manyToOneEntities [1..])) ++
            (map (\ (name, varId) -> CPVar(varId,lowerFirst $ name++"s"))
                 (zip manyToManyEntities [1..]))
           )
        ]
      )
      [
        noGuard (
          tupleExpr
            ((foldr 
              (\(Attribute aname domain _ _) expr ->
                case domain of
                  (KeyDom rel) ->
                    applyF (erdname, "set"++entityName++aname) [
                      expr,
                      applyF (erdname, (lowerFirst rel)++"Key")
                             [CVar (1, lowerFirst rel)]
                    ]    
                  _ ->           
                    applyF (erdname, "set"++entityName++aname) [
                      expr, 
                      CVar (1, lowerFirst aname)
                    ]
              )
              (
                (CVar (0, lowerFirst $ entityName++"ToUpdate"))
              )
              attrlist
            ) : (map (\e -> cvar (lowerFirst $ e ++ "s")) manyToManyEntities))
        )
      ]
      []
    ]

entity2Tuple :: ViewGenerator
entity2Tuple erdname (Entity entityName attrlist) relationships allEntities =
  let
    manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
    manyToOneEntities = manyToOne (Entity entityName attrlist) relationships
  in
    cmtfunc
    ("Transformation from entity type "++entityName++" to a tuple\n"++
     "which can be used in WUI specifications.")
    (viewModuleName entityName, (lowerFirst entityName)++"2Tuple") 2 Public
    (
      foldr (CFuncType)
      (entityInterface (filter notKey attrlist)
                       manyToOneEntities manyToManyEntities)
      (
        (map ctvar manyToOneEntities) ++
        
        [(
          if null manyToManyEntities
          then baseType (erdname, entityName)
          else
            tupleType ([ctvar entityName] ++
                       map (\name -> listType (ctvar name)) manyToManyEntities)
        )]
      )
    )
    [
      CRule
      ( 
        (map (\ (name, varId) -> CPVar(varId,(lowerFirst name)))
             (zip manyToOneEntities [1..])) ++
        [
         tuplePattern
          (
            CPVar (1, lowerFirst entityName) :
            (map (\ (name, varId) -> CPVar(varId,(lowerFirst $ name++"s")))
                 (zip manyToManyEntities [1..]))
          )
        ]
      )
      [
        noGuard (
          tupleExpr
            (map (\ (Attribute a _ _ _) ->
                     applyF (erdname, (lowerFirst entityName)++a)
                            [cvar (lowerFirst entityName)])
                  (filter notKey attrlist) ++
             map (\e -> cvar (lowerFirst e)) manyToOneEntities ++
             map (\e -> cvar (lowerFirst $ e ++ "s")) manyToManyEntities)
        )
      ]
      []
    ]

wuiType :: ViewGenerator
wuiType _ (Entity entityName attrlist) relationships allEntities =
  let
    manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
    manyToOneEntities  = manyToOne (Entity entityName attrlist) relationships
  in
    cmtfunc 
    ("WUI Type for editing or creating "++entityName++" entities.\n"++
     "Includes fields for associated entities.")
    (viewModuleName entityName, "w"++entityName++"Type") 2 Public
    (
      foldr CFuncType
      (CTCons ("WUI", "WuiSpec") [
        if null manyToManyEntities
        then ctvar entityName
        else
          tupleType ([ctvar entityName] ++
                     map (\name -> listType (ctvar name)) manyToManyEntities)
      ])
      (
        [ctvar entityName] ++
        (map (\e -> ctvar e) (manyToOneEntities)) ++ -- related values
        --(map (\e -> listType (ctvar e)) manyToManyEntities) ++ -- related values
        (map (\e -> listType (ctvar e))
             (manyToOneEntities ++ manyToManyEntities))-- possible values
      )
    )
    [
      CRule
      (
        [CPVar (1, lowerFirst entityName)] ++
        (map (\e -> CPVar (1, lowerFirst e)) manyToOneEntities) ++ -- related values
        --(map (\e -> CPVar (1, lowerFirst $ e++"s")) (manyToManyEntities)) ++ -- related values
        (map (\e -> CPVar (1, lowerFirst $ e++"List"))
             (manyToOneEntities ++ manyToManyEntities))
      )
      [
        noGuard (
          applyF (wui "transformWSpec") [
            tupleExpr
            [
             applyF (viewModuleName entityName, "tuple2"++entityName)
                    [cvar (lowerFirst entityName)],
             applyF (viewModuleName entityName,lowerFirst entityName++"2Tuple")
                    (map (\e -> CVar (1, lowerFirst e)) (manyToOneEntities))
            ],
            applyF (viewModuleName entityName, "w"++entityName)
                   (map (\e -> CVar (1, lowerFirst $ e ++ "List"))
                        (manyToOneEntities ++ manyToManyEntities))
          ]
        )
      ]
      []
    ]


createView :: ViewGenerator
createView _ (Entity entityName attrlist) relationships allEntities =
  let
    manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
    manyToOneEntities = manyToOne (Entity entityName attrlist) relationships
  in
    viewFunction 
      ("Supplies a WUI form to create a new "++entityName++" entity.\n"++
       "Takes default values to be prefilled in the form fields.")
      entityName "create" 2
      ( -- function type
        (foldr CFuncType viewBlockType (
          (map attrType attrlist) ++
          (map ctvar manyToOneEntities) ++ -- defaults for n:1
          (map (\e -> listType (ctvar e)) manyToManyEntities) ++ -- defaults for n:m
          (map (\e -> listType (ctvar e)) (manyToOneEntities ++ manyToManyEntities)) ++ -- possible values
          [boolType ~>
           entityInterface attrlist manyToOneEntities manyToManyEntities
           ~> controllerType]))
      )
      [
        CRule
        ( -- params
          (map (\ ((Attribute name _ _ _), varId) ->
                        CPVar(varId,("default"++name)))
               (zip attrlist [1..])) ++
          (map (\ (name, varId) -> CPVar(varId,("default"++name)))
               (zip manyToOneEntities [1..])) ++
          (map (\ (name, varId) -> CPVar(varId,("default"++name++"s")))
               (zip manyToManyEntities [1..])) ++
          (map (\ (name, varId) -> CPVar(varId,("possible"++name++"s")))
               (zip (manyToOneEntities++manyToManyEntities) [1..])) ++
          [CPVar (100, "controller")]
        )
        [let initdata = (3,"initdata")
             wuiframe = (4,"wuiframe")
          in
           noGuard (
            (CLetDecl
              -- let
              [CLocalPat
                 (CPVar initdata)
                 (tupleExpr (
                    (map (\ ((Attribute name _ _ _), varId) ->
                                CVar(varId,("default"++name)))
                         (zip attrlist [1..])) ++
                    (map (\ (name, varId) -> CVar(varId,("default"++name)))
                         (zip manyToOneEntities [1..])) ++
                    (map (\ (name, varId) -> CVar(varId,"default"++name++"s"))
                         (zip manyToManyEntities [1..])))) [],
               CLocalPat
                 (CPVar wuiframe)
                 (applyF ("Spicey","wuiEditForm")
                       [string2ac ("Create new "++entityName),
                        string2ac "create",
                        CApply (CApply (CVar (1, "controller"))
                                       (constF (pre "False")))
                               (CVar initdata)]) [],
               CLocalPat (tuplePattern [CPVar (1,"hexp"), CPVar (2,"handler")])
              -- =
                (
                  applyF ("WUI", "wuiWithErrorForm") [
                    applyF (viewModuleName entityName, "w"++entityName) (
                      (map (\ (name, varId) ->
                                       CVar (varId,("possible"++name++"s")))
                           (zip (manyToOneEntities++manyToManyEntities) [1..]))
                    ),
                    CVar initdata,
                    applyF ("Spicey", "nextControllerForData")
                           [CApply (CVar(100, "controller"))
                                   (constF (pre "True"))],
                    applyF ("Spicey", "wuiFrameToForm") [CVar wuiframe]
                  ]
                ) []]
              -- in
              (applyV wuiframe [CVar (1, "hexp"), CVar (2, "handler")])
            )
          )
        ]
        []
      ]
      
editView :: ViewGenerator
editView erdname (Entity entityName attrlist) relationships allEntities =
  let
    manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
    manyToOneEntities  = manyToOne (Entity entityName attrlist) relationships
  in
    viewFunction 
      ("Supplies a WUI form to edit the given "++entityName++" entity.\n"++
       "Takes also associated entities and a list of possible associations\n"++
       "for every associated entity type.")
      entityName "edit" 2
      ( -- function type
        (foldr CFuncType viewBlockType (
          [tupleType ([baseType (erdname, entityName)] ++
                      map (\name -> listType (ctvar name)) manyToManyEntities)
          ] ++
          (map ctvar manyToOneEntities) ++ -- defaults for n:1
          (map (\e -> listType (ctvar e))
               (manyToOneEntities ++ manyToManyEntities)) ++ -- possible values
          [boolType ~> 
           tupleType ([baseType (erdname, entityName)] ++
                       map (\name -> listType (ctvar name)) manyToManyEntities)
           ~> controllerType]))
      )
      [
        CRule
        ( -- params
          [tuplePattern
             ([CPVar (1, lowerFirst entityName)] ++
              (map (\name -> CPVar (1, lowerFirst (name++"s")))
                   manyToManyEntities)
             )] ++
          (map (\ (name, varId) -> CPVar(varId,("related"++name)))
               (zip manyToOneEntities [2..])) ++
          (map (\ (name, varId) -> CPVar(varId,("possible"++name++"s")))
               (zip (manyToOneEntities++manyToManyEntities) [2..])) ++
          [CPVar (1, "controller")]
        )
        [let initdata = (3,"initdata")
             wuiframe = (4,"wuiframe")
          in
          noGuard (
            (CLetDecl
              -- let
              [CLocalPat
                  (CPVar initdata)
                  (tupleExpr ([CVar (1, lowerFirst entityName)] ++
                              map (\name -> CVar (1, lowerFirst (name++"s")))
                                  manyToManyEntities)) [],
               CLocalPat
                 (CPVar wuiframe)
                 (applyF ("Spicey","wuiEditForm")
                       [string2ac ("Edit "++entityName),
                        string2ac "change",
                        applyV (1, "controller")
                               [constF (pre "False"), CVar initdata]]) [],
               CLocalPat (tuplePattern [CPVar (1,"hexp"), CPVar (2,"handler")])
              -- =
                (applyF ("WUI", "wuiWithErrorForm") [
                    applyF (viewModuleName entityName, "w"++entityName++"Type") (
                      [cvar (lowerFirst entityName)] ++
                      --(map (\ (name, varId) -> CVar(varId,((lowerFirst name)++"s"))) (zip manyToManyEntities [2..])) ++
                      (map (\ (name, varId) -> CVar(varId,("related"++name)))
                           (zip manyToOneEntities [2..])) ++
                      (map (\ (name, varId) -> CVar(varId,("possible"++name++"s")))
                           (zip (manyToOneEntities++manyToManyEntities) [2..]))
                    ),
                    CVar initdata,
                    applyF ("Spicey", "nextControllerForData")
                           [CApply (CVar (1, "controller"))
                                   (constF (pre "True"))],
                    applyF ("Spicey", "wuiFrameToForm") [CVar wuiframe]
                  ]
                ) []]
              -- in
              (applyV wuiframe [CVar (1, "hexp"), CVar (2, "handler")])
            )
          )
        ]
        []
      ]
      
blankView :: ViewGenerator
blankView _ (Entity entityName attrlist) relationships allEntities =
  let
    manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
    manyToOneEntities  = manyToOne (Entity entityName attrlist) relationships
    withCTime          = hasCalendarTimeAttribute attrlist
  in
    viewFunction
      ("Supplies a WUI form to create a new "++entityName++" entity.\n"++
       "The fields of the entity have some default values.")
      entityName "blank" 2
      ( -- function type
        foldr CFuncType viewBlockType (
          (if withCTime then [baseType ("Time","CalendarTime")] else []) ++
          (map (\e -> listType (ctvar e))
               (manyToOneEntities ++ manyToManyEntities)) ++ -- possible values
          [boolType ~>
           entityInterface attrlist manyToOneEntities manyToManyEntities
           ~> controllerType])
      )
      [
        CRule
        ( -- params
          (if withCTime then [CPVar (0,"ctime")] else []) ++
          (map (\ (name, varId) -> CPVar(varId,("possible"++name++"s")))
               (zip (manyToOneEntities++manyToManyEntities) [2..])) ++
          [CPVar (1, "controller")]
        )
        [
          noGuard (
              applyF (viewFunctionName entityName "create") 
              (
                (attrDefaultValues (Just (CVar (0,"ctime"))) attrlist) ++
                (map (\ (name, varId) ->
                           applyF (pre "head")
                                  [CVar (varId,("possible"++name++"s"))])
                     (zip manyToOneEntities [2..])) ++
                (map (\_ -> list2ac []) (zip manyToManyEntities [2..])) ++
                (map (\ (name, varId) -> CVar (varId,("possible"++name++"s")))
                     (zip (manyToOneEntities++manyToManyEntities) [2..])) ++
                [CVar (1, "controller")]
              )
          )
        ]
        []
      ]

-- Generate function to compare to entities in lexicographic order.
leqEntity :: ViewGenerator
leqEntity erdname (Entity entityName attrlist) _ _ =
  cmtfunc
    ("Compares two "++entityName++" entities. This order is used in the list view.")
    (viewModuleName entityName, "leq" ++ entityName) 2 Private
    -- function type
    (baseType (erdname,entityName) ~> baseType (erdname,entityName) ~>boolType)
    [let ename = lowerFirst entityName
         e1 = (1,"x1")
         e2 = (2,"x2")
      in CRule [CPVar e1,CPVar e2]
               [noGuard (
                  applyF (pre "<=")
                   [tupleExpr (map (\ (Attribute a _ _ _) ->
                                       applyF (erdname,ename++a) [CVar e1])
                                   (filter notKey attrlist)),
                    tupleExpr (map (\ (Attribute a _ _ _) ->
                                       applyF (erdname,ename++a) [CVar e2])
                                   (filter notKey attrlist))
                   ])
               ]
               []
    ]


-- generate view for showing entities
showView :: ViewGenerator
showView erdname (Entity entityName attrlist) relationships allEntities =
 let manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
     manyToOneEntities  = manyToOne (Entity entityName attrlist) relationships
     evar    = (1, lowerFirst entityName)
     ctrlvar = (2, "controller")
  in viewFunction 
      ("Supplies a view to show the details of a "++entityName++".\n")
      entityName "show" 2
      -- function type
      (foldr CFuncType viewBlockType (
          [baseType (erdname,entityName)] ++
          (map ctvar manyToOneEntities) ++ -- defaults for n:1
          (map (\name -> listType (ctvar name)) manyToManyEntities) ++
          [controllerType])
      )
      [CRule
        ( -- parameters
          [CPVar evar] ++
          (map (\ (name, varId) -> CPVar (varId,"related"++name))
               (zip manyToOneEntities [3..])) ++
          (map (\ (name, varId) -> CPVar (varId, lowerFirst name ++ "s"))
               (zip manyToManyEntities [(length manyToOneEntities + 3)..])) ++
          [CPVar ctrlvar]
        )
        [
          noGuard (
            applyF (pre "++")
              [applyF (erdname++"EntitiesToHtml",
                       lowerFirst entityName++"ToDetailsView")
                  ([CVar evar] ++
                   map (\ (name, varId) -> CVar (varId,"related"++name))
                       (zip manyToOneEntities [3..]) ++
                   map (\ (name, varId) -> CVar (varId, lowerFirst name++"s"))
                       (zip manyToManyEntities
                            [(length manyToOneEntities + 3)..])
                  ),
               list2ac [applyF ("Spicey","spButton")
                          [string2ac ("back to "++entityName++" list"),
                           applyF ("Spicey", "nextController")
                                  [CVar ctrlvar]]]
              ]
            )
         ]
         []
       ]
      

listView :: ViewGenerator
listView erdname (Entity entityName attrlist) _ _ =
 let showctrlvar = (2, snd (controllerFunctionName entityName "show"))
     editctrlvar = (3, snd (controllerFunctionName entityName "edit"))
     deltctrlvar = (4, snd (controllerFunctionName entityName "delete"))
  in
    viewFunction 
      ("Supplies a list view for a given list of "++entityName++" entities.\n"++
       "Shows also buttons to show, delete, or edit entries.\n"++
       "The arguments are the list of "++entityName++" entities\n"++
       "and the controller functions to show, delete and edit entities.\n")
      entityName "list" 4
      -- function type
      (foldr CFuncType viewBlockType
             [listType (baseType (erdname,entityName)),
              baseType (erdname,entityName) ~> controllerType,
              baseType (erdname,entityName) ~> controllerType,
              baseType (erdname,entityName) ~> boolType ~> controllerType
             ]
      ) -- type
      [
        CRule
        ( -- params
          [
            CPVar (1, lowerFirst entityName ++ "s"),
            CPVar showctrlvar,
            CPVar editctrlvar,
            CPVar deltctrlvar
          ]
        )
        [
          noGuard (            
            applyF (pre ":") [
              applyF ("HTML", "h1")
                     [list2ac [applyF ("HTML", "htxt")
                                      [string2ac $ entityName ++ " list"]]],
              list2ac [
                applyF ("Spicey", "spTable") [
                  applyF (pre "++") [
                    list2ac [
                      applyF (pre "take") [
                        CLit (CIntc (length attrlist)),
                        constF (erdname++"EntitiesToHtml",
                                lowerFirst entityName++"LabelList")
                      ]
                    ],
                    applyF (pre "map") [
                      constF (viewModuleName entityName,"list"++entityName),
                      applyF ("Sort","mergeSort") [
                          constF (viewModuleName entityName,"leq"++entityName),
                          cvar (lowerFirst $ entityName ++ "s")
                      ]
                    ]
                  ]
                ]
              ]
            ]

          )
        ]
        [
          CLocalFunc (cfunc
            (viewModuleName entityName, "list"++entityName) 2 Private
            (ctvar entityName ~> listType viewBlockType)
            [
             CRule
              [
               CPVar (1, lowerFirst entityName)
              ]
              [noGuard (
                applyF (pre "++") [
                  applyF (erdname++"EntitiesToHtml",
                          lowerFirst entityName++"ToListView")
                         [cvar $ lowerFirst entityName],
                  list2ac [list2ac [
                    applyF ("Spicey", "spSmallButton")
                      [string2ac "show",
                       applyF ("Spicey", "nextController")
                         [applyV showctrlvar
                                 [cvar $ lowerFirst entityName]]],
                    applyF ("Spicey", "spSmallButton")
                      [string2ac "edit",
                       applyF ("Spicey", "nextController")
                         [CApply (CVar editctrlvar)
                                 (cvar $ lowerFirst entityName)]],
                    applyF ("Spicey", "spSmallButton")
                      [string2ac "delete",
                       applyF ("Spicey", "confirmNextController")
                         [applyF ("HTML","h3")
                            [list2ac
                              [applyF ("HTML","htxt")
                                [applyF (pre "concat")
                                  [list2ac
                                    [string2ac "Really delete entity \"",
                                     applyF (erdname++"EntitiesToHtml",
                                          lowerFirst entityName++"ToShortView")
                                            [cvar $ lowerFirst entityName],
                                     string2ac "\"?"]]
                                ]]],
                          CApply (CVar deltctrlvar)
                                 (cvar $ lowerFirst entityName)]]
                  ]]
                ]
                )
              ]
              []
            ])
        ]
      ]      
      
-- aux

-- entityName: Name of entity the view should be generated for
-- viewType: the function of the generated View, e.g. "new", "edit", "list"
-- arity
-- functionType: the type of the view function
-- rules: the rules defining the view
viewFunction description entityName viewType arity functionType rules =
  cmtfunc description (viewFunctionName entityName viewType) arity
          Public functionType rules
  
entityInterface :: [Attribute] -> [String] -> [String] -> CTypeExpr
entityInterface attrlist manyToOne manyToMany = 
  tupleType (map attrType attrlist ++
             map ctvar manyToOne ++
             map (\e -> listType (ctvar e)) manyToMany)
