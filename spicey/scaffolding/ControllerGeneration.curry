import AbstractCurry
import AbstractCurryGoodies
import GenerationHelper
import ERD
import ERDGoodies
import Char(toLower)

-- Name of generic authorization module:
authModName = "Authorization"
-- Name of entity-specific authorization module:
enauthModName = "AuthorizedControllers"
-- Name of module defining the default controller:
defCtrlModName = "DefaultController"

-- "main"-function
generateControllersForEntity :: String -> [Entity] -> Entity -> [Relationship]
                             -> CurryProg
generateControllersForEntity erdname allEntities
                             (Entity ename attrlist) relationships =
 CurryProg
  (controllerModuleName ename)
  -- imports:
  ["Spicey", "KeyDatabase", "HTML", "Time", erdname, viewModuleName ename,
   "Maybe", "SessionInfo", authModName, enauthModName, "UserProcesses",
   erdname++"EntitiesToHtml"]
  [] -- typedecls
  -- functions
  (
    [
     -- controller for dispatching to various controllers:
     mainController erdname (Entity ename attrlist) relationships allEntities,
     -- controller for providing a page to enter new entity data:
     newController erdname (Entity ename attrlist) relationships allEntities,
     -- controller for saving data in new entity:
     createController erdname (Entity ename attrlist) relationships allEntities,
     -- controller to show an existing record in a form to edit
     editController erdname (Entity ename attrlist) relationships allEntities,
     -- controller to update a record with the given data
     updateController erdname (Entity ename attrlist) relationships allEntities,
     -- controllers to delete an entity with the given data
     confirmDeleteController erdname (Entity ename attrlist)
                             relationships allEntities,
     deleteController erdname (Entity ename attrlist) relationships allEntities,
     -- contgroller to list of all entities:
     listController erdname (Entity ename attrlist) relationships allEntities,
     -- controller to show entites:
     showController erdname (Entity ename attrlist) relationships allEntities
   ] ++ 
    (manyToManyAddOrRemove erdname (Entity ename attrlist) (manyToMany allEntities (Entity ename attrlist)) allEntities) ++
    --(getAll erdname (Entity ename attrlist) (manyToOne (Entity ename attrlist) relationships) allEntities) ++
    --(getAll erdname (Entity ename attrlist) (manyToMany allEntities (Entity ename attrlist)) allEntities) ++
    --(manyToManyGetRelated erdname (Entity ename attrlist) (manyToMany allEntities (Entity ename attrlist)) allEntities) ++
    (manyToOneGetRelated erdname (Entity ename attrlist) (manyToOne (Entity ename attrlist) relationships) allEntities relationships)
  )
  [] -- opdecls


-- erdname: name of the entity-relationship-specification
-- entity: the entity to generate a controller for
type ControllerGenerator = String -> Entity -> [Relationship] -> [Entity] -> CFuncDecl

-- Generates the main controller that dispatches to the various
-- subcontrollers according to the URL parameters.
mainController :: ControllerGenerator
mainController erdname (Entity entityName _) _ _ =
  controllerFunction 
  ("Choose the controller for a "++entityName++
   " entity according to the URL parameter.")
  entityName "main" 0
    controllerType -- function type
    [ -- rules
    CRule
      [] -- no arguments
      [noGuard (
        CDoExpr
         [CSPat (CPVar (1,"args"))
                (constF ("Spicey","getControllerParams")),
          CSExpr
           (CCase (CVar (1,"args"))
            ([CBranch (listPattern [])
                      (constF (controllerFunctionName entityName "list")),
              CBranch (listPattern [stringPattern "list"])
                      (constF (controllerFunctionName entityName "list")),
              CBranch (listPattern [stringPattern "new"])
                      (constF (controllerFunctionName entityName "new")),
              CBranch (listPattern [stringPattern "show", CPVar (2,"s")])
                (applyF ("Spicey","applyControllerOn")
                  [applyF (erdname,"read"++entityName++"Key") [CVar (2,"s")],
                   constF (erdname,"get"++entityName),
                   constF (controllerFunctionName entityName "show")]),
              CBranch (listPattern [stringPattern "edit", CPVar (2,"s")])
                (applyF ("Spicey","applyControllerOn")
                  [applyF (erdname,"read"++entityName++"Key") [CVar (2,"s")],
                   constF (erdname,"get"++entityName),
                   constF (controllerFunctionName entityName "edit")]),
              CBranch (listPattern [stringPattern "delete", CPVar (2,"s")])
                (applyF ("Spicey","applyControllerOn")
                  [applyF (erdname,"read"++entityName++"Key") [CVar (2,"s")],
                   constF (erdname,"get"++entityName),
                   constF (controllerFunctionName entityName "confirmDelete")]),
              CBranch (CPVar (3,"_"))
                 (applyF ("Spicey", "displayError")
                         [string2ac "Illegal URL"])])
          )
         ]
      )]
      [] -- where clauses
    ]

-- generates a controller to show a form to create a new entity
-- the input is then passed to the create controller
-- only has to call the blank entry form and pass the create controller
newController :: ControllerGenerator
newController erdname (Entity entityName attrList) relationships allEntities =
  let
    manyToManyEntities = manyToMany allEntities (Entity entityName attrList)
    manyToOneEntities  = manyToOne (Entity entityName attrList) relationships
    withCTime          = hasCalendarTimeAttribute attrList
    infovar            = (0, "sinfo")
    ctimevar           = (1,"ctime")
  in
    controllerFunction 
    ("Shows a form to create a new "++entityName++" entity.")
    entityName "new" 0
      controllerType -- function type
      [ -- rules
      CRule
        [] -- no arguments
        [
          noGuard (
           applyF (pre "$")
            [applyF (authModName,"checkAuthorization")
              [applyF (enauthModName,lowerFirst entityName++"OperationAllowed")
                [constF (authModName,"NewEntity")]],
             CDoExpr (
              (CSPat (CPVar infovar) getUserSessionInfoFunc :
               map 
                (\ (ename, num) ->
                   CSPat (CPVar (num,"all"++ename++"s")) 
                         (applyF (db "runQ")
                                 [constF (erdname,"queryAll"++ename++"s")])
                )
                (zip (manyToOneEntities ++ manyToManyEntities) [2..])
              ) ++
              (if withCTime
               then [CSPat (CPVar ctimevar)
                           (constF ("Time","getLocalTime"))]
               else []) ++
              [         
                CSExpr (
                  applyF (pre "return")
                   [applyF (viewFunctionName entityName "blank")
                     ([CVar infovar] ++
                      (if withCTime then [CVar ctimevar] else []) ++
                      map (\ (ename, num) -> CVar (num, "all"++ename++"s"))
                           (zip (manyToOneEntities ++ manyToManyEntities)
                                [2..]) ++
                      [constF (controllerFunctionName entityName "create")])
                  ]
                )
              ]
            )
           ]
          )
        ]
        [] -- where clauses
      ]

createController :: ControllerGenerator
createController erdname (Entity entityName attrList) relationships allEntities =
  let
    noPKeys            = (filter notPKey attrList)
--    foreignKeys = (filter isForeignKey attrList)
    notGeneratedAttributes = filter (\attr -> (not (isForeignKey attr))
                                              && (notPKey attr))     attrList
    parameterList      = map (\(Attribute name _ _ _) -> lowerFirst name)
                             (filter (not . isForeignKey) noPKeys)
    manyToManyEntities = manyToMany allEntities (Entity entityName attrList)
    manyToOneEntities  = manyToOne (Entity entityName attrList) relationships
  in
    controllerFunction
    ("Persists a new "++entityName++" entity to the database.")
    entityName "create" 2
      (boolType ~>
       tupleType (map attrType notGeneratedAttributes ++
                  map ctvar manyToOneEntities ++
                  map (listType . ctvar) manyToManyEntities)
        ~> controllerType
      )
      [CRule [CPComb (pre "False") [], CPVar (0,"_")]
             [noGuard (callEntityListController entityName)]
             [],
       CRule 
        [CPComb (pre "True") [],
         tuplePattern
          (map (\ (param, varId) -> CPVar (varId, param)) 
               (zip (parameterList ++ map lowerFirst manyToOneEntities ++
                     map (\e -> (lowerFirst e) ++ "s") manyToManyEntities)
                     [1..]))
        ] -- parameterlist for controller
        [
          noGuard (
            CDoExpr [
              CSPat (CPVar (4,"transResult")) 
                (applyF (db "runT") [
                  foldr1 (\a b -> applyF (db "|>>=") [a,b]) --(applyF (db "doneT") [])
                    ([
                      applyF (entityConstructorFunction erdname (Entity entityName attrList) relationships) 
                        (map (\((Attribute name dom key null), varId) -> 
                          if (isForeignKey (Attribute name dom key null))
                            then applyF (erdname, (lowerFirst (getReferencedEntityName dom))++"Key")
                                        [CVar (varId, lowerFirst (getReferencedEntityName dom))]
                            else let cv = CVar (varId, lowerFirst name)
                                  in if hasDefault dom && not (isStringDom dom)
                                        && not null
                                     then applyF (pre "Just") [cv]
                                     else cv)
                          (zip noPKeys [1..])
                        )
                     ] ++ (map (\name -> applyF (controllerModuleName entityName, "add"++(linkTableName entityName name allEntities)) [cvar ((lowerFirst name)++"s")]) manyToManyEntities)
                    )
                ]),
                CSExpr (
                  applyF (pre "either") [
                    CLambda [CPVar(0, "_")]
                      (applyF ("Spicey","nextInProcessOr")
                        [callEntityListController entityName,
                         constF (pre "Nothing")]),
                    CLambda [CPVar(1, "error")]
                      (applyF displayErrorFunction
                         [applyF (db "showTError") [CVar(1,"error")]]),
                    CVar (4,"transResult")
                  ]
                )              
            ]
          )
        ]
        [] -- where clauses
      ]
      
editController :: ControllerGenerator
editController erdname (Entity entityName attrList) relationships allEntities =
  let
    manyToManyEntities = manyToMany allEntities (Entity entityName attrList)
    manyToOneEntities  = manyToOne (Entity entityName attrList) relationships
    pvar               = (0, lowerFirst entityName ++ "ToEdit")
    infovar            = (1, "sinfo")
  in
    controllerFunction
    ("Shows a form to edit the given "++entityName++" entity.")
    entityName "edit" 1
      (baseType (erdname,entityName) ~> controllerType
      )
      [
      CRule 
        [CPVar pvar] -- parameterlist for controller
        [
          noGuard (
           applyF (pre "$")
            [applyF (authModName,"checkAuthorization")
              [applyF (enauthModName,lowerFirst entityName++"OperationAllowed")
                [applyF (authModName,"UpdateEntity") [CVar pvar]]],
             CDoExpr (
              (CSPat (CPVar infovar) getUserSessionInfoFunc :
               map 
                (\ (ename, num) ->
                      CSPat (CPVar (num,"all"++ename++"s")) 
                            (applyF (db "runQ")
                                    [constF (erdname,"queryAll"++ename++"s")])
                )
                (zip (manyToOneEntities ++ manyToManyEntities) [1..])
              ) ++
              (map 
                (\ (ename, num) -> CSPat (CPVar (num,(lowerFirst (fst $ relationshipName entityName ename relationships))++ename)) 
                                (
                                  applyF (db "runJustT") [
                                    applyF (controllerModuleName entityName,"get"++(fst $ relationshipName entityName ename relationships)++ename) [CVar pvar]
                                  ]
                                )
                )
                (zip (manyToOneEntities) [1..])
              ) ++
              (map 
                (\ (ename, num) -> CSPat (CPVar (num,(lowerFirst (linkTableName entityName ename allEntities))++ename++"s")) 
                                (
                                  applyF (db "runJustT") [
                                    applyF (controllerModuleName entityName,"get"++entityName++ename++"s") [CVar pvar]
                                  ]
                                )
                )
                (zip (manyToManyEntities) [1..])
              ) ++
              [CSExpr (
                 applyF (pre "return")
                  [applyF (viewFunctionName entityName "edit")
                     ([CVar infovar,
                       tupleExpr
                        (
                          [CVar pvar] ++ 
                          (map (\ (ename, num) ->
                                 CVar (num,lowerFirst (linkTableName entityName
                                                       ename allEntities)
                                        ++ename++"s"))
                               (zip (manyToManyEntities) [1..]))
                        )
                      ] ++
                      (map 
                        (\ (ename, num) ->
                               CVar (num,lowerFirst (fst $ relationshipName
                                            entityName ename relationships)
                                         ++ ename)) 
                        (zip (manyToOneEntities) [1..])
                      ) ++
                      ((map (\ (ename, num) -> CVar (num, "all"++ename++"s"))
                            (zip (manyToOneEntities ++ manyToManyEntities)
                                 [1..])) ++
                      [applyF (controllerFunctionName entityName "update") []])
                    )
                  ]
                )
              ]
            )
           ]
          )
        ]
        [] -- where clauses
      ]

updateController :: ControllerGenerator
updateController erdname (Entity entityName attrList) _ allEntities =
 let manyToManyEntities = manyToMany allEntities (Entity entityName attrList)
     -- manyToOneEntities = manyToOne (Entity entityName attrList) relationships
     -- noPKeys = (filter notPKey attrList)
  in
    controllerFunction
    ("Persists modifications of a given "++entityName++" entity to the\n"++
     "database depending on the Boolean argument. If the Boolean argument\n"++
     "is False, nothing is changed.")
    entityName "update" 2
      ( boolType ~> 
        tupleType ([baseType (erdname, entityName)] ++
                   map (\name -> listType (ctvar name)) manyToManyEntities)
        ~> controllerType
      )
      [CRule [CPComb (pre "False") [], CPVar (0,"_")]
             [noGuard (callEntityListController entityName)]
             [],
       CRule 
        [CPComb (pre "True") [],
         tuplePattern
               ([CPVar (0, lowerFirst entityName)] ++
                (map (\ (param, varId) -> CPVar (varId, param)) 
                     (zip (map (\e -> lowerFirst e ++ "s" ++
                                      linkTableName entityName e allEntities)
                               manyToManyEntities)
                          [1..])))
        ] -- parameter list for controller
        [
          noGuard (
            CDoExpr (          
              [
                CSPat (CPVar (4,"transResult")) 
                  (applyF (db "runT") [
                    foldr1 (\a b -> applyF (db "|>>") [a,b])
                      (
                        [applyF (erdname, "update"++entityName) [cvar (lowerFirst entityName)]] ++ 
                        (map 
                          (\name -> 
                            applyF (db "|>>=") [
                              applyF (controllerModuleName entityName,"get"++entityName++name++"s") [cvar (lowerFirst entityName)],
                              CLambda [CPVar(0, "old"++(linkTableName entityName name allEntities)++name++"s")] (applyF (controllerModuleName entityName, "remove"++(linkTableName entityName name allEntities)) [cvar ("old"++(linkTableName entityName name allEntities)++name++"s"), cvar (lowerFirst entityName)])
                            ]
                          )
                         manyToManyEntities
                        ) ++
                        (map (\name -> applyF (controllerModuleName entityName, "add"++(linkTableName entityName name allEntities)) [cvar ((lowerFirst name)++"s"++(linkTableName entityName name allEntities)), cvar (lowerFirst entityName)]) manyToManyEntities)
                      )
                  ]),
                  CSExpr (
                    applyF (pre "either") [
                      CLambda [CPVar(0, "_")]
                        (applyF ("Spicey","nextInProcessOr")
                           [callEntityListController entityName,
                            constF (pre "Nothing")]),
                      CLambda [CPVar(1, "error")]
                        (applyF displayErrorFunction
                           [applyF (db "showTError") [CVar(1,"error")]]),
                      CVar (4,"transResult")
                    ]
                  )              
              ]
            )
          )
        ]
        [] -- where clauses
      ]

--- Generates controller to confirm and delete an entity.
confirmDeleteController :: ControllerGenerator
confirmDeleteController erdname (Entity entityName _) _ _ =
  let entlc  = lowerFirst entityName  -- entity name in lowercase
      entvar = (0, entlc)             -- entity parameter for controller
  in
  controllerFunction
  ("Deletes a given "++entityName++" entity (after asking for confirmation)\n"++
   "and proceeds with the list controller.")
  entityName "confirmDelete" 1
  (baseType (erdname, entityName) ~> controllerType)
  [CRule 
    [CPVar entvar]
    [noGuard (
      applyF (pre "$")
       [applyF (authModName,"checkAuthorization")
         [applyF (enauthModName,entlc++"OperationAllowed")
                 [applyF (authModName,"DeleteEntity") [CVar entvar]]],
        applyF ("Spicey","confirmController")
         [applyF ("HTML","h3")
           [list2ac
             [applyF ("HTML","htxt")
              [applyF (pre "concat")
               [list2ac [string2ac "Really delete entity \"",
                         applyF (erdname++"EntitiesToHtml",entlc++"ToShortView")
                                [CVar entvar],
                         string2ac "\"?"]]]]],
            CLambda [CPVar (1,"ack")]
             (applyF (pre "if_then_else")
               [CVar (1,"ack"),
                applyF (controllerFunctionName entityName "delete")
                       [CVar entvar],
                applyF (controllerFunctionName entityName "show")
                       [CVar entvar]])]])]
     [] -- where clauses
    ]

--- Generates controller to delete an entity.
deleteController :: ControllerGenerator
deleteController erdname (Entity entityName attrList) _ allEntities =
  let manyToManyEntities = manyToMany allEntities (Entity entityName attrList)
      entlc  = lowerFirst entityName  -- entity name in lowercase
      entvar = (0, entlc)             -- entity parameter for controller
  in
    controllerFunction
    ("Deletes a given "++entityName++" entity (depending on the Boolean\n"++
     "argument) and proceeds with the list controller.")
    entityName "delete" 1
    (baseType (erdname, entityName) ~> controllerType)
    [CRule 
      [CPVar entvar] -- entity parameter for controller
      [noGuard (
         applyF (pre "$")
          [applyF (authModName,"checkAuthorization")
            [applyF (enauthModName,entlc++"OperationAllowed")
                    [applyF (authModName,"DeleteEntity") [CVar entvar]]],
           CDoExpr
            [
              CSPat (CPVar (4,"transResult")) 
                (applyF (db "runT") [
                  foldr1 (\a b -> applyF (db "|>>") [a,b])
                   (map (\name ->
                        applyF (db "|>>=")
                         [applyF (controllerModuleName entityName,
                                  "get"++entityName++name++"s")
                                 [CVar entvar],
                          CLambda [CPVar(0, "old"++(linkTableName entityName name allEntities)++name++"s")]
                                  (applyF (controllerModuleName entityName,
                                           "remove"++(linkTableName entityName name allEntities))
                                    [cvar ("old"++(linkTableName entityName name allEntities)++name++"s"),
                                     CVar entvar ])
                        ]
                        ) 
                        manyToManyEntities
                    ++
                    [applyF (erdname, "delete"++entityName) [CVar entvar]]
                   )
                ]),
                CSExpr (
                  applyF (pre "either") [
                    CLambda [CPVar(0, "_")]
                      (callEntityListController entityName),
                    CLambda [CPVar(1, "error")]
                       (applyF displayErrorFunction
                          [applyF (db "showTError") [CVar(1,"error")]]),
                    CVar (4,"transResult")
                  ]
                )              
            ]
          ]
        )
      ]
      [] -- where clauses
    ]

listController :: ControllerGenerator
listController erdname (Entity entityName _) _ _ =
  let infovar = (0, "sinfo")
      entsvar = (1, (lowerFirst entityName)++"s")
   in
    controllerFunction
      ("Lists all "++entityName++" entities with buttons to show, delete,\n"++
       "or edit an entity.")
      entityName "list" 0
      controllerType
      [
      CRule 
        [] -- no arguments
        [
          noGuard (
           applyF (pre "$")
            [applyF (authModName,"checkAuthorization")
              [applyF (enauthModName,lowerFirst entityName++"OperationAllowed")
                [applyF (authModName,"ListEntities") []]],
             CDoExpr (            
              [CSPat (CPVar infovar) getUserSessionInfoFunc,
               CSPat (CPVar entsvar)
                     (applyF (db "runQ")
                             [constF (erdname,"queryAll"++entityName++"s")]),
               CSExpr (applyF (pre "return")
                             [applyF (viewFunctionName entityName "list")
                                     [CVar infovar, CVar entsvar]])
              ]
            )
           ]
          )
        ]
        [] -- where clauses
      ]

showController :: ControllerGenerator
showController erdname (Entity entityName attrList) relationships allEntities =
  let manyToManyEntities = manyToMany allEntities (Entity entityName attrList)
      manyToOneEntities  = manyToOne (Entity entityName attrList) relationships
      pvar               = (0, lowerFirst entityName)
      infovar            = (1, "sinfo")
  in
    controllerFunction
    ("Shows a "++entityName++" entity.")
    entityName "show" 1
      (baseType (erdname,entityName) ~> controllerType
      )
      [
      CRule 
        [CPVar pvar] -- parameterlist for controller
        [noGuard (
           applyF (pre "$")
            [applyF (authModName,"checkAuthorization")
              [applyF (enauthModName,lowerFirst entityName++"OperationAllowed")
                [applyF (authModName,"ShowEntity") [CVar pvar]]],
             CDoExpr (
              (CSPat (CPVar infovar) getUserSessionInfoFunc :
               map (\ (ename, num) ->
                     CSPat (CPVar (num,lowerFirst
                                         (fst $ relationshipName entityName
                                               ename relationships) ++ ename)) 
                           (applyF (db "runJustT")
                              [applyF (controllerModuleName entityName,
                                       "get"++ fst (relationshipName
                                                entityName ename relationships)
                                            ++ename)
                                      [CVar pvar]
                              ])
                   )
                   (zip (manyToOneEntities) [1..])
              ) ++
              (map (\ (ename, num) ->
                      CSPat (CPVar (num,lowerFirst (linkTableName entityName
                                                           ename allEntities)
                                        ++ename++"s"))
                            (applyF (db "runJustT")
                               [applyF (controllerModuleName entityName,
                                        "get"++entityName++ename++"s")
                                       [CVar pvar]])
                   )
                   (zip (manyToManyEntities) [1..])
              ) ++
              [CSExpr (
                 applyF (pre "return")
                    [applyF (viewFunctionName entityName "show")
                       ([CVar infovar, CVar pvar] ++
                        (map (\ (ename, num) ->
                                CVar (num,lowerFirst (fst $ relationshipName
                                             entityName ename relationships)
                                           ++ ename))
                             (zip (manyToOneEntities) [1..])) ++
                        (map (\ (ename, num) ->
                               CVar (num,lowerFirst (linkTableName entityName
                                                       ename allEntities)
                                         ++ename++"s"))
                             (zip (manyToManyEntities) [1..])))
                    ])
              ])
            ]
          )
        ]
        [] -- where clauses
      ]

-- Code to call the list controller of an entity where the current
-- URL parameters are passed to this list controller.
callEntityListController :: String -> CExpr
callEntityListController entityName =
  constF (controllerFunctionName entityName "list")

manyToManyAddOrRemove :: String -> Entity -> [String] -> [Entity] -> [CFuncDecl]
manyToManyAddOrRemove erdname (Entity entityName _) entities allEntities =
    (map (addOrRemoveFunction "add" "new" entityName) entities) ++
    (map (addOrRemoveFunction "remove" "delete" entityName) entities)
  where
    addOrRemoveFunction :: String -> String -> String -> String -> CFuncDecl
    addOrRemoveFunction funcPrefix dbFuncPrefix e1 e2 =      
      cmtfunc 
      (if (funcPrefix == "add")
        then ("Associates given entities with the "++entityName++" entity.")
        else ("Removes association to the given entities with the "++entityName++" entity."))
      (controllerModuleName e1, funcPrefix++(linkTableName e1 e2 allEntities))
      2 
      Private
      (listType (ctvar e2) ~> ctvar e1 ~> CTCons (db "Transaction")
                                                 [tupleType []])
      [
        CRule [CPVar (0, (lowerFirst e2)++"s"), CPVar (1, (lowerFirst e1))]
        [
          noGuard (
          applyF (db "mapT_") [
            CLambda [CPVar(2, "t")] (applyF (erdname, dbFuncPrefix++(linkTableName e1 e2 allEntities)) [
                applyF (erdname, (lowerFirst e1)++"Key") [cvar (lowerFirst e1)],
                applyF (erdname, (lowerFirst e2)++"Key") [cvar "t"]
              ]
            ),
            cvar ((lowerFirst e2)++"s")
          ]
          )
        ]
        [] -- where clauses
      ]

getAll :: String -> Entity -> [String] -> [Entity] -> [CFuncDecl]
getAll erdname (Entity entityName _) entities _ =
    map getAllFunction entities
  where
    getAllFunction :: String -> CFuncDecl
    getAllFunction foreignEntity =
      cmtfunc 
      ("Gets all "++foreignEntity++" entities.")
      (controllerModuleName entityName, "getAll"++foreignEntity++"s")
      0
      Private
      (ioType (listType (ctvar foreignEntity)))
      [
        CRule []
        [
          noGuard (
            applyF (db "runQ") [
              applyF (db "queryAll") [
                      CLambda [CPVar(0, take 1 (lowerFirst foreignEntity) )] (CLetDecl [(CLocalVar (1,"key"))] (applyF (erdname, lowerFirst foreignEntity) [cvar "key", cvar (take 1 (lowerFirst foreignEntity))]))
                    ]          
            ] 
          )
        ]
        []
      ]
      
manyToManyGetRelated :: String -> Entity -> [String] -> [Entity] -> [CFuncDecl]
manyToManyGetRelated erdname (Entity entityName _) entities allEntities =      
    map getRelatedFunction entities
  where
    getRelatedFunction :: String -> CFuncDecl
    getRelatedFunction foreignEntity =
      cmtfunc 
      ("Gets the associated "++foreignEntity++" entities for a given "++entityName++" entity.")
      (controllerModuleName entityName, "get"++(linkTableName entityName foreignEntity allEntities)++foreignEntity++"s")
      0
      Private
      (ctvar entityName ~> CTCons (db "Query") [listType (ctvar foreignEntity)])
      [
        CRule [CPVar (1, (take 1 $ lowerFirst entityName)++foreignEntity)]
        [
          noGuard (
            applyF (db "queryAll") [
                    CLambda [CPVar(0, take 1 (lowerFirst foreignEntity) )] 
                      (CLetDecl [(CLocalVar (1,(take 1 $ lowerFirst entityName)++"key")), (CLocalVar (2,(take 1 $ lowerFirst foreignEntity)++"key"))]
                      (
                        foldr (\a b -> applyF ("Dynamic", "<>") [a,b]) 
                        (applyF (erdname, lowerFirst (linkTableName entityName foreignEntity allEntities)) [cvar ((take 1 $ lowerFirst entityName)++"key"), cvar ((take 1 $ lowerFirst foreignEntity)++"key")])
                        [
                        (applyF (erdname, lowerFirst entityName) [cvar $ (take 1 $ lowerFirst entityName)++"key", cvar ((take 1 $ lowerFirst entityName)++foreignEntity)]),
                        (applyF (erdname, lowerFirst foreignEntity) [cvar $ (take 1 $ lowerFirst foreignEntity)++"key", cvar (take 1 (lowerFirst foreignEntity))])
                        ]
                      )
                      )
                  ]
          )
        ]
        []
      ]

manyToOneGetRelated :: String -> Entity -> [String] -> [Entity]
                    -> [Relationship] -> [CFuncDecl]
manyToOneGetRelated erdname (Entity entityName _) entities _ relationships =      
    map getRelatedFunction entities
  where
    getRelatedFunction :: String -> CFuncDecl
    getRelatedFunction foreignEntity =
      let argvar  = (1, (take 1 $ lowerFirst entityName)++foreignEntity)
          rname   = fst (relationshipName entityName foreignEntity relationships)
          fkeysel = lowerFirst entityName++foreignEntity++rname++"Key"
      in
      cmtfunc 
      ("Gets the associated "++foreignEntity++" entity for a given "++
       entityName++" entity.")
      (controllerModuleName entityName,
       "get"++rname++foreignEntity)
      0
      Private
      ((ctvar entityName) ~> CTCons (db "Transaction") [ctvar foreignEntity])
      [CRule [CPVar argvar]
        [noGuard (
            applyF (erdname,"get"++foreignEntity)
                   [applyF (erdname,fkeysel) [CVar argvar]])
        ]
        []
      ]

relationshipName :: String -> String -> [Relationship] -> (String, String)
relationshipName e1 e2 (rel:relrest)=
  case rel of
    (Relationship name [(REnd relE1 _ _), (REnd relE2 relName _)]) ->
      if ((relE1 == e1 && relE2 == e2) || (relE1 == e2 && relE2 == e1)) then (name, relName) else relationshipName e1 e2 relrest
relationshipName _ _ [] = error "relationshipName: relationship not found"
---- aux ---


displayErrorFunction :: QName
displayErrorFunction = ("Spicey", "displayError")

entityConstructorFunction :: String -> Entity -> [Relationship] -> QName
entityConstructorFunction erdname (Entity entityName attrList) relationships =
  (erdname, "new" ++ 
    entityName ++ (newSuffix entityName attrList relationships)
  )

-- entityName: Name of entity the controller should be generated for
-- controllerType: the function of the generated Controller, e.g. "new", "edit", "list"
-- arity
-- functionType: the type of the controller function
-- rules: the rules defining the controller
controllerFunction description entityName controllerType arity functionType
                   rules =
  cmtfunc description (controllerFunctionName entityName controllerType) arity
          (if controllerType `elem` ["main"]
           then Public
           else Private)
          functionType rules

getReferencedEntityName :: Domain -> String
getReferencedEntityName t =
  case t of KeyDom kd -> kd
            _         -> ""
            
relatedEntityNames :: Entity -> [Relationship] -> [String]
relatedEntityNames (Entity entityName attrlist) relationships =
  map (\(Relationship _ ((REnd name1 _ _):(REnd name2 _ _):[])) -> if (name1 == entityName) then name2 else name1) (relationshipsForEntity (Entity entityName attrlist) relationships)

-- gets all relationships 
relationshipsForEntity :: Entity -> [Relationship] -> [Relationship]
relationshipsForEntity (Entity entityName _) relationships =
  filter (\(Relationship _ ((REnd name1 _ _):(REnd name2 _ _):[])) -> name1 == entityName || name2 == entityName) (filter (not . isGeneratedR) relationships)
    
------ from ERD CodeGeneration

newSuffix eName attrs rels = 
  let
    generatedRs = filter isGeneratedR rels
    exactRs  = filter isExactB  generatedRs --(i,i), i>1
    maxRs    = filter isMaxB    generatedRs --(0,i), i>1
    minMaxRs = filter isMinMaxB generatedRs --(i,j), i>0, j>i
  in
    concatMap ("With"++)
              (map attributeName (filter isForeignKey attrs)) ++
    if (length (exactRs ++ maxRs ++ minMaxRs))==0
    then ""
    else concatMap (\k->"With"++k++"Keys")
                   (map (relatedRelation eName)
                        (exactRs++maxRs++minMaxRs))
  where
    isExactB (Relationship _ [REnd _ _ _, REnd _ _ c]) =
      case c of Exactly i -> i>1
                _         -> False
    isMaxB (Relationship _ [REnd _ _ _, REnd _ _ c]) =
      case c of (Between 0 (Max i)) -> i>1
                _                   -> False
    isMinMaxB (Relationship _ [REnd _ _ _, REnd _ _ c]) =
      case c of (Between i (Max j)) -> i>0 && j>i
                _                   -> False

isGeneratedR (Relationship n _) = n == ""

-- extracts the name of the relationship related to a given entity name
relatedRelation :: String -> Relationship -> String
relatedRelation en (Relationship _ [REnd en1 _ _, REnd en2 _ _]) =
  if en==en1 then en2 else en1

relationshipsForEntityName :: String -> [Relationship] -> [Relationship]
relationshipsForEntityName ename rels = filter endsIn rels
 where
  endsIn (Relationship _ ends) = any (\ (REnd n _ _) -> ename == n) ends

------------------------------------------------------------------------
-- Generate the module defining the default controller.
generateDefaultController :: String -> [Entity] -> CurryProg
generateDefaultController _ (Entity ename _:_) = CurryProg
  defCtrlModName
  [ename++"Controller","Spicey"] -- imports
  [] -- typedecls
  -- functions
  [cmtfunc
    "The default controller of the application."
    (defCtrlModName,"defaultController")
    1
    Public
    controllerType
    [CRule []
     [noGuard (constF (ename++"Controller","main"++ename++"Controller"))]
     [] -- where clauses
    ]
  ]
  [] -- opdecls

------------------------------------------------------------------------
-- Generate all default authorizations.
generateAuthorizations :: String -> [Entity] -> CurryProg
generateAuthorizations erdname entities = CurryProg
  enauthModName
  ["Authorization", erdname] -- imports
  [] -- typedecls
  -- functions
  (map operationAllowed entities)
  [] -- opdecls
 where
  operationAllowed (Entity entityName _) =
   cmtfunc
    ("Checks whether the application of an operation to a "++entityName++"\n"++
     "entity is allowed.")
    (enauthModName, lowerFirst entityName ++ "OperationAllowed")
    1
    Public
    (CTCons (authModName,"AccessType") [baseType (erdname,entityName)]
     ~> ioType (baseType (authModName,"AccessResult")))
    [CRule [CPVar (1,"at")]
     [noGuard (CCase (CVar (1,"at"))
       [CBranch (CPComb (authModName,"ListEntities") []) allowed,
        CBranch (CPComb (authModName,"NewEntity")    []) allowed,
        CBranch (CPComb (authModName,"ShowEntity")   [CPVar (2,"_")]) allowed,
        CBranch (CPComb (authModName,"DeleteEntity") [CPVar (2,"_")]) allowed,
        CBranch (CPComb (authModName,"UpdateEntity") [CPVar (2,"_")]) allowed])]
     [] -- where clauses
    ]

  -- Expression implemented access allowed
  allowed = applyF (pre "return") [constF (authModName,"AccessGranted")]

  -- Expression implemented access denied
  --exprDenied = applyF (pre "return")
  --                    [applyF (authModName,"AccessDenied")
  --                            [string2ac "Operation not allowed!"]]

------------------------------------------------------------------------
-- Auxiliaries:

getUserSessionInfoFunc = constF ("SessionInfo","getUserSessionInfo")

------------------------------------------------------------------------