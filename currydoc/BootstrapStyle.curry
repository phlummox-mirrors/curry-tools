-------------------------------------------------------------------
--- This library contains some operations to generate web pages
--- rendered with [Bootstrap](http://twitter.github.com/bootstrap/)
-------------------------------------------------------------------

module BootstrapStyle(bootstrapForm,bootstrapPage,titledSideMenu) where

import HTML

--- An HTML form rendered with bootstrap.
--- @param rootdir - the root directory to find styles (in subdirectory `css`
---                  of the root) and images (in subdirectory `img` of the root)
--- @param styles - the style files to be included (typically,
---                 `bootstrap` and `bootstrap-responsive`), stored in
---                 `rootdir/css` with suffix `.css`)
--- @param title - the title of the form
--- @lefttopmenu - the menu shown in the left side of the top navigation bar
--- @righttopmenu - the menu shown in the right side of the top navigation bar
---                 (could be empty)
--- @param columns - number of columns for the left-side menu
---                  (if columns==0, then the left-side menu is omitted)
--- @param sidemenu - the menu shown at the left-side of the main document
---                   (maybe created with 'titledSideMenu')
--- @param htmlHeader   - the main header (rendered with hero-unit style)
--- @param contents - the main contents of the document
--- @param htmlFooter   - the footer of the document
bootstrapForm :: String -> [String] -> String -> [[HtmlExp]]
              -> [[HtmlExp]] -> Int -> [HtmlExp] -> [HtmlExp]
              -> [HtmlExp] -> [HtmlExp] -> HtmlForm
bootstrapForm rootdir styles title lefttopmenu righttopmenu
              leftcols sidemenu htmlHeader contents htmlFooter =
  HtmlForm title
           ([formEnc "utf-8",responsiveView,icon] ++
             map (\n -> formCSS (rootdir++"/css/"++n++".css")) styles)
           (topNavigationBar lefttopmenu righttopmenu ++
            [blockstyle "container-fluid"
              [blockstyle "row-fluid"
                (if leftcols==0
                 then [headerRow, blockstyle "row-fluid" contents]
                 else [blockstyle ("span"++show leftcols)
                        [blockstyle "well sidebar-nav" sidemenu],
                       blockstyle ("span"++show (12-leftcols))
                        [headerRow,
                         blockstyle "row-fluid" contents]]),
               hrule,
               footer htmlFooter]])
 where
  -- for a better view on handheld devices:
  responsiveView =
    HeadInclude (HtmlStruct "meta"
                    [("name","viewport"),
                     ("content","width=device-width, initial-scale=1.0")] [])

  icon = HeadInclude (HtmlStruct "link"
                                 [("rel","shortcut icon"),
                                  ("href",rootdir++"/img/favicon.ico")] [])

  -- header row:
  headerRow = blockstyle "row-fluid"
                [header htmlHeader `addAttr` ("class","hero-unit")]

--- An HTML page rendered with bootstrap.
--- @param rootdir - the root directory to find styles (in subdirectory `css`
---                  of the root) and images (in subdirectory `img` of the root)
--- @param styles - the style files to be included (typically,
---                 `bootstrap` and `bootstrap-responsive`), stored in
---                 `rootdir/css` with suffix `.css`)
--- @param title - the title of the form
--- @lefttopmenu - the menu shown in the left side of the top navigation bar
--- @righttopmenu - the menu shown in the right side of the top navigation bar
---                 (could be empty)
--- @param columns - number of columns for the left-side menu
---                  (if columns==0, then the left-side menu is omitted)
--- @param sidemenu - the menu shown at the left-side of the main document
---                   (maybe created with 'titledSideMenu')
--- @param htmlHeader   - the main header (rendered with hero-unit style)
--- @param contents - the main contents of the document
--- @param htmlFooter   - the footer of the document
bootstrapPage :: String -> [String] -> String -> [[HtmlExp]]
              -> [[HtmlExp]] -> Int -> [HtmlExp] -> [HtmlExp]
              -> [HtmlExp] -> [HtmlExp] -> HtmlPage
bootstrapPage rootdir styles title lefttopmenu righttopmenu
              leftcols sidemenu htmlHeader contents htmlFooter =
  HtmlPage title
           ([pageEnc "utf-8",responsiveView,icon] ++
             map (\n -> pageCSS (rootdir ++ "/css/" ++ n ++ ".css")) styles)
           (topNavigationBar lefttopmenu righttopmenu ++
                (if leftcols == 0
                 then fluidBlock $ [headerRow, blockstyle "row-fluid" contents]
                        ++ footline
                 else ([blockstyle "container-fluid" [headerRow]]
                        ++ (fluidBlock $ sidemenuBlock
                        ++ [blockstyle ("span" ++ show (12-leftcols))
                                       contentBlock]
                        ++ footline))))
 where
  -- for a better view on handheld devices:
  responsiveView =
    pageMetaInfo [("name","viewport"),
                  ("content","width=device-width, initial-scale=1.0")]

  icon = pageLinkInfo [("rel","shortcut icon"),
                       ("href",rootdir++"/img/favicon.ico")]

  -- header row:
  headerRow = blockstyle "row-fluid"
                [header htmlHeader `addAttr` ("class","hero-unit")]

  -- content block
  contentBlock = [blockstyle "row-fluid" contents]
  
  -- sidemenu block
  sidemenuBlock = [nav [blockstyle ("span"++show leftcols)
                                   [blockstyle "well sidebar-nav" sidemenu]]]

  -- footline
  footline = [hrule, footer htmlFooter]

  fluidBlock b = [blockstyle "container-fluid" [blockstyle "row-fluid" b]]

-- Navigation bar at the top:
topNavigationBar :: [[HtmlExp]] -> [[HtmlExp]] -> [HtmlExp]
topNavigationBar leftmenu rightmenu = [nav
  [blockstyle "navbar navbar-inverse navbar-fixed-top"
    [blockstyle "navbar-inner"
      [blockstyle "container-fluid"
        ([ulist leftmenu `addClass` "nav"] ++
         if null rightmenu then []
         else [blockstyle "navbar-text pull-right"
                [ulist rightmenu `addClass` "nav"]])]]]]

--- Create a side menu containing a title and a list of items:
titledSideMenu :: String -> [[HtmlExp]] -> [HtmlExp]
titledSideMenu title items =
  (if null title
   then []
   else [HtmlStruct "small" [] [htxt title]]) ++
  [ulist items `addClass` "nav nav-list"]

------------------------------------------------------------------------
