-------------------------------------------------------------------
--- This library contains some operations to generate web pages
--- rendered with [Bootstrap](http://twitter.github.com/bootstrap/)
---
--- @author Michael Hanus
--- @version January 2016
-------------------------------------------------------------------

module Bootstrap3Style(bootstrapForm,bootstrapPage,titledSideMenu) where

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
--- @param header   - the main header (rendered with jumbotron style)
--- @param contents - the main contents of the document
--- @param footer   - the footer of the document
bootstrapForm :: String -> [String] -> String -> (String,[HtmlExp])
              -> [[HtmlExp]] -> [[HtmlExp]] -> Int -> [HtmlExp] -> [HtmlExp]
              -> [HtmlExp] -> [HtmlExp] -> HtmlForm
bootstrapForm rootdir styles title brandurltitle lefttopmenu righttopmenu
              leftcols sidemenu header contents footer =
  HtmlForm title
           ([formEnc "utf-8",responsiveView,icon] ++
             map (\n -> formCSS (rootdir++"/css/"++n++".css")) styles)
           (bootstrapBody rootdir brandurltitle lefttopmenu righttopmenu
                          leftcols sidemenu header contents footer)
 where
  -- for a better view on handheld devices:
  responsiveView =
    HeadInclude (HtmlStruct "meta"
                    [("name","viewport"),
                     ("content","width=device-width, initial-scale=1.0")] [])

  icon = HeadInclude (HtmlStruct "link"
                                 [("rel","shortcut icon"),
                                  ("href",rootdir++"/img/favicon.ico")] [])

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
--- @param header   - the main header (rendered with jumbotron style)
--- @param contents - the main contents of the document
--- @param footer   - the footer of the document
bootstrapPage :: String -> [String] -> String -> (String,[HtmlExp])
              -> [[HtmlExp]] -> [[HtmlExp]] -> Int -> [HtmlExp] -> [HtmlExp]
              -> [HtmlExp] -> [HtmlExp] -> HtmlPage
bootstrapPage rootdir styles title brandurltitle lefttopmenu righttopmenu
              leftcols sidemenu header contents footer =
  HtmlPage title
           ([pageEnc "utf-8",responsiveView,icon] ++
             map (\n -> pageCSS (rootdir++"/css/"++n++".css")) styles)
           (bootstrapBody rootdir brandurltitle lefttopmenu righttopmenu
                          leftcols sidemenu header contents footer)
 where
  -- for a better view on handheld devices:
  responsiveView =
    pageMetaInfo [("name","viewport"),
                  ("content","width=device-width, initial-scale=1.0")]

  icon = pageLinkInfo [("rel","shortcut icon"),
                       ("href",rootdir++"/img/favicon.ico")]

--- Create body of HTML page. Used by bootstrapForm and bootstrapPage.
bootstrapBody :: String -> (String,[HtmlExp]) -> [[HtmlExp]]
              -> [[HtmlExp]] -> Int -> [HtmlExp] -> [HtmlExp]
              -> [HtmlExp] -> [HtmlExp] -> [HtmlExp]
bootstrapBody rootdir brandurltitle lefttopmenu righttopmenu
              leftcols sidemenu header contents footer =
  topNavigationBar brandurltitle lefttopmenu righttopmenu ++
  [blockstyle "container-fluid"
   ([blockstyle "row"
      (if leftcols==0
       then [blockstyle (bsCols 12)
              (headerRow ++ contents)]
       else [blockstyle (bsCols leftcols)
              [blockstyle "well nav-sidebar" sidemenu],
             blockstyle (bsCols (12-leftcols))
              (headerRow ++ contents)])] ++
     if null footer
      then []
      else [hrule, HtmlStruct "footer" [] footer]),
   -- JavaScript includes placed at the end so page loads faster:
   HtmlStruct "script" [("src",rootdir++"/js/jquery.min.js")] [],
   HtmlStruct "script" [("src",rootdir++"/js/bootstrap.min.js")] []]
 where
  bsCols n = "col-sm-" ++ show n ++ " " ++ "col-md-" ++ show n
  
  -- header row:
  headerRow = if null header
              then []
              else [HtmlStruct "header" [("class","jumbotron")] header]


-- Navigation bar at the top. The first argument is a header element
-- put at the left, the second and third arguments are the left
-- and right menus which will be collapsed if the page is two small.
topNavigationBar :: (String,[HtmlExp]) -> [[HtmlExp]] -> [[HtmlExp]]
                 -> [HtmlExp]
topNavigationBar (brandurl,brandtitle) leftmenu rightmenu =
  [blockstyle "navbar navbar-inverse navbar-fixed-top"
    [blockstyle "container-fluid"
      [blockstyle "navbar-header"
         [HtmlStruct "button"
           [("type","button"),("class","navbar-toggle collapsed"),
            ("data-toggle","collapse"),("data-target","#topnavbar"),
            ("aria-expanded","false"),("aria-controls","navbar")]
           [textstyle "sr-only" "Toggle navigation",
            textstyle "icon-bar" "",
            textstyle "icon-bar" "",
            textstyle "icon-bar" ""],
          href brandurl brandtitle `addClass` "navbar-brand"],
        HtmlStruct "div" [("id","topnavbar"),
                          ("class","navbar-collapse collapse")]
         ([ulist leftmenu `addClass` "nav navbar-nav"] ++
          if null rightmenu then []
          else [ulist rightmenu `addClass` "nav navbar-nav navbar-right"])]]]

-- Create a side menu containing a title and a list of items:
titledSideMenu :: String -> [[HtmlExp]] -> [HtmlExp]
titledSideMenu title items =
  (if null title
   then []
   else [HtmlStruct "small" [] [htxt title]]) ++
  [ulist items `addClass` "nav nav-sidebar"]

------------------------------------------------------------------------
