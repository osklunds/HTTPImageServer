
module HTMLGen
( generateImagePage
, generateFolderPage
)
where

--------------------------------------------------------------------------------
-- Image page
--------------------------------------------------------------------------------

generateImagePage :: String -> Maybe String -> String -> Maybe String -> String
generateImagePage parent leftImg curImg rightImg =
  basePagePart1 ++
  backgroundImagePart curImg ++
  basePagePart2 ++
  parentButtonPart parent ++
  leftButtonPart leftImg ++
  rightButtonPart rightImg ++
  basePagePart3

basePagePart1 :: String
basePagePart1 =
  "<!DOCTYPE html>\n\
  \<html>\n\
  \<head>\n\
  \<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n\
  \<style>\n\
  \body, html {\n\
  \  height: 100%;\n\
  \  margin: 0;\n\
  \  background-color: black\n\
  \}\n\
  \\n\
  \.bg {\n"

backgroundImagePart :: String -> String
backgroundImagePart imagePath =
  "  background-image: url(\"" ++ imagePath ++ "\");\n"

basePagePart2 :: String
basePagePart2 =
  "\n\
  \  height: 100%;\n\
  \  float: center;\n\
  \\n\
  \  background-position: center;\n\
  \  background-repeat: no-repeat;\n\
  \  background-size: contain;\n\
  \}\n\
  \\n\
  \.button_top {\n\
  \  cursor: pointer;\n\
  \  float: left;\n\
  \  width: 60%;\n\
  \  height: 20%;\n\
  \  background-color: blue;\n\
  \  opacity: .0;\n\
  \}\n\
  \\n\
  \.button_left {\n\
  \  cursor: pointer;\n\
  \  float: left;\n\
  \  width: 20%;\n\
  \  height: 100%;\n\
  \  background-color: red;\n\
  \  opacity: .0;\n\
  \}\n\
  \\n\
  \.button_right {\n\
  \  cursor: pointer;\n\
  \  width: 20%;\n\
  \  height: 100%;\n\
  \  background-color: green;\n\
  \  margin-left: auto;\n\
  \  margin-right: 0;\n\
  \  opacity: .0;\n\
  \}\n\
  \\n\
  \</style>\n\
  \</head>\n\
  \<body>\n\
  \\n\
  \<div class=\"bg\">\n"

leftButtonPart :: Maybe String -> String
leftButtonPart (Just imagePath) =
  "  <div class=\"button_left\" onclick=\"window.location='"
    ++ imagePath ++ ".html';\"></div>\n\n"
leftButtonPart Nothing = ""

parentButtonPart :: String -> String
parentButtonPart parentPath =
  "  <div class=\"button_top\" onclick=\"window.location='"
    ++ parentPath ++ ".html';\"></div>\n\n"

rightButtonPart :: Maybe String -> String
rightButtonPart (Just imagePath) =
  "  <div class=\"button_right\" onclick=\"window.location='"
    ++ imagePath ++ ".html';\"></div>\n"
rightButtonPart Nothing = ""

basePagePart3 :: String
basePagePart3 =
  "</div>\n\
  \\n\
  \</body>\n\
  \</html>\n"

--------------------------------------------------------------------------------
-- Folder page
--------------------------------------------------------------------------------

generateFolderPage = undefined