
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

generateFolderPage :: String -> String -> [String] -> String
generateFolderPage title parent imagePaths =
  folderBasePagePart1 title parent ++
  (concatMap thumbnailPart imagePaths) ++
  folderBasePagePart2
  
folderBasePagePart1 :: String -> String -> String
folderBasePagePart1 title parent =
  "<!DOCTYPE html>\n\
  \<html>\n\
  \<head>\n\
  \<style>\n\
  \* {\n\
  \  box-sizing: border-box;\n\
  \}\n\
  \\n\
  \.column {\n\
  \  float: left;\n\
  \  width: 33.33%;\n\
  \  padding-left: 5px;\n\
  \  padding-right: 5px;\n\
  \  padding-top: 5px;\n\
  \  padding-bottom: : 5px;\n\
  \}\n\
  \\n\
  \.row::after {\n\
  \  content: \"\";\n\
  \  clear: both;\n\
  \  display: table;\n\
  \}\n\
  \\n\
  \.image_container {\n\
  \  cursor: pointer;\n\
  \}\n\
  \\n\
  \.image {\n\
  \  object-fit: cover;\n\
  \  aspect-ratio: 1/1;\n\
  \  width: 100%;\n\
  \}\n\
  \\n\
  \.button_top {\n\
  \  background-color: lightgray;\n\
  \  width: 100%;\n\
  \  padding: 10px;\n\
  \  cursor: pointer;\n\
  \  font-family: Helvetica, sans-serif;\n\
  \}\n\
  \\n\
  \</style>\n\
  \</head>\n\
  \<body>\n\
  \\n\
  \<div class=\"button_top\" height=\"20px\" onclick=\"window.location='" ++ parent ++ "'\">\n\
  \  " ++ title ++ "\n\
  \</div>\n\
  \\n\
  \<div class=\"row\">\n"

thumbnailPart imgPath =
  "  <div class=\"column\">\n\
  \    <div class=\"image_container\" onclick=\"window.location='bild1_full.html';\">\n\
  \      <img class=\"image\" src=\"" ++ imgPath ++ "\" width=\"100%\">\n\
  \    </div>\n\
  \  </div>\n"

folderBasePagePart2 :: String
folderBasePagePart2 =
  "</div>\n\
  \\n\
  \</body>\n\
  \</html>\n"
