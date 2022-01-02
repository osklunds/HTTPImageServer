
module HTMLGen.Tests where

import HTMLGen

generateImagePageTest = writeFile "test.html" html
  where
    html = generateImagePage parent leftImg curImg rightImg
    parent = "overview"
    leftImg = Just "img1"
    curImg = "Images/img2.png"
    rightImg = Just "img3"

generateFolderPageTest = writeFile "test.html" html
  where
    html = generateFolderPage "current/path/is/this" "parent" imagePaths
    imagePaths = ["Images/img1.png",
                  "Images/img2.png",
                  "Images/img3.png",
                  "Images/img4.png",
                  "Images/img5.png",
                  "Images/img1.png",
                  "Images/img2.png",
                  "Images/img3.png"]

