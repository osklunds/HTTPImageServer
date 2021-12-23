
module HTMLGen.Tests where

import HTMLGen

generateImagePageTest = writeFile "test.html" html
  where
    html = generateImagePage parent leftImg curImg rightImg
    parent = "overview"
    leftImg = Just "img1"
    curImg = "Images/img2.png"
    rightImg = Just "img3"

