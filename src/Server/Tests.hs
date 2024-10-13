
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Tests where

import Test.QuickCheck
import Test.QuickCheck.Monadic hiding (assert)
import Control.Concurrent
import Control.Monad
import Control.Exception as CE hiding (assert)
import Data.Either

import System.IO.Temp
import System.FilePath
import System.Directory
import System.Timeout
import Control.Concurrent.Async

import Network.HTTP.Simple

import Text.Regex.PCRE.Heavy
import Text.Regex.PCRE.Light (multiline, dotall)
import Data.Text
import Data.Text.Encoding

import Server

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

prop_folderPage_root = runTest $ do
    assertResponseContainsStrings "" [
        -- Top button
        "<div class=\"top_button\" height=\"30px\" >\n\
        \/\n\
        \\n\
        \</div>",

        -- Folder button 1
        -- Only check all details once to avoid overspecifying in case of changes
        "<div class=\"folder_button\" onclick=\"window.location='/level1_1'\">\n\
        \    /level1_1\n\
        \</div>",

        -- Folder button 2
        "folder_button",
        "window.location",
        "/level1_2",
        "/level1_2",

        -- Folder button 3
        "folder_button",
        "window.location",
        "/level1_3",
        "/level1_3",

        -- Folder button 4
        "folder_button",
        "window.location",
        "/level1_4utf8såäöあ",
        "/level1_4utf8såäöあ",

        -- Folder button onlyInThumbs
        "folder_button",
        "window.location",
        "/onlyInThumbs",
        "/onlyInThumbs",

        -- root_level_img1
        -- Only check all details once to avoid overspecifying in case of changes
        "<link rel=\"preload\" href=\"/root_level_img1.jpg.thumb\" as=\"image\"/>\n\
        \<div class=\"column\">\n\
        \    <div class=\"image_container\" onclick=\"window.location='/root_level_img1.jpg.html';\">\n\
        \        <img class=\"image\" loading=\"lazy\" src=\"/root_level_img1.jpg.thumb\" width=\"100%\">\n\
        \    </div>\n\
        \</div>",

        -- root_level_img2
        "preload",
        "/root_level_img2.jpg.thumb",
        "image_container",
        "window.location",
        "/root_level_img2.jpg.html",
        "img class",
        "/root_level_img2.jpg.thumb"
        ]

    return ()

prop_folderPage_level1 = runTest $ do
    assertResponseContainsStrings "/level1_1" [
       -- Top button
       "<div class=\"top_button\" height=\"30px\" onclick=\\\"window.location='/.';\">\n\
       \/level1_1\n\
       \\n\
       \</div>",

       -- Folder button 2_1
       "folder_button",
       "window.location",
       "/level1_1/level2_1",
       "/level1_1/level2_1",

       -- Folder button 2_2
       "folder_button",
       "window.location",
       "/level1_1/level2_2",
       "/level1_1/level2_2",

        -- Image
        "preload",
        "/level1_1/level11_img.jpg.thumb",
        "image_container",
        "window.location",
        "/level1_1/level11_img.jpg.html",
        "img class",
        "/level1_1/level11_img.jpg.thumb"
       ]

prop_folderPage_specialChars = runTest $ do
    assertResponseContainsStrings "/level1_4utf8såäöあ" [
       -- Top button
       "<div class=\"top_button\" height=\"30px\" onclick=\\\"window.location='/.';\">\n\
       \/level1_4utf8såäöあ\n\
       \\n\
       \</div>",

        -- Image
        "preload",
        "/level1_4utf8såäöあ/åäöあabc.jpg.thumb",
        "image_container",
        "window.location",
        "/level1_4utf8såäöあ/åäöあabc.jpg.html",
        "img class",
        "/level1_4utf8såäöあ/åäöあabc.jpg.thumb"
       ]

prop_folderPage_level2 = runTest $ do
    assertResponseContainsStrings "/level1_1/level2_1" [
       -- Top button
       "<div class=\"top_button\" height=\"30px\" onclick=\\\"window.location='/level1_1';\">\n\
       \/level1_1/level2_1\n\
       \\n\
       \</div>"
       ]

    assertResponseContainsStrings "/level1_1/level2_2" [
       -- Top button
       "<div class=\"top_button\" height=\"30px\" onclick=\\\"window.location='/level1_1';\">\n\
       \/level1_1/level2_2\n\
       \\n\
       \</div>",

       -- Folder button
       "folder_button",
       "window.location",
       "/level1_1/level2_2/level3",
       "/level1_1/level2_2/level3",

        -- Image
        "preload",
        "/level1_1/level2_2/level2_img.jpg.thumb",
        "image_container",
        "window.location",
        "/level1_1/level2_2/level2_img.jpg.html",
        "img class",
        "/level1_1/level2_2/level2_img.jpg.thumb"
       ]

prop_folderPage_level3 = runTest $ do
    assertResponseContainsStrings "/level1_1/level2_2/level3" [
       -- Top button
       "<div class=\"top_button\" height=\"30px\" onclick=\\\"window.location='/level1_1/level2_2';\">\n\
       \/level1_1/level2_2/level3\n\
       \\n\
       \</div>",

       -- Folder button
       "folder_button",
       "window.location",
       "/level1_1/level2_2/level3/level4",
       "/level1_1/level2_2/level3/level4",

        -- Image
        "preload",
        "/level1_1/level2_2/level3/just_a_name.jpg.thumb",
        "image_container",
        "window.location",
        "/level1_1/level2_2/level3/just_a_name.jpg.html",
        "img class",
        "/level1_1/level2_2/level3/just_a_name.jpg.thumb"
       ]

prop_folderPage_level4 = runTest $ do
    assertResponseContainsStrings "/level1_1/level2_2/level3/level4" [
       -- Top button
       "<div class=\"top_button\" height=\"30px\" onclick=\\\"window.location='/level1_1/level2_2/level3';\">\n\
       \/level1_1/level2_2/level3/level4\n\
       \\n\
       \</div>",

        -- Image
        "preload",
        "/level1_1/level2_2/level3/level4/just_a_name.jpg.thumb",
        "image_container",
        "window.location",
        "/level1_1/level2_2/level3/level4/just_a_name.jpg.html",
        "img class",
        "/level1_1/level2_2/level3/level4/just_a_name.jpg.thumb"
       ]

prop_folderPage_onlyInThumbs = runTest $ do
    assertResponseContainsStrings "/onlyInThumbs" [
       -- Top button
       "<div class=\"top_button\" height=\"30px\" onclick=\\\"window.location='/.';\">\n\
       \/onlyInThumbs\n\
       \\n\
       \</div>"
       ]

prop_folderPage_onlyInFull = runTest $ do
    assertError "/onlyInFull"

prop_folderPage_pathDoesNotExist = runTest $ do
    assertError "/doesNotExist"

prop_imagePage_1Of2 = runTest $ do
    assertResponseContainsStrings "/root_level_img1.jpg.html" [
        -- The image itself
        "background-image: url\\(\"/./root_level_img1.jpg.full\"\\);",

        -- Navigation buttons
        "<div class=\"left_button\" ></div>",
        "<div class=\"top_button\" onclick=\"window.location='/.';\"></div>",
        "<div class=\"right_button\" onclick=\"window.location='/./root_level_img2.jpg.html';\"></div>",

        -- Preload of neighbor images
        "<link rel=\"preload\" href=\"/./root_level_img2.jpg.full\" as=\"image\"/>",

        -- Preload of neighbor pages
        "<link rel=\"preload\" href=\"/./root_level_img2.jpg.html\" as=\"image\"/>"
        ]

prop_imagePage_2Of2 = runTest $ do
    assertResponseContainsStrings "/root_level_img2.jpg.html" [
        -- The image itself
        "background-image",
        "/./root_level_img2.jpg.full",

        -- Navigation buttons
        "left_button",
        "'/./root_level_img1.jpg.html'",
        "top_button",
        "'/.'",
        "<div class=\"right_button\" ></div>",

        -- Preload of neighbor images
        "preload",
        "/./root_level_img1.jpg.full",
        "preload",
        "/./root_level_img1.jpg.html"
        ]

prop_imagePage_1Of1 = runTest $ do
    assertResponseContainsStrings "/level1_1/level11_img.jpg.html" [
        -- The image itself
        "background-image",
        "/level1_1/level11_img.jpg.full",

        -- Navigation buttons
        "<div class=\"left_button\" ></div>",
        "top_button",
        "'/level1_1'",
        "<div class=\"right_button\" ></div>"
        ]

prop_imagePage_specialChars = runTest $ do
    assertResponseContainsStrings "/level1_4utf8såäöあ/åäöあabc.jpg.html" [
        -- The image itself
        "background-image",
        "/level1_4utf8såäöあ/åäöあabc.jpg.full",

        -- Navigation buttons
        "<div class=\"left_button\" ></div>",
        "top_button",
        "'/level1_4utf8såäöあ'",
        "<div class=\"right_button\" ></div>"
        ]

prop_imagePage_1OfMany = runTest $ do
    assertResponseContainsStrings "/level1_2/level12_img1.jpg.html" [
        -- The image itself
        "background-image",
        "/level1_2/level12_img1.jpg.full",

        -- Navigation buttons
        "<div class=\"left_button\" ></div>",
        "top_button",
        "'/level1_2'",
        "right_button",
        "'/level1_2/level12_img2.jpg.html'",

        -- Preload of neighbor images
        "preload",
        -- Backward
        "/level1_2/level12_img1.jpg.full",
        "/level1_2/level12_img1.jpg.full",
        "/level1_2/level12_img1.jpg.full",
        "/level1_2/level12_img1.jpg.full",
        "/level1_2/level12_img1.jpg.full",
        -- Current
        "/level1_2/level12_img1.jpg.full",
        -- Forward
        "/level1_2/level12_img2.jpg.full",
        "/level1_2/level12_img3.jpg.full",
        "/level1_2/level12_img4.jpg.full",
        "/level1_2/level12_img5.jpg.full",
        "/level1_2/level12_img6.jpg.full",
        -- Backward
        "/level1_2/level12_img1.jpg.html",
        "/level1_2/level12_img1.jpg.html",
        "/level1_2/level12_img1.jpg.html",
        "/level1_2/level12_img1.jpg.html",
        "/level1_2/level12_img1.jpg.html",
        -- Current
        "/level1_2/level12_img1.jpg.html",
        -- Forward
        "/level1_2/level12_img2.jpg.html",
        "/level1_2/level12_img3.jpg.html",
        "/level1_2/level12_img4.jpg.html",
        "/level1_2/level12_img5.jpg.html",
        "/level1_2/level12_img6.jpg.html"
        ]

prop_imagePage_2OfMany = runTest $ do
    assertResponseContainsStrings "/level1_2/level12_img2.jpg.html" [
        -- The image itself
        "background-image",
        "/level1_2/level12_img2.jpg.full",

        -- Navigation buttons
        "left_button",
        "'/level1_2/level12_img1.jpg.html'",
        "top_button",
        "'/level1_2'",
        "right_button",
        "'/level1_2/level12_img3.jpg.html'",

        -- Preload of neighbor images
        "preload",
        -- Backward
        "/level1_2/level12_img1.jpg.full",
        "/level1_2/level12_img1.jpg.full",
        "/level1_2/level12_img1.jpg.full",
        "/level1_2/level12_img1.jpg.full",
        "/level1_2/level12_img1.jpg.full",
        -- Current
        "/level1_2/level12_img2.jpg.full",
        -- Forward
        "/level1_2/level12_img3.jpg.full",
        "/level1_2/level12_img4.jpg.full",
        "/level1_2/level12_img5.jpg.full",
        "/level1_2/level12_img6.jpg.full",
        "/level1_2/level12_img7.jpg.full",
        -- Backward
        "/level1_2/level12_img1.jpg.html",
        "/level1_2/level12_img1.jpg.html",
        "/level1_2/level12_img1.jpg.html",
        "/level1_2/level12_img1.jpg.html",
        "/level1_2/level12_img1.jpg.html",
        -- Current
        "/level1_2/level12_img2.jpg.html",
        -- Forward
        "/level1_2/level12_img3.jpg.html",
        "/level1_2/level12_img4.jpg.html",
        "/level1_2/level12_img5.jpg.html",
        "/level1_2/level12_img6.jpg.html",
        "/level1_2/level12_img7.jpg.html"
        ]

prop_imagePage_middleOfMany = runTest $ do
    assertResponseContainsStrings "/level1_2/level12_img7.jpg.html" [
        -- The image itself
        "background-image",
        "/level1_2/level12_img7.jpg.full",

        -- Navigation buttons
        "left_button",
        "'/level1_2/level12_img6.jpg.html'",
        "top_button",
        "'/level1_2'",
        "right_button",
        "'/level1_2/level12_img8.jpg.html'",

        -- Preload of neighbor images
        "preload",
        -- Backward
        "/level1_2/level12_img2.jpg.full",
        "/level1_2/level12_img3.jpg.full",
        "/level1_2/level12_img4.jpg.full",
        "/level1_2/level12_img5.jpg.full",
        "/level1_2/level12_img6.jpg.full",
        -- Current
        "/level1_2/level12_img7.jpg.full",
        -- Forward
        "/level1_2/level12_img8.jpg.full",
        "/level1_2/level12_img9.jpg.full",
        "/level1_2/level12_imgA.jpg.full",
        "/level1_2/level12_imgB.jpg.full",
        "/level1_2/level12_imgC.jpg.full",
        -- Backward
        "/level1_2/level12_img2.jpg.html",
        "/level1_2/level12_img3.jpg.html",
        "/level1_2/level12_img4.jpg.html",
        "/level1_2/level12_img5.jpg.html",
        "/level1_2/level12_img6.jpg.html",
        -- Current
        "/level1_2/level12_img7.jpg.html",
        -- Forward
        "/level1_2/level12_img8.jpg.html",
        "/level1_2/level12_img9.jpg.html",
        "/level1_2/level12_imgA.jpg.html",
        "/level1_2/level12_imgB.jpg.html",
        "/level1_2/level12_imgC.jpg.html"
        ]

prop_imagePage_lastOfMany = runTest $ do
    assertResponseContainsStrings "/level1_2/level12_imgD.jpg.html" [
        -- The image itself
        "background-image",
        "/level1_2/level12_imgD.jpg.full",

        -- Navigation buttons
        "left_button",
        "'/level1_2/level12_imgC.jpg.html'",
        "top_button",
        "'/level1_2'",
        "<div class=\"right_button\" ></div>",

        -- Preload of neighbor images
        "preload",
        -- Backward
        "/level1_2/level12_img8.jpg.full",
        "/level1_2/level12_img9.jpg.full",
        "/level1_2/level12_imgA.jpg.full",
        "/level1_2/level12_imgB.jpg.full",
        "/level1_2/level12_imgC.jpg.full",
        -- Current
        "/level1_2/level12_imgD.jpg.full",
        -- Forward
        "/level1_2/level12_imgD.jpg.full",
        "/level1_2/level12_imgD.jpg.full",
        "/level1_2/level12_imgD.jpg.full",
        "/level1_2/level12_imgD.jpg.full",
        "/level1_2/level12_imgD.jpg.full",
        -- Backward
        "/level1_2/level12_img8.jpg.html",
        "/level1_2/level12_img9.jpg.html",
        "/level1_2/level12_imgA.jpg.html",
        "/level1_2/level12_imgB.jpg.html",
        "/level1_2/level12_imgC.jpg.html",
        -- Current
        "/level1_2/level12_imgD.jpg.html",
        -- Forward
        "/level1_2/level12_imgD.jpg.html",
        "/level1_2/level12_imgD.jpg.html",
        "/level1_2/level12_imgD.jpg.html",
        "/level1_2/level12_imgD.jpg.html",
        "/level1_2/level12_imgD.jpg.html"
        ]

prop_imagePage_pathDoesNotExist = runTest $ do
    assertError "/doesNotExist.jpg.html"

prop_thumbImage_rootLevel = runTest $ do
    assertResponseContainsStrings "/root_level_img1.jpg.thumb" [
        "^content_of_root_level_img1_thumb$"
        ]
    assertResponseContainsStrings "/root_level_img2.jpg.thumb" [
        "^content_of_root_level_img2_thumb$"
        ]

prop_thumbImage_level1 = runTest $ do
    assertResponseContainsStrings "/level1_1/level11_img.jpg.thumb" [
        "^content_of_level11_img_thumb$"
        ]

prop_thumbImage_level2 = runTest $ do
    assertResponseContainsStrings "/level1_1/level2_2/level2_img.jpg.thumb" [
        "^random_stuff_that_for_sure_is_unique$"
        ]

prop_thumbImage_level3 = runTest $ do
    assertResponseContainsStrings "/level1_1/level2_2/level3/just_a_name.jpg.thumb" [
        "^content_of_level3_thumb_img$"
        ]

prop_thumbImage_level4 = runTest $ do
    assertResponseContainsStrings "/level1_1/level2_2/level3/level4/just_a_name.jpg.thumb" [
        "^content_of_level4_thumb_img$"
        ]

prop_thumbImage_doesNotExist = runTest $ do
    assertError "/doesNotExist.jpg.thumb"

prop_thumbImage_specialChars = runTest $ do
    assertResponseContainsStrings "/level1_4utf8såäöあ/åäöあabc.jpg.thumb" [
        "^thumb_åäöあabc_thumb$"
        ]

prop_fullImage_rootLevel = runTest $ do
    assertResponseContainsStrings "/root_level_img1.jpg.full" [
        "^content_of_root_level_img1_full$"
        ]
    assertResponseContainsStrings "/root_level_img2.jpg.full" [
        "^content_of_root_level_img2_full$"
        ]

prop_fullImage_level1 = runTest $ do
    assertResponseContainsStrings "/level1_1/level11_img.jpg.full" [
        "^content_of_level11_img_full$"
        ]

prop_fullImage_level2 = runTest $ do
    assertResponseContainsStrings "/level1_1/level2_2/level2_img.jpg.full" [
        "^full size version of the thumbnail content$"
        ]

prop_fullImage_level3 = runTest $ do
    assertResponseContainsStrings "/level1_1/level2_2/level3/just_a_name.jpg.full" [
        "^content_of_level3_full_img$"
        ]

prop_fullImage_level4 = runTest $ do
    assertResponseContainsStrings "/level1_1/level2_2/level3/level4/just_a_name.jpg.full" [
        "^content_of_level4_full_img$"
        ]

prop_fullImage_doesNotExist = runTest $ do
    assertError "/doesNotExist.jpg.full"

prop_fullImage_specialChars = runTest $ do
    assertResponseContainsStrings "/level1_4utf8såäöあ/åäöあabc.jpg.full" [
        "^full_åäöあabc_full$"
        ]

prop_incorrectExtension = runTest $ do
    assertError "/something.incorrectExtension"

prop_noExtension = runTest $ do
    assertError "/something.noExtension"

prop_nonImage = runTest $ do
    -- Folder page
    responseFolderPage <- request ""
    assert $ responseFolderPage =~ makeRegex "top_button"
    assert $ not $ responseFolderPage =~ makeRegex "other_file"

    -- ImagePage
    responseImagePage <- request "/root_level_img1.jpg.html"
    assert $ responseImagePage =~ makeRegex "background-image"
    assert $ not $ responseImagePage =~ makeRegex "other_file"

    -- Thumbnail
    responseThumbExtension <- request "/other_file.txt.thumb"
    assertContainsStrings responseThumbExtension ["^other_file_content$"]
    responseThumbNoExtension <- request "/other_file_no_extension.thumb"
    assertContainsStrings responseThumbNoExtension ["^other_file_no_extension_content$"]

    -- Full image
    responseFullExtension <- request "/other_file.txt.full"
    assertContainsStrings responseFullExtension ["^other_file_content$"]
    responseFullNoExtension <- request "/other_file_no_extension.full"
    assertContainsStrings responseFullNoExtension ["^other_file_no_extension_content$"]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
    
runTest :: IO () -> Property
runTest testFunc = once $ monadicIO $ run $ do
    withSystemTempDirectory "normalCases" (\thumbDir -> do
        withSystemTempDirectory "normalCases" (\fullImageDir -> do
            createFoldersAndFiles thumbDir fullImageDir
            let serverFunc = mainWithArgs thumbDir fullImageDir 12345
            race_ testFunc serverFunc))

createFoldersAndFiles :: FilePath -> FilePath -> IO ()
createFoldersAndFiles thumbDir fullImageDir = do
    -- Create directories and images
    forM_ [thumbDir, fullImageDir]
          (\dir -> do
                       createDirectory $ dir </> "level1_1"
                       createDirectory $ dir </> "level1_1"
                                             </> "level2_1"
                       createDirectory $ dir </> "level1_1"
                                             </> "level2_2"
                       createDirectory $ dir </> "level1_1"
                                             </> "level2_2"
                                             </> "level3"
                       createDirectory $ dir </> "level1_1"
                                             </> "level2_2"
                                             </> "level3"
                                             </> "level4"
                       createDirectory $ dir </> "level1_2"
                       createDirectory $ dir </> "level1_3"
                       createDirectory $ dir </> "level1_4utf8såäöあ"
          )

    createDirectory $ thumbDir </> "onlyInThumbs"
    createDirectory $ fullImageDir </> "onlyInFull"

    -- Images in root level
    writeFile (thumbDir </> "root_level_img1.jpg")
              "content_of_root_level_img1_thumb"
    writeFile (thumbDir </> "root_level_img2.jpg")
              "content_of_root_level_img2_thumb"
    writeFile (thumbDir </> "other_file.txt")
              "other_file_content"
    writeFile (thumbDir </> "other_file_no_extension")
              "other_file_no_extension_content"

    writeFile (fullImageDir </> "root_level_img1.jpg")
              "content_of_root_level_img1_full"
    writeFile (fullImageDir </> "root_level_img2.jpg")
              "content_of_root_level_img2_full"
    writeFile (fullImageDir </> "other_file.txt")
              "other_file_content"
    writeFile (fullImageDir </> "other_file_no_extension")
              "other_file_no_extension_content"

    -- Images in level 1_1
    writeFile (thumbDir </> "level1_1"
                        </> "level11_img.jpg")
              "content_of_level11_img_thumb"
    writeFile (fullImageDir </> "level1_1"
                            </> "level11_img.jpg")
              "content_of_level11_img_full"

    -- Images in level 1_4
    writeFile (thumbDir </> "level1_4utf8såäöあ"
                        </> "åäöあabc.jpg")
              "thumb_åäöあabc_thumb"
    writeFile (fullImageDir </> "level1_4utf8såäöあ"
                            </> "åäöあabc.jpg")
              "full_åäöあabc_full"

    -- Images in level 2
    writeFile (thumbDir </> "level1_1"
                        </> "level2_2"
                        </> "level2_img.jpg")
              "random_stuff_that_for_sure_is_unique"
    writeFile (fullImageDir </> "level1_1"
                            </> "level2_2"
                            </> "level2_img.jpg")
              "full size version of the thumbnail content"

    -- Images in level 3
    writeFile (thumbDir </> "level1_1"
                        </> "level2_2"
                        </> "level3"
                        </> "just_a_name.jpg")
              "content_of_level3_thumb_img"
    writeFile (fullImageDir </> "level1_1"
                            </> "level2_2"
                            </> "level3"
                            </> "just_a_name.jpg")
              "content_of_level3_full_img"

    -- Images in level 4
    writeFile (thumbDir </> "level1_1"
                        </> "level2_2"
                        </> "level3"
                        </> "level4"
                        </> "just_a_name.jpg")
              "content_of_level4_thumb_img"
    writeFile (fullImageDir </> "level1_1"
                            </> "level2_2"
                            </> "level3"
                            </> "level4"
                            </> "just_a_name.jpg")
              "content_of_level4_full_img"

    writeFile (thumbDir </> "level1_2" </> "level12_img1.jpg") ""
    writeFile (thumbDir </> "level1_2" </> "level12_img2.jpg") ""
    writeFile (thumbDir </> "level1_2" </> "level12_img3.jpg") ""
    writeFile (thumbDir </> "level1_2" </> "level12_img4.jpg") ""
    writeFile (thumbDir </> "level1_2" </> "level12_img5.jpg") ""
    writeFile (thumbDir </> "level1_2" </> "level12_img6.jpg") ""
    writeFile (thumbDir </> "level1_2" </> "level12_img7.jpg") ""
    writeFile (thumbDir </> "level1_2" </> "level12_img8.jpg") ""
    writeFile (thumbDir </> "level1_2" </> "level12_img9.jpg") ""
    writeFile (thumbDir </> "level1_2" </> "level12_imgA.jpg") ""
    writeFile (thumbDir </> "level1_2" </> "level12_imgB.jpg") ""
    writeFile (thumbDir </> "level1_2" </> "level12_imgC.jpg") ""
    writeFile (thumbDir </> "level1_2" </> "level12_imgD.jpg") ""

assertResponseContainsStrings :: String -> [Text] -> IO ()
assertResponseContainsStrings path needles = do
    response <- request path
    -- print response
    assertContainsStrings response needles

request :: String -> IO Text
request path = do
    -- Sleep to make sure the sever has started. Just 100 microseconds
    threadDelay 100
    request <- parseRequest $ "http://127.0.0.1:12345" ++ path
    response <- httpBS request
    return $ decodeUtf8 $ getResponseBody response

assertContainsStrings :: Text -> [Text] -> IO ()
assertContainsStrings haystack needles = do
    -- TODO: regex-quote each needle
    -- First check each individual string for easier debugging
    forM_ needles (\needle -> if haystack =~ makeRegex needle
                      then return ()
                      else do
                          throwIO (AssertionFailed (unpack needle)))

    if haystack =~ (makeRegexMultiline $ intercalate ".*" needles)
       then return ()
       else throwIO (AssertionFailed "All-at-once regex not found")

makeRegex :: Text -> Regex
makeRegex needle = makeRegexOpts needle []

makeRegexMultiline :: Text -> Regex
makeRegexMultiline needle = makeRegexOpts needle [multiline, dotall]

makeRegexOpts :: Text -> [PCREOption] -> Regex
makeRegexOpts needle options = regex
    where
        regex = fromRight (error "Failed to compile regex")
                          (compileM (encodeUtf8 needle) options)

assertError :: String -> IO ()
assertError path = do
    assertResponseContainsStrings path [
        "^Something went wrong$"
       ]

    -- To see that the server still works afterwards
    assertResponseContainsStrings "" [
        "<div class=\"top_button\" height=\"30px\" >\n\
        \/\n\
        \\n\
        \</div>"
       ]

assert :: Bool -> IO ()
assert True = return ()
assert False = throwIO (AssertionFailed "assert function")


return []
runTests = $quickCheckAll
