
module RequestHandler.Tests where

import RequestHandler.Internal

testRequestTypeJpgImage = Image == requestType "/etc/folder/image.jpg"

testRequestTypePngImage = Image == requestType "/etc/folder/image.png"

testRequestTypeImagePage = ImagePage == requestType "/etc/folder/image.jpg.html"

testRequestTypeFolderPage = FolderPage == requestType "/etc/folder.html"