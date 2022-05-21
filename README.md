
# HTTPImageServer

Smartphones like Android and iPhone allow you to scroll and browse through your photo collection blazingly fast. But what if you don't have all your photos on your smartphone? Perhaps you store your family photos - taken over tens of years - in a more traditional way, i.e. as files and folders? This means you have full control over them, and can backup them in your own way, etc. But it makes it difficult to view them conveniently. This is where `HTTPImageServer` comes in.

`HTTPImageServer` is a webserver that displays photos stored in the local file system as a webpage. It's designed to mimic a typical smartphone's photos app, i.e. be fast and have the possibility to show photos as a grid. Below are some examples.

TODO: Add images

Because `HTTPImageServer` uses HTML, it can be used for both Android and iPhone, and even computers too. The user interface is pretty rudimentary, but all the basic functionality is there.

`HTTPImageServer` can be used with the original photos directly, but to make it fast, the photos need to be compressed. To achieve that, my other project [ImageMapper](https://github.com/osklunds/ImageMapper) can be used to compress the photos.

I actually use `HTTPImageServer` for real world use. I use `ImageMapper` to periodically map my (original) family photos to two parallell folder hierachies: thumbnail-sized and mobile-sized. Then `HTTPImageServer` uses the thumbnail-sized photos for the grid view and the mobile-sized photos for the fullscreen view. In this way I can view all my family photos from my smartphone just as if they were directly on my smartphone. Thanks to VPN I can access `HTTPImageServer` away from home too.

## Using the web page

## Starting the server

## TODOs

- Keyboard navigation
- Swipe navigation
- Calculate position when going to parent rather than saving
- Flicker-free switching (by using javascript or AJAX?)
