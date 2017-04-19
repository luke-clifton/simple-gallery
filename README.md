# Simple Gallery

Does pretty much what it says on the tin.

It looks in the current working directory for image files, creates thumbnails
for them and generates a HTML page that contains these thumbnails and links
to the original image.

## Motivation

I use [syncthing](https://syncthing.net/) to sync my camera role to my server.
I also serve the camera role with nginx, but would like to be able to get an
overview of all my images, including a small preview. I run this utility on
any filesystem events in that directory to update the gallery.
